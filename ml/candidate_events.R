candidate_setup_mask <- function(data,
                                support_ratio = 0.94,
                                lower_ratio = 1.5,
                                upper_ratio = 1.5,
                                max_body_to_range = 0.65,
                                min_body_pct = 0.0005) {
  open <- as.numeric(data$Open)
  high <- as.numeric(data$High)
  low <- as.numeric(data$Low)
  close <- as.numeric(data$Close)
  range <- pmax(high - low, .Machine$double.eps)
  body <- abs(close - open)
  upper <- pmax(high - pmax(open, close), 0)
  lower <- pmax(pmin(open, close) - low, 0)
  sma20 <- as.numeric(TTR::SMA(close, n = 20L))
  sma100 <- as.numeric(TTR::SMA(close, n = 100L))
  is.finite(close) &
    is.finite(sma20) &
    is.finite(sma100) &
    close < sma20 &
    close >= support_ratio * sma100 &
    body >= min_body_pct * close &
    lower > lower_ratio * body &
    upper < upper_ratio * body &
    body / range <= max_body_to_range
}

independent_event_rows <- function(rows, min_gap = 5L) {
  rows <- sort(unique(as.integer(rows)))
  if (!length(rows)) return(integer())
  keep <- logical(length(rows))
  last <- -Inf
  for (i in seq_along(rows)) {
    if (rows[i] - last >= min_gap) {
      keep[i] <- TRUE
      last <- rows[i]
    }
  }
  rows[keep]
}

trailing_mean_past <- function(x, window = 126L) {
  out <- rep(NA_real_, length(x))
  if (length(x) <= window) return(out)
  for (i in seq.int(window + 1L, length(x))) {
    previous <- x[(i - window):(i - 1L)]
    if (all(is.finite(previous))) out[i] <- mean(previous)
  }
  out
}

expected_market_log_return <- function(data, horizon, window = 126L) {
  n <- nrow(data)
  raw <- rep(NA_real_, n)
  valid <- seq_len(n - horizon)
  raw[valid] <- log(data$Close[valid + horizon] / data$Open[valid + 1L])
  trailing_mean_past(raw, window = window)
}

candidate_target_columns <- function(data, rows, horizon, fee_bps = ml_fee_bps) {
  entry <- rows + 1L
  exit <- rows + horizon
  fee <- fee_bps / 10000
  fee_log <- log1p(-fee) - log1p(fee)
  net <- log(data$Close[exit] / data$Open[entry]) + fee_log
  benchmark <- log(data$Close[exit] / data$Close[rows])
  expected_market <- expected_market_log_return(data, horizon)
  excess <- net - expected_market[rows]
  adverse <- vapply(seq_along(rows), function(i) {
    min(log(data$Low[seq.int(entry[i], exit[i])] / data$Open[entry[i]]))
  }, 0.0)
  utility <- excess - 0.25 * abs(pmin(adverse, 0))
  list(
    net = net,
    benchmark = benchmark,
    expected_market = expected_market[rows],
    excess = excess,
    adverse = adverse,
    utility = utility
  )
}

build_candidate_event_table <- function(data,
                                        min_gap = 5L,
                                        support_ratio = 0.94,
                                        lower_ratio = 1.5,
                                        upper_ratio = 1.5,
                                        max_body_to_range = 0.65,
                                        min_body_pct = 0.0005) {
  stopifnot(inherits(data$Date, "Date"))
  relaxed_mask <- candidate_setup_mask(
    data,
    support_ratio = support_ratio,
    lower_ratio = lower_ratio,
    upper_ratio = upper_ratio,
    max_body_to_range = max_body_to_range,
    min_body_pct = min_body_pct
  )
  expected_5 <- expected_market_log_return(data, 5L)
  expected_10 <- expected_market_log_return(data, 10L)
  expected_20 <- expected_market_log_return(data, 20L)
  eligible <- data$warmup_complete &
    seq_len(nrow(data)) + 20L <= nrow(data) &
    is.finite(expected_5) &
    is.finite(expected_10) &
    is.finite(expected_20)
  original_rows <- independent_event_rows(
    which(eligible & data$tchr_setup),
    min_gap = min_gap
  )
  relaxed_rows <- which(eligible & relaxed_mask & !data$tchr_setup)
  candidate_rows <- relaxed_rows[
    !vapply(relaxed_rows, function(row) {
      any(abs(row - original_rows) < min_gap)
    }, logical(1L))
  ]
  candidate_rows <- independent_event_rows(candidate_rows, min_gap = min_gap)
  rows <- sort(c(original_rows, candidate_rows))
  if (length(rows) < 30L) stop("Too few candidate events for research.")

  feature_names <- ml_feature_columns(data)
  targets <- lapply(c(5L, 10L, 20L), function(horizon) {
    candidate_target_columns(data, rows, horizon)
  })
  names(targets) <- c("5d", "10d", "20d")
  event <- data.frame(
    event_id = seq_along(rows),
    row_index = rows,
    Date = data$Date[rows],
    split = data$split[rows],
    original_tchr_setup = as.logical(data$tchr_setup[rows]),
    original_tchr_flag = as.numeric(data$tchr_setup[rows]),
    data[rows, feature_names, drop = FALSE],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (horizon in names(targets)) {
    target <- targets[[horizon]]
    event[[paste0("net_return_", horizon)]] <- target$net
    event[[paste0("benchmark_log_", horizon)]] <- target$benchmark
    event[[paste0("expected_market_log_", horizon)]] <- target$expected_market
    event[[paste0("excess_return_", horizon)]] <- target$excess
    event[[paste0("adverse_", horizon)]] <- target$adverse
    event[[paste0("utility_", horizon)]] <- target$utility
    event[[paste0("label_excess_", horizon)]] <- as.integer(target$excess > 0)
  }
  event$candidate_only <- !event$original_tchr_setup
  candidate_feature_names <- c(feature_names, "original_tchr_flag")
  attr(event, "ml_feature_names") <- candidate_feature_names
  list(
    data = event,
    candidate_rows = rows,
    feature_names = candidate_feature_names,
    parameters = list(
      min_gap = min_gap,
      support_ratio = support_ratio,
      lower_ratio = lower_ratio,
      upper_ratio = upper_ratio,
      max_body_to_range = max_body_to_range,
      min_body_pct = min_body_pct
    )
  )
}

candidate_event_diagnostics <- function(candidate) {
  event <- candidate$data
  horizons <- c("5d", "10d", "20d")
  rows <- lapply(horizons, function(horizon) {
    net <- event[[paste0("net_return_", horizon)]]
    excess <- event[[paste0("excess_return_", horizon)]]
    utility <- event[[paste0("utility_", horizon)]]
    data.frame(
      horizon = horizon,
      events = nrow(event),
      original_tchr_events = sum(event$original_tchr_setup),
      candidate_only_events = sum(event$candidate_only),
      positive_excess = sum(excess > 0),
      positive_excess_rate = mean(excess > 0),
      mean_net_bp = mean(expm1(net)) * 10000,
      median_net_bp = median(expm1(net)) * 10000,
      mean_excess_bp = mean(expm1(excess)) * 10000,
      median_excess_bp = median(expm1(excess)) * 10000,
      mean_adverse_bp = mean(expm1(utility - excess)) * 10000,
      mean_utility_bp = mean(expm1(utility)) * 10000,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}






