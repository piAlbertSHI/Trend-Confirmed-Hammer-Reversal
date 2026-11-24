ml_fixed_start <- as.Date("2001-03-01")
ml_fixed_end <- as.Date("2025-05-30")
ml_warmup <- 220L
ml_fee_bps <- 5

lag_value <- function(x, n = 1L) {
  n <- as.integer(n)
  if (n <= 0L) return(x)
  if (n >= length(x)) return(rep(NA_real_, length(x)))
  c(rep(NA_real_, n), head(x, -n))
}

safe_ratio <- function(a, b) {
  out <- rep(NA_real_, length(a))
  ok <- is.finite(a) & is.finite(b) & b != 0
  out[ok] <- a[ok] / b[ok] - 1
  out
}

rolling_slope <- function(x, window = 5L) {
  level <- as.numeric(TTR::SMA(x, n = window))
  safe_ratio(level, lag_value(level, window))
}

rolling_zscore <- function(x, window = 20L) {
  avg <- as.numeric(TTR::SMA(x, n = window))
  sdev <- as.numeric(TTR::runSD(x, n = window, sample = FALSE))
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x) & is.finite(avg) & is.finite(sdev) & sdev > 0
  out[ok] <- (x[ok] - avg[ok]) / sdev[ok]
  out
}

validate_ml_bars <- function(bars) {
  required <- c("Date", "Open", "High", "Low", "Close")
  missing <- setdiff(required, names(bars))
  if (length(missing)) stop("Missing market fields: ", paste(missing, collapse = ", "))
  validate_bars(bars)
  if ("Volume" %in% names(bars)) {
    if (any(!is.finite(bars$Volume)) || any(bars$Volume < 0)) {
      stop("Volume contains invalid values.")
    }
  }
  invisible(bars)
}

build_ml_table <- function(bars,
                           start = ml_fixed_start,
                           end = ml_fixed_end,
                           warmup = ml_warmup,
                           fee_bps = ml_fee_bps) {
  validate_ml_bars(bars)
  if (start < min(bars$Date) || end > max(bars$Date) || start >= end) {
    stop("The requested ML window is outside the available market data.")
  }

  bars <- bars[bars$Date >= start & bars$Date <= end, , drop = FALSE]
  rownames(bars) <- NULL
  if (nrow(bars) < warmup + 20L) stop("The ML window is too short for the requested warm-up.")

  open <- as.numeric(bars$Open)
  high <- as.numeric(bars$High)
  low <- as.numeric(bars$Low)
  close <- as.numeric(bars$Close)
  volume <- if ("Volume" %in% names(bars)) as.numeric(bars$Volume) else rep(NA_real_, length(close))
  candle_range <- pmax(high - low, .Machine$double.eps)
  body <- abs(close - open)
  upper_shadow <- pmax(high - pmax(open, close), 0)
  lower_shadow <- pmax(pmin(open, close) - low, 0)
  daily_log <- log(close / lag_value(close))
  gap_log <- log(open / lag_value(close))
  intraday_log <- log(close / open)

  sma5 <- as.numeric(TTR::SMA(close, 5L))
  sma20 <- as.numeric(TTR::SMA(close, 20L))
  sma50 <- as.numeric(TTR::SMA(close, 50L))
  sma100 <- as.numeric(TTR::SMA(close, 100L))
  sma200 <- as.numeric(TTR::SMA(close, 200L))
  bb_mid <- as.numeric(TTR::SMA(close, 20L))
  bb_sd <- as.numeric(TTR::runSD(close, 20L, sample = FALSE))
  atr20 <- as.numeric(TTR::ATR(cbind(high, low, close), n = 20L)[, "atr"])
  rsi2 <- as.numeric(TTR::RSI(close, n = 2L))
  rsi14 <- as.numeric(TTR::RSI(close, n = 14L))
  volume_log <- log1p(volume)

  raw_features <- features(bars, window = 20L)
  tchr_setup <- close < bb_mid &
    close >= 0.96 * sma100 &
    hammer(raw_features, 2, 1)

  feature_data <- data.frame(
    Date = bars$Date,
    Open = open,
    High = high,
    Low = low,
    Close = close,
    Volume = volume,
    daily_log_return = daily_log,
    gap_log_return = gap_log,
    intraday_log_return = intraday_log,
    candle_range_pct = candle_range / close,
    body_pct = body / close,
    body_to_range = body / candle_range,
    upper_shadow_to_body = pmin(upper_shadow / pmax(body, 0.1 * candle_range), 20),
    lower_shadow_to_body = pmin(lower_shadow / pmax(body, 0.1 * candle_range), 20),
    close_location = (close - low) / candle_range,
    close_to_sma5 = safe_ratio(close, sma5),
    close_to_sma20 = safe_ratio(close, sma20),
    close_to_sma50 = safe_ratio(close, sma50),
    close_to_sma100 = safe_ratio(close, sma100),
    close_to_sma200 = safe_ratio(close, sma200),
    sma5_slope5 = rolling_slope(close, 5L),
    sma20_slope10 = rolling_slope(close, 10L),
    sma50_slope20 = rolling_slope(close, 20L),
    sma100_slope20 = rolling_slope(sma100, 20L),
    bollinger_z = (close - bb_mid) / pmax(2 * bb_sd, .Machine$double.eps),
    bollinger_position = (close - (bb_mid - 2 * bb_sd)) /
      pmax(4 * bb_sd, .Machine$double.eps),
    atr20_pct = atr20 / close,
    volatility5 = as.numeric(TTR::runSD(daily_log, 5L, sample = FALSE)),
    volatility20 = as.numeric(TTR::runSD(daily_log, 20L, sample = FALSE)),
    volatility60 = as.numeric(TTR::runSD(daily_log, 60L, sample = FALSE)),
    rsi2 = rsi2,
    rsi14 = rsi14,
    return_3d = log(close / lag_value(close, 3L)),
    return_5d = log(close / lag_value(close, 5L)),
    return_10d = log(close / lag_value(close, 10L)),
    return_20d = log(close / lag_value(close, 20L)),
    drawdown20 = close / pmax(TTR::runMax(close, 20L), .Machine$double.eps) - 1,
    drawdown100 = close / pmax(TTR::runMax(close, 100L), .Machine$double.eps) - 1,
    volume_log = volume_log,
    volume_change = safe_ratio(volume_log, lag_value(volume_log)),
    volume_z20 = rolling_zscore(volume_log, 20L),
    tchr_setup_flag = as.numeric(tchr_setup),
    tchr_setup = as.logical(tchr_setup),
    stringsAsFactors = FALSE
  )

  feature_names <- setdiff(
    names(feature_data),
    c("Date", "Open", "High", "Low", "Close", "Volume", "tchr_setup")
  )

  fee <- fee_bps / 10000
  fee_log <- log1p(-fee) - log1p(fee)
  n <- nrow(feature_data)
  target_from_next_open <- function(horizon) {
    entry <- c((2L):n, rep(NA_integer_, 1L))
    exit <- seq_len(n) + horizon
    out <- rep(NA_real_, n)
    ok <- !is.na(entry) & exit <= n
    out[ok] <- log(close[exit[ok]] / open[entry[ok]]) + fee_log
    out
  }
  target_excess <- function(horizon, net_target) {
    exit <- seq_len(n) + horizon
    out <- rep(NA_real_, n)
    ok <- exit <= n & is.finite(net_target)
    out[ok] <- net_target[ok] - log(close[exit[ok]] / close[seq_len(n)][ok])
    out
  }

  feature_data$target_net_5d <- target_from_next_open(5L)
  feature_data$target_net_10d <- target_from_next_open(10L)
  feature_data$target_excess_5d <- target_excess(5L, feature_data$target_net_5d)
  feature_data$target_positive_5d <- ifelse(
    is.finite(feature_data$target_net_5d),
    feature_data$target_net_5d > 0,
    NA
  )
  feature_data$target_positive_excess_5d <- ifelse(
    is.finite(feature_data$target_excess_5d),
    feature_data$target_excess_5d > 0,
    NA
  )
  feature_data$warmup_complete <- seq_len(n) > warmup

  attr(feature_data, "ml_feature_names") <- feature_names

  numeric_features <- as.matrix(feature_data[feature_names])
  finite_features <- apply(numeric_features, 1L, function(x) all(is.finite(x)))
  quality <- data.frame(
    check = c(
      "window_first", "window_last", "sessions", "duplicate_dates",
      "invalid_ohlc", "zero_volume_sessions", "feature_count",
      "warmup_sessions", "finite_feature_rows", "finite_feature_cells",
      "max_abs_feature", "extreme_feature_cells", "tchr_setup_rows",
      "valid_5d_labels", "positive_5d_rate", "valid_10d_labels"
    ),
    value = c(
      as.character(min(feature_data$Date)),
      as.character(max(feature_data$Date)),
      nrow(feature_data),
      sum(duplicated(feature_data$Date)),
      sum(!(feature_data$Low <= pmin(feature_data$Open, feature_data$Close) &
              feature_data$High >= pmax(feature_data$Open, feature_data$Close))),
      sum(feature_data$Volume == 0, na.rm = TRUE),
      length(feature_names),
      sum(!feature_data$warmup_complete),
      sum(finite_features),
      sum(is.finite(numeric_features)),
      signif(max(abs(numeric_features), na.rm = TRUE), 8),
      sum(abs(numeric_features) > 1000, na.rm = TRUE),
      sum(feature_data$tchr_setup, na.rm = TRUE),
      sum(is.finite(feature_data$target_net_5d)),
      mean(feature_data$target_positive_5d, na.rm = TRUE),
      sum(is.finite(feature_data$target_net_10d))
    ),
    stringsAsFactors = FALSE
  )

  list(
    data = feature_data,
    feature_names = feature_names,
    quality = quality,
    window = c(start = start, end = end),
    fee_bps = fee_bps
  )
}

feature_dictionary <- function(data, feature_names = attr(data, "ml_feature_names")) {
  stopifnot(length(feature_names) > 0L)
  data.frame(
    feature = feature_names,
    type = vapply(data[feature_names], function(x) if (is.numeric(x)) "numeric" else class(x)[1L], ""),
    description = c(
      "Close-to-close log return.",
      "Open-to-prior-close overnight log return.",
      "Open-to-close intraday log return.",
      "Daily high-low range as a fraction of close.",
      "Absolute candle body as a fraction of close.",
      "Candle body divided by total range.",
      "Upper shadow divided by body.",
      "Lower shadow divided by body.",
      "Close location within the daily range.",
      "Close relative to the five-session SMA.",
      "Close relative to the twenty-session SMA.",
      "Close relative to the fifty-session SMA.",
      "Close relative to the one-hundred-session SMA.",
      "Close relative to the two-hundred-session SMA.",
      "Five-session SMA slope over five sessions.",
      "Twenty-session SMA slope over ten sessions.",
      "Fifty-session SMA slope over twenty sessions.",
      "One-hundred-session SMA slope over twenty sessions.",
      "Bollinger-style standardized close distance.",
      "Close position within the twenty-session volatility band.",
      "Twenty-session ATR as a fraction of close.",
      "Five-session return volatility.",
      "Twenty-session return volatility.",
      "Sixty-session return volatility.",
      "Two-session RSI.",
      "Fourteen-session RSI.",
      "Three-session log return.",
      "Five-session log return.",
      "Ten-session log return.",
      "Twenty-session log return.",
      "Distance from the rolling twenty-session high.",
      "Distance from the rolling one-hundred-session high.",
      "Log-transformed volume.",
      "Change in log-transformed volume.",
      "Twenty-session standardized volume.",
      "TCHR setup indicator available at the signal close."
    )[seq_along(feature_names)],
    stringsAsFactors = FALSE
  )
}





