configuration_grid <- function() {
  avg <- c(100L, 150L, 200L)
  band <- c(0, .01)
  length_limit <- c(5L, 10L)
  cross <- function(type, ...) {
    x <- expand.grid(..., KEEP.OUT.ATTRS = FALSE)
    empty <- setdiff(c("slow", "buffer", "hold", "momentum", "lookback", "exit"), names(x))
    for (name in empty) x[[name]] <- 0
    cbind(family = type, x[c("slow", "buffer", "hold", "momentum", "lookback", "exit")])
  }
  channel <- data.frame(family = "Channel", slow = 0L, buffer = 0, hold = 0L, momentum = 0L,
                        lookback = c(55L, 100L, 200L), exit = c(20L, 40L, 50L))
  conf <- rbind(cross("Trend", slow = avg, buffer = band),
                cross("TCHR", slow = avg, buffer = band, hold = length_limit),
                cross("Confirmed hammer", slow = avg, hold = length_limit),
                cross("Dual trend", slow = avg, momentum = c(63L, 126L), buffer = .01),
                channel)
  cbind(id = sprintf("R%02d", seq_len(nrow(conf))), conf)
}

position_spec <- function(bars, rules) {
  close <- bars$Close
  windows <- sort(unique(rules$slow[rules$slow > 0L]))
  average <- setNames(lapply(windows, function(w) as.numeric(TTR::SMA(close, w))), windows)
  shapes <- features(bars)
  list(close = close, high = bars$High, low = bars$Low, avg = average,
       pullback = !is.na(shapes$mid) & close < shapes$mid,
       shape = hammer(shapes, 2, 1))
}

# Carry forward the result of the latest decisive close.
switch_state <- function(enter, leave) {
  enter <- !is.na(enter) & enter
  leave <- !is.na(leave) & leave
  decision <- cummax(ifelse(enter | leave, seq_along(enter), 0L))
  c(FALSE, enter)[decision + 1L]
}

trend_target <- function(close, slow, buffer) {
  switch_state(close > (1 + buffer) * slow, close < (1 - buffer) * slow)
}

confirmed_holding <- function(spec, slow, holding, use_shape = TRUE) {
  support <- !is.na(slow) & spec$close >= .96 * slow
  setup <- support & spec$pullback & (!use_shape | spec$shape)
  active <- confirmed <- logical(length(slow))
  until <- pending <- 0L
  line <- 0
  close <- spec$close
  for (i in seq_along(slow)) {
    if (i <= until && (!support[i] || close[i] <= line)) until <- 0L
    if (i > until) {
      if (pending > 0L && i <= pending + 3L && support[i] && close[i] > spec$high[pending]) {
        until <- i + holding - 1L
        line <- spec$low[pending]
        confirmed[i] <- TRUE
        pending <- 0L
      } else if (setup[i]) {
        pending <- i
      } else if (i > pending + 3L) {
        pending <- 0L
      }
    }
    active[i] <- i <= until
  }
  list(active = active, confirmed = confirmed, setup = setup)
}

close_targets <- function(spec, rule, ablation = c("full", "core", "no_shape")) {
  ablation <- match.arg(ablation)
  close <- spec$close
  family <- rule$family
  if (family == "Channel") {
    buy <- c(NA_real_, head(as.numeric(TTR::runMax(close, rule$lookback)), -1L))
    sell <- c(NA_real_, head(as.numeric(TTR::runMin(close, rule$exit)), -1L))
    return(switch_state(close > buy, close < sell))
  }

  slow <- spec$avg[[as.character(rule$slow)]]
  stopifnot(!is.null(slow))
  core <- trend_target(close, slow, rule$buffer)
  if (family == "Dual trend") {
    m <- rule$momentum
    positive <- c(rep(FALSE, m), tail(close, -m) > head(close, -m))
    return(core & positive)
  }
  if (family == "Trend" || ablation == "core") return(core)
  sleeve <- confirmed_holding(spec, slow, rule$hold, use_shape = ablation == "full")$active
  if (family == "Confirmed hammer") return(sleeve)
  core | sleeve
}

next_open_positions <- function(target) {
  c(FALSE, head(target, -1L))
}

return_components <- function(open, close) {
  stopifnot(length(open) == length(close), length(open) > 1L,
            all(is.finite(open)), all(is.finite(close)), all(open > 0), all(close > 0))
  list(intraday = log(close / open), gap = c(0, log(tail(open, -1L) / head(close, -1L))))
}

# At any evaluation boundary, start in cash at the first open and settle at the final close.
portfolio_log <- function(ret, exposure, rows, fee_bps) {
  stopifnot(length(rows) > 1L, all(diff(rows) == 1L), length(exposure) == length(ret$gap),
            fee_bps >= 0, fee_bps < 10000)
  w <- as.logical(exposure[rows])
  previous <- c(FALSE, head(w, -1L))
  buys <- w & !previous
  sells <- !w & previous
  exit_close <- tail(w, 1L)
  intra <- w * ret$intraday[rows]
  overnight <- previous * ret$gap[rows]
  fee <- fee_bps / 10000
  drag <- buys * log1p(fee) - sells * log1p(-fee)
  drag[length(drag)] <- drag[length(drag)] - exit_close * log1p(-fee)
  list(daily = intra + overnight - drag, gross_intraday = intra, gross_gap = overnight,
       cost_log = drag, buys = buys, sells = sells, exit_close = exit_close, weight = w)
}

intraday_reference <- function(ret, exposure, rows, fee_bps) {
  entered <- exposure[rows]
  c <- fee_bps / 10000
  d <- ret$intraday[rows] * entered
  cost <- entered * (log1p(c) - log1p(-c))
  list(daily = d - cost, gross_intraday = d, gross_gap = 0 * d, cost_log = cost,
       buys = entered, sells = entered, exit_close = FALSE, weight = entered)
}

position_trades <- function(run, date, open, close, fee_bps) {
  entries <- which(run$buys)
  exits <- c(which(run$sells), if (run$exit_close) length(run$weight))
  stopifnot(length(entries) == length(exits))
  open_based <- rep(TRUE, length(exits))
  if (run$exit_close) open_based[length(exits)] <- FALSE
  out <- ifelse(open_based, open[exits], close[exits])
  paid <- open[entries]
  fee <- fee_bps / 10000
  data.frame(entry_date = date[entries], exit_date = date[exits], entry = paid, exit = out,
             exit_at = ifelse(open_based, "open", "close"),
             sessions = exits - entries + as.integer(!open_based),
             net = out * (1 - fee) / (paid * (1 + fee)) - 1)
}

pass_objectives <- function(log_net, log_bench) log_net > 0 & log_net > log_bench
