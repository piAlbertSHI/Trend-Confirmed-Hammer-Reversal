features <- function(bars, window = 20L) {
  stopifnot(nrow(bars) >= window, window >= 2L)
  mid <- as.numeric(TTR::SMA(bars$Close, n = window))
  dispersion <- as.numeric(TTR::runSD(bars$Close, n = window, sample = FALSE))
  body <- abs(bars$Close - bars$Open)
  upper_shadow <- bars$High - pmax(bars$Open, bars$Close)
  lower_shadow <- pmin(bars$Open, bars$Close) - bars$Low
  data.frame(mid, lower = mid - 2 * dispersion, upper = mid + 2 * dispersion,
             body, upper_shadow, lower_shadow)
}

hammer <- function(f, a, b) {
  f$body > 0 & f$lower_shadow > a * f$body & f$upper_shadow < b * f$body
}

signal_index <- function(bars, f, a = 2, b = 1, band = "mid", shape = TRUE) {
  stopifnot(a > 0, b > 0, band %in% c("mid", "lower", "none"))
  below <- if (band == "none") !is.na(f$mid) else bars$Close < f[[band]]
  which(below & (!shape | hammer(f, a, b)))
}

# Archived sensitivity used the completed bar to enter at that day's open.
# It has a separate entry point and cannot reach the execution engine with lag 0.
archived_grid <- function(bars, parameters, f = features(bars)) {
  delta <- bars$Close - bars$Open
  rows <- lapply(seq_len(nrow(parameters)), function(i) {
    j <- signal_index(bars, f, parameters$a[i], parameters$b[i])
    data.frame(trades = length(j), points = sum(delta[j]), win = mean(delta[j] > 0),
               points_per_trade = mean(delta[j]))
  })
  cbind(parameters, do.call(rbind, rows))
}

trade_log <- function(bars, signals, lag = 1L, hold = 1L, fee_bps = 0, first = 1L,
                      last = nrow(bars)) {
  stopifnot(is.finite(lag), lag >= 1, lag == as.integer(lag),
            is.finite(hold), hold >= 1, hold == as.integer(hold),
            length(fee_bps) == 1L, is.finite(fee_bps), fee_bps >= 0, fee_bps < 10000,
            first >= 1L, first <= last, last <= nrow(bars),
            all(is.finite(signals)), all(signals == as.integer(signals)),
            all(signals >= 1L & signals <= nrow(bars)),
            !is.unsorted(signals, strictly = TRUE))
  entry <- signals + lag
  closeout <- entry + hold - 1L
  valid <- which(entry >= first & closeout <= last)
  if (hold > 1L) {
    keep <- logical(length(valid))
    busy_through <- 0L
    for (i in seq_along(valid)) {
      j <- valid[i]
      if (entry[j] > busy_through) {
        keep[i] <- TRUE
        busy_through <- closeout[j]
      }
    }
    valid <- valid[keep]
  }
  entry <- entry[valid]
  closeout <- closeout[valid]
  signal <- signals[valid]
  open <- bars$Open[entry]
  close <- bars$Close[closeout]
  fee <- fee_bps / 10000
  gross_log <- log(close / open)
  fee_log <- log1p(-fee) - log1p(fee)
  net_log <- gross_log + fee_log
  trades <- data.frame(signal, entry, exit = closeout, signal_date = bars$Date[signal],
                       entry_date = bars$Date[entry], exit_date = bars$Date[closeout],
                       open, close, points = close - open, gross_log, net_log,
                       gross = expm1(gross_log), net = expm1(net_log))
  gross_daily <- numeric(nrow(bars))
  gross_daily[entry] <- log(bars$Close[entry] / open)
  if (hold > 1L) {
    close_log <- c(0, diff(log(bars$Close)))
    retained <- unlist(lapply(entry, function(j) seq.int(j + 1L, length.out = hold - 1L)))
    gross_daily[retained] <- close_log[retained]
  }
  net_daily <- gross_daily
  net_daily[entry] <- net_daily[entry] - log1p(fee)
  net_daily[closeout] <- net_daily[closeout] + log1p(-fee)
  list(trades = trades, gross_daily = gross_daily, net_daily = net_daily)
}

risk_metrics <- function(daily_log) {
  wealth <- exp(c(0, cumsum(daily_log)))
  daily <- expm1(daily_log)
  s <- sd(daily)
  c(compound = tail(wealth, 1L) - 1,
    annualized = expm1(252 * mean(daily_log)),
    annual_vol = s * sqrt(252),
    sharpe = if (is.finite(s) && s > 0) sqrt(252) * mean(daily) / s else NA_real_,
    max_drawdown = min(wealth / cummax(wealth) - 1))
}

trade_metrics <- function(run, rows) {
  x <- run$trades
  n <- nrow(x)
  trading_days <- if (n) sum(x$exit - x$entry + 1L) else 0L
  avg <- function(z) if (length(z)) mean(z) else NA_real_
  raw <- risk_metrics(run$gross_daily[rows])
  net <- risk_metrics(run$net_daily[rows])
  data.frame(sessions = length(rows), trades = n, exposure = trading_days / length(rows),
             points = sum(x$points), rounded_points = sum(round(x$points, 2)),
             gross_win = avg(x$gross > 0), net_win = avg(x$net > 0),
             gross_mean_bp = avg(x$gross) * 10000, net_mean_bp = avg(x$net) * 10000,
             gross_median_bp = if (n) median(x$gross) * 10000 else NA_real_,
             gross_compound = raw[["compound"]], net_compound = net[["compound"]],
             net_annualized = net[["annualized"]], net_annual_vol = net[["annual_vol"]],
             net_sharpe = net[["sharpe"]], net_drawdown = net[["max_drawdown"]])
}

legacy_fields <- function(bars) {
  replace(bars, c("Open", "Close"), list(bars$Close, bars$Open))
}
