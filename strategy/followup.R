earlier_file <- "strategy/data/HSI_pre2000_chart.json"
earlier_sha <- "8a472039a2df264ab818090563bbc422d9824b0f302fcc76d75fe7b87ae562c5"

followup_rule <- function() {
  rules <- pullback_grid()
  rules[rules$id == "P06", ]
}

early_market <- function(archive) {
  stopifnot(digest::digest(file = earlier_file, algo = "sha256") == earlier_sha)
  chart <- jsonlite::fromJSON(earlier_file, simplifyVector = FALSE)$chart
  stopifnot(is.null(chart$error), length(chart$result) == 1L)
  value <- chart$result[[1L]]
  stopifnot(value$meta$symbol == "^HSI", value$meta$exchangeTimezoneName == "Asia/Hong_Kong")
  date <- as.Date(as.POSIXct(unlist(value$timestamp), origin = "1970-01-01", tz = "UTC"),
                  tz = value$meta$exchangeTimezoneName)
  quote <- value$indicators$quote[[1L]]
  number <- function(v) vapply(v, function(x) if (is.null(x)) NA_real_ else as.double(x), 0.0)
  m <- cbind(Open = number(quote$open), High = number(quote$high),
             Low = number(quote$low), Close = number(quote$close))
  observed <- rowSums(!is.na(m))
  stopifnot(all(observed %in% c(0L, 4L)), !anyNA(date), all(diff(date) > 0))
  reliable <- date >= as.Date("1990-01-01") & observed == 4L
  early <- data.frame(Date = date[reliable], m[reliable, , drop = FALSE])
  validate_bars(early)
  first <- min(archive$Date)
  overlap <- match(early$Date, archive$Date)
  both <- !is.na(overlap)
  delta <- vapply(ohlc_fields, function(name) {
    max(abs(early[[name]][both] - archive[[name]][overlap[both]]))
  }, 0.0)
  stopifnot(sum(both) > 30L, all(delta < .01))
  before <- early[early$Date < first, ]
  stopifnot(nrow(before) > warmup)
  audit <- data.frame(first = min(before$Date), last = max(before$Date),
                      sessions_before_warmup = nrow(before), warmup = warmup,
                      backtest_start = before$Date[warmup + 1L], count_overlaps = sum(both),
                      response_rows = length(date), empty_price_rows = sum(observed == 0L),
                      earliest_not_used_rows = sum(date < as.Date("1990-01-01") & observed == 4L),
                      open_delta = delta[["Open"]], high_delta = delta[["High"]],
                      low_delta = delta[["Low"]], close_delta = delta[["Close"]])
  list(prices = before, audit = audit)
}

followup_study <- function(x) {
  early <- early_market(x$market$archive)
  d <- early$prices
  q <- followup_rule()
  w <- pullback_matrix(d, q)[, 1L]
  r <- return_components(d$Open, d$Close)
  ids <- seq.int(warmup + 1L, nrow(d))
  match_dates <- seq_len(nrow(d)) %in% ids
  span <- data.frame(period = "Earlier historical sample", start = min(d$Date[match_dates]),
                     end = max(d$Date[match_dates]))
  model <- list("P06" = w, "Buy and hold" = rep(TRUE, nrow(d)))
  performance <- score_universe(d, model, span, "^HSI")
  a <- portfolio_log(r, w, ids, trial_fee)$daily
  b <- portfolio_log(r, rep(TRUE, nrow(d)), ids, trial_fee)$daily
  closed <- block_inference(cbind("P06 net" = a, "P06 vs index" = a - b))

  market <- x$market$full
  id <- period_rows(market, x$spans[x$spans$period == "Out of sample", ])
  universe <- pullback_matrix(market, pullback_grid())
  ret <- return_components(market$Open, market$Close)
  adjusted <- family_inference(ret, universe, id, fee = trial_fee)
  d_oos <- comparison_daily(market, list("P06" = universe[, q$id], "Buy and hold" = rep(TRUE, nrow(market))),
                             x$spans[x$spans$period == "Out of sample", ])
  etf <- x$fund$prices
  f_oos <- comparison_daily(etf, list("P06" = universe[x$fund$index_rows, q$id], "Buy and hold" = rep(TRUE, nrow(etf))),
                             x$spans[x$spans$period == "Out of sample", ])
  list(rule = q, audit = early$audit, early = performance, early_intervals = closed, adjusted = adjusted,
       early_daily = comparison_daily(d, model, span), oos_daily = d_oos, fund_daily = f_oos)
}

followup_signature <- function(pullback) {
  files <- c("strategy/FOLLOW_UP.md", "strategy/followup.R", earlier_file)
  c(pullback$signatures, setNames(vapply(files, function(file) digest::digest(file = file, algo = "sha256"), ""), files))
}

save_followup <- function(x, p) {
  y <- followup_study(x)
  y$signatures <- followup_signature(p)
  output_tables(list(early_hsi_audit = y$audit, early_hsi_performance = y$early,
                     early_hsi_intervals = y$early_intervals, early_hsi_daily = y$early_daily,
                     p06_hsi_oos_daily = y$oos_daily, p06_trahk_oos_daily = y$fund_daily,
                     pullback_oos_max_t = y$adjusted))
  saveRDS(y, "strategy/results/followup.rds", version = 3L)
  y
}

read_followup <- function(p) {
  y <- readRDS("strategy/results/followup.rds")
  stopifnot(identical(y$signatures, followup_signature(p)))
  y
}
