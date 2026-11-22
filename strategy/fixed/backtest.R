fixed_start <- as.Date("2001-03-01")
fixed_end <- as.Date("2025-05-30")

fixed_setup <- function() {
  market <- load_market()
  bars <- market$full
  rows <- which(bars$Date >= fixed_start & bars$Date <= fixed_end)
  stopifnot(bars$Date[min(rows)] == fixed_start, bars$Date[max(rows)] == fixed_end,
            min(rows) == warmup + 1L, length(rows) == 5973L)
  bars <- bars[seq_len(max(rows)), , drop = FALSE]
  conf <- configuration_grid()
  primary <- conf[conf$id == "R16", ]
  stopifnot(nrow(primary) == 1L, primary$family == "TCHR", primary$slow == 100L,
            primary$buffer == .01, primary$hold == 10L)
  span <- data.frame(period = "Fixed historical sample", start = fixed_start, end = fixed_end)
  list(bars = bars, rows = rows, span = span, rules = conf, primary = primary,
       pullbacks = pullback_grid())
}

fixed_positions <- function(bars, primary, p06) {
  spec <- position_spec(bars, primary)
  list("TCHR" = next_open_positions(close_targets(spec, primary)),
       "Same trend only" = next_open_positions(close_targets(spec, primary, "core")),
       "Same confirmation without shape" = next_open_positions(close_targets(spec, primary, "no_shape")),
       "Bollinger reentry" = next_open_positions(pullback_target(pullback_spec(bars), p06)),
       "Original intraday" = next_open_positions(seq_len(nrow(bars)) %in% signal_index(bars, features(bars))),
       "Buy and hold" = rep(TRUE, nrow(bars)))
}

fixed_family_test <- function(ret, state, rows, selected_id) {
  benchmark <- portfolio_log(ret, rep(TRUE, nrow(state)), rows, trial_fee)$daily
  spread <- vapply(seq_len(ncol(state)), function(i) {
    portfolio_log(ret, state[, i], rows, trial_fee)$daily - benchmark
  }, numeric(length(rows)))
  do.call(rbind, lapply(strategy_blocks, function(k) {
    mean_day <- colMeans(spread)
    trials <- circular_totals(spread, k, strategy_boot, 880230L + k) / nrow(spread)
    se <- apply(trials, 2L, sd)
    stopifnot(all(is.finite(se)), all(se > 0))
    null <- sweep(sweep(trials, 2L, mean_day, "-"), 2L, se, "/")
    maxima <- apply(null, 1L, max)
    z <- mean_day / se
    i <- match(selected_id, colnames(state))
    best <- which.max(z)
    data.frame(block = k, reps = strategy_boot, candidates = length(z), primary = selected_id,
               primary_t = z[i], primary_p = (1 + sum(null[, i] >= z[i])) / (strategy_boot + 1L),
               primary_adjusted_p = (1 + sum(maxima >= z[i])) / (strategy_boot + 1L),
               best_by_t = colnames(state)[best], family_p = (1 + sum(maxima >= z[best])) /
                 (strategy_boot + 1L))
  }))
}

sample_comparison <- function(bars, rows, state, fee) {
  r <- return_components(bars$Open, bars$Close)
  runs <- lapply(names(state), function(id) {
    if (id == "Original intraday") intraday_reference(r, state[[id]], rows, fee) else
      portfolio_log(r, state[[id]], rows, fee)
  })
  setNames(runs, names(state))
}

fixed_research <- function() {
  x <- fixed_setup()
  bars <- x$bars
  rows <- x$rows
  r <- return_components(bars$Open, bars$Close)
  state <- fixed_positions(bars, x$primary, followup_rule())
  scores <- score_universe(bars, state, x$span, "^HSI")
  z <- sample_comparison(bars, rows, state, trial_fee)
  q <- z[["TCHR"]]
  b <- z[["Buy and hold"]]
  old <- z[["Original intraday"]]
  cash_blend <- log1p(mean(q$weight) * expm1(b$daily + b$cost_log))
  series <- cbind("Net" = q$daily, "Active" = q$daily - b$daily,
                  "Exposure-adjusted" = q$daily - cash_blend,
                  "Confirmation increment" = q$daily - z[["Same trend only"]]$daily,
                  "Shape increment" = q$daily - z[["Same confirmation without shape"]]$daily)
  d <- comparison_daily(bars, state, x$span)
  d$overnight_log <- b$gross_gap
  d$intraday_log <- b$gross_intraday
  d$matched_log <- cash_blend
  d$strategy_intraday_log <- q$gross_intraday
  d$strategy_gap_log <- q$gross_gap
  d$strategy_cost_log <- q$cost_log
  fills <- position_trades(q, bars$Date[rows], bars$Open[rows], bars$Close[rows], trial_fee)
  stopifnot(abs(sum(q$daily) - sum(log1p(fills$net))) < 1e-11)
  z1 <- position_matrix(bars, x$rules)
  z2 <- pullback_matrix(bars, x$pullbacks)
  family <- fixed_family_test(r, cbind(z1, z2), rows, x$primary$id)
  universe <- c(setNames(lapply(colnames(z1), function(id) z1[, id]), colnames(z1)),
                setNames(lapply(colnames(z2), function(id) z2[, id]), colnames(z2)))
  all_rules <- score_universe(bars, universe, x$span, "^HSI", fees = trial_fee)
  audit <- data.frame(first = min(bars$Date[rows]), last = max(bars$Date[rows]), sessions = length(rows),
                      warmup = warmup, old_trades = sum(old$buys), old_exposure = mean(old$weight),
                      old_bp_gross = 10000 * sum(old$gross_intraday) / sum(old$buys),
                      old_net = expm1(sum(old$daily)), buyhold_overnight_log = sum(b$gross_gap),
                      buyhold_intraday_log = sum(b$gross_intraday), matched_compound = expm1(sum(cash_blend)))
  list(setup = x, performance = scores, daily = d, trades = fills, diagnostic = audit,
       all_rules = all_rules, intervals = block_inference(series), max_t = family)
}
