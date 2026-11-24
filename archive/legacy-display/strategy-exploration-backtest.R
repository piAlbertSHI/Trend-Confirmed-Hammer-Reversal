backtest_signature <- function() {
  files <- c("strategy/PROTOCOL.md", "strategy/engine.R", "strategy/inputs.R",
             "strategy/evaluate.R", "strategy/backtest.R", "R/data.R", "R/strategy.R", "R/research.R",
             unname(market_files), fund_file)
  setNames(vapply(files, function(file) digest::digest(file = file, algo = "sha256"), ""), files)
}

period_comparisons <- function(bars, states, spans, instrument) {
  ret <- return_components(bars$Open, bars$Close)
  groups <- spans[spans$period %in% c("Out of sample", "Historical holdout", "Post-archive"), ]
  do.call(rbind, lapply(seq_len(nrow(groups)), function(i) {
    p <- groups[i, ]
    rows <- period_rows(bars, p)
    selected <- portfolio_log(ret, states[["Selected"]], rows, trial_fee)
    buy_hold <- portfolio_log(ret, states[["Buy and hold"]], rows, trial_fee)
    combined <- portfolio_log(ret, states[["Trend + hammer best"]], rows, trial_fee)
    core <- portfolio_log(ret, states[["Same trend without overlay"]], rows, trial_fee)
    no_shape <- portfolio_log(ret, states[["Same overlay without hammer"]], rows, trial_fee)
    matched <- log1p(mean(selected$weight) * expm1(buy_hold$daily + buy_hold$cost_log))
    s <- cbind("Selected net" = selected$daily,
               "Selected vs index or fund" = selected$daily - buy_hold$daily,
               "Selected vs equal exposure" = selected$daily - matched,
               "Hammer overlay vs trend" = combined$daily - core$daily,
               "Hammer shape vs no shape" = combined$daily - no_shape$daily)
    cbind(instrument, period = p$period, block_inference(s))
  }))
}

run_strategy_backtest <- function() {
  market <- load_market()
  bars <- market$full
  ref <- read_fund()
  fund <- align_fund(bars, ref)
  spans <- study_spans(bars, max(market$archive$Date))
  rules <- configuration_grid()
  stopifnot(nrow(rules) == 33L)
  ret <- return_components(bars$Open, bars$Close)
  positions <- position_matrix(bars, rules)
  development <- period_rows(bars, spans[spans$period == "Development", ])
  ends <- as.Date(c("2005-12-31", "2010-12-31"))
  training <- training_table(ret, positions, rules, development, training_blocks(development, ends, bars$Date))
  selected <- frozen_choices(training)
  main <- selected[1L, ]
  combined <- selected[selected$role == "Trend + hammer best", ]
  spec <- position_spec(bars, rules)
  models <- setNames(lapply(selected$id, function(i) positions[, i]), selected$role)
  comparisons <- c(models, list(
    "Same trend without overlay" = next_open_positions(close_targets(spec, combined, "core")),
    "Same overlay without hammer" = next_open_positions(close_targets(spec, combined, "no_shape")),
    "Original intraday" = next_open_positions(seq_len(nrow(bars)) %in% signal_index(bars, features(bars))),
    "Buy and hold" = rep(TRUE, nrow(bars))
  ))
  fund_models <- lapply(comparisons, "[", fund$index_rows)
  index_score <- score_universe(bars, comparisons, spans, "^HSI")
  fund_score <- score_universe(fund$prices, fund_models, spans, "2800.HK")
  scores <- rbind(index_score, fund_score)
  statistics <- rbind(period_comparisons(bars, comparisons, spans, "^HSI"),
                      period_comparisons(fund$prices, fund_models, spans, "2800.HK"))
  walk <- annual_selection(bars, rules, positions, spans)
  diagnostics <- baseline_diagnosis(market)
  oos <- spans[spans$period == "Out of sample", ]
  rows <- period_rows(bars, oos)
  trades <- position_trades(portfolio_log(ret, comparisons[["Selected"]], rows, trial_fee),
                            bars$Date[rows], bars$Open[rows], bars$Close[rows], trial_fee)
  stopifnot(abs(sum(log1p(trades$net)) - sum(portfolio_log(ret, comparisons[["Selected"]], rows, trial_fee)$daily)) < 1e-10,
            identical(unname(comparisons[["Selected"]]), unname(positions[, main$id])))
  list(signatures = backtest_signature(),
       market = market, fund = fund, spans = spans, primary = main, combined = combined,
       configurations = rules, training = training, selected = selected, scores = scores,
       diagnostics = diagnostics, oos_positions = comparisons, statistics = statistics,
       data_oos = comparison_daily(bars, comparisons, oos),
       fund_oos = comparison_daily(fund$prices, fund_models, oos),
       trades = trades, walk = walk, family = family_inference(ret, positions, development),
       session = capture.output(sessionInfo()))
}

output_tables <- function(items) {
  dir.create("strategy/results", showWarnings = FALSE)
  for (name in names(items)) {
    write.csv(items[[name]], file.path("strategy/results", paste0(name, ".csv")),
              row.names = FALSE, fileEncoding = "UTF-8", na = "")
  }
}

save_backtest <- function(x) {
  output_tables(list(v1_diagnosis = x$diagnostics, period_definitions = x$spans,
                     candidate_ranking = x$training, frozen_decisions = x$selected,
                     performance = x$scores, bootstrap_intervals = x$statistics,
                     selection_adjustment = x$family, hsi_oos_daily = x$data_oos,
                     trahk_oos_daily = x$fund_oos, trahk_audit = x$fund$audit,
                     trahk_missing_dates = x$fund$missing, selected_trades_oos = x$trades,
                     walk_forward_years = x$walk$annual, walk_forward_daily = x$walk$daily,
                     walk_forward_summary = x$walk$summary))
  writeLines(x$session, "strategy/results/session-info.txt", useBytes = TRUE)
  saveRDS(x, "strategy/results/backtest.rds", version = 3L)
  invisible(x)
}

read_backtest <- function() {
  x <- readRDS("strategy/results/backtest.rds")
  stopifnot(identical(x$signatures, backtest_signature()))
  x
}

backtest_brief <- function(x) {
  scores <- x$scores
  d <- scores[scores$fee_bps == trial_fee & scores$rule %in% c("Selected", "Trend + hammer best") &
                scores$period %in% c("Complete", "Validation", "Historical holdout", "Post-archive", "Out of sample"), ]
  list(primary = x$primary[c("id", "family", "slow", "buffer", "hold", "momentum", "lookback", "exit")],
       confirmed_strategy = x$combined[c("id", "slow", "buffer", "hold")],
       results = d[c("instrument", "rule", "period", "trades", "net", "benchmark_net", "drawdown", "passes_both")],
       walk_forward = x$walk$summary[c("net", "benchmark_net", "passes_both")],
       family_p = x$family$adjusted_p)
}
