#!/usr/bin/env Rscript

source("R/data.R", encoding = "UTF-8")
source("R/strategy.R", encoding = "UTF-8")
source("R/research.R", encoding = "UTF-8")
source("strategy/engine.R", encoding = "UTF-8")
source("strategy/evaluate.R", encoding = "UTF-8")
source("strategy/pullback.R", encoding = "UTF-8")
source("strategy/followup.R", encoding = "UTF-8")
source("strategy/fixed/backtest.R", encoding = "UTF-8")
source("ml/features.R", encoding = "UTF-8")
source("ml/splits.R", encoding = "UTF-8")
source("ml/preprocess.R", encoding = "UTF-8")
source("ml/metrics.R", encoding = "UTF-8")
source("ml/event_models.R", encoding = "UTF-8")
source("ml/event_regression.R", encoding = "UTF-8")
source("ml/candidate_events.R", encoding = "UTF-8")
source("ml/candidate_regression.R", encoding = "UTF-8")
source("ml/candidate_policy_nogate.R", encoding = "UTF-8")
source("ml/daily_transfer.R", encoding = "UTF-8")

market <- load_market()
fixed <- fixed_setup(); bars <- fixed$bars
ml <- build_ml_table(market$full)
ml$data <- assign_ml_split(ml$data)
attr(ml$data, "ml_feature_names") <- ml$feature_names
daily <- build_daily_economic_table(ml$data)
candidate <- build_candidate_event_table(ml$data)
event <- candidate$data
folds <- purged_event_walk_forward_folds(
  event, min_train = 30L, validation_size = 12L, test_size = 8L,
  step = 8L, train_embargo_sessions = 10L, test_gap_sessions = 10L
)
candidate_run <- run_daily_transfer_walk_forward(
  daily, event, folds, horizon = "5d", feature_mode = "correlation_pruned",
  evaluation_subset = "all", weight_half_life = 1000
)

base_state <- fixed_positions(bars, fixed$primary, followup_rule())[["TCHR"]]
entry_rows <- which(base_state & !c(FALSE, head(base_state, -1L)))
ret <- return_components(bars$Open, bars$Close)
base_run <- portfolio_log(ret, base_state, fixed$rows, trial_fee)
trades <- position_trades(base_run, bars$Date[fixed$rows], bars$Open[fixed$rows], bars$Close[fixed$rows], trial_fee)
entry_bar <- match(trades$entry_date, bars$Date)
exit_bar <- match(trades$exit_date, bars$Date)
signal_bar <- entry_bar - 1L
daily_rows <- match(bars$Date[signal_bar], daily$Date)
benchmark <- log(bars$Close[exit_bar] / bars$Close[signal_bar])
tchr <- data.frame(
  entry_id = seq_along(entry_bar), entry_date = trades$entry_date,
  exit_date = trades$exit_date, row_index = daily_rows,
  net_5d = daily$net_return_5d[daily_rows],
  excess_5d = daily$excess_return_5d[daily_rows],
  utility_5d = daily$utility_5d[daily_rows],
  trade_net = trades$net,
  trade_excess = expm1(log1p(trades$net) - benchmark),
  stringsAsFactors = FALSE
)

fold_metrics <- list(); predictions <- list()
for (i in seq_along(candidate_run$models)) {
  fold_id <- names(candidate_run$models)[i]
  fold <- folds[[which(vapply(folds, function(x) x$id == fold_id, logical(1L)))[1L]]]
  model <- candidate_run$models[[fold_id]]
  s <- candidate_run$selections[candidate_run$selections$fold == fold_id, , drop = FALSE]
  rows <- which(tchr$entry_date >= fold$test_start & tchr$entry_date <= fold$test_end)
  if (!length(rows) || !nrow(s)) next
  score <- model$fit$predict(tchr$row_index[rows])
  selected <- if (s$policy[1L] == "top_fraction") {
    select_event_top_fraction(score, s$policy_fraction[1L])$selected
  } else if (s$policy[1L] == "positive_score") score > 0 else rep(FALSE, length(rows))
  net <- tchr$net_5d[rows]; excess <- tchr$excess_5d[rows]
  utility <- tchr$utility_5d[rows]; trade <- tchr$trade_net[rows]
  fold_metrics[[length(fold_metrics) + 1L]] <- data.frame(
    fold = fold_id, test_start = fold$test_start, test_end = fold$test_end,
    events = length(rows), selected = sum(selected), selection_rate = mean(selected),
    uplift_5d_net_bp = (mean(expm1(net[selected])) - mean(expm1(net))) * 10000,
    uplift_5d_excess_bp = (mean(expm1(excess[selected])) - mean(expm1(excess))) * 10000,
    uplift_5d_utility_bp = (mean(expm1(utility[selected])) - mean(expm1(utility))) * 10000,
    uplift_trade_net_bp = (mean(trade[selected]) - mean(trade)) * 10000,
    stringsAsFactors = FALSE
  )
  predictions[[length(predictions) + 1L]] <- data.frame(
    fold = fold_id, entry_id = tchr$entry_id[rows], entry_date = tchr$entry_date[rows],
    score = score, selected = selected, stringsAsFactors = FALSE
  )
}
fold_metrics <- do.call(rbind, fold_metrics)
predictions <- do.call(rbind, predictions)
summary <- data.frame(
  folds = nrow(fold_metrics), events = sum(fold_metrics$events),
  mean_5d_net_bp = mean(fold_metrics$uplift_5d_net_bp),
  positive_5d_net_rate = mean(fold_metrics$uplift_5d_net_bp > 0),
  mean_5d_excess_bp = mean(fold_metrics$uplift_5d_excess_bp),
  positive_5d_excess_rate = mean(fold_metrics$uplift_5d_excess_bp > 0),
  mean_trade_net_bp = mean(fold_metrics$uplift_trade_net_bp),
  positive_trade_net_rate = mean(fold_metrics$uplift_trade_net_bp > 0),
  stringsAsFactors = FALSE
)

decision <- rep(NA, nrow(tchr)); decision[predictions$entry_id] <- predictions$selected
filtered_state <- base_state
for (i in which(!is.na(decision) & !decision)) {
  start <- entry_bar[i]; stop <- start
  while (stop <= length(filtered_state) && base_state[stop]) stop <- stop + 1L
  filtered_state[start:min(stop - 1L, length(filtered_state))] <- FALSE
}
transfer_start <- min(which(!is.na(decision)))
transfer_rows <- entry_bar[transfer_start]:max(fixed$rows)
base_transfer <- portfolio_log(ret, base_state, transfer_rows, trial_fee)
filtered_transfer <- portfolio_log(ret, filtered_state, transfer_rows, trial_fee)
portfolio_row <- function(label, run) {
  m <- risk_metrics(run$daily)
  data.frame(rule = label, compound = unname(m[["compound"]]),
             annualized = unname(m[["annualized"]]), annual_vol = unname(m[["annual_vol"]]),
             sharpe = unname(m[["sharpe"]]), max_drawdown = unname(m[["max_drawdown"]]),
             trades = sum(run$buys), exposure = mean(run$weight), stringsAsFactors = FALSE)
}
portfolio_summary <- rbind(portfolio_row("TCHR", base_transfer),
                           portfolio_row("TCHR + transferred ML", filtered_transfer))
dir.create("ml/results/tchr_integration/expanded", showWarnings = FALSE, recursive = TRUE)
write.csv(fold_metrics, "ml/results/tchr_integration/expanded/fold_metrics.csv", row.names = FALSE)
write.csv(predictions, "ml/results/tchr_integration/expanded/predictions.csv", row.names = FALSE)
write.csv(summary, "ml/results/tchr_integration/expanded/summary.csv", row.names = FALSE)
write.csv(portfolio_summary, "ml/results/tchr_integration/expanded/portfolio_summary.csv", row.names = FALSE)
saveRDS(list(event = event, folds = folds, candidate_run = candidate_run,
             tchr = tchr, predictions = predictions,
             portfolio_summary = portfolio_summary),
        "ml/results/tchr_integration/expanded/integration_models.rds", version = 3L)
print(fold_metrics); print(summary); print(portfolio_summary)
