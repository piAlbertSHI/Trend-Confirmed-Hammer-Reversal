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
fixed <- fixed_setup()
bars <- fixed$bars
ml <- build_ml_table(market$full)
ml$data <- assign_ml_split(ml$data)
attr(ml$data, "ml_feature_names") <- ml$feature_names
daily <- build_daily_economic_table(ml$data)

# The entry event is the close that determines the next-open TCHR position.
base_state <- fixed_positions(bars, fixed$primary, followup_rule())[["TCHR"]]
entry_rows <- which(base_state & !c(FALSE, head(base_state, -1L)))
signal_rows <- entry_rows - 1L
signal_dates <- bars$Date[signal_rows]
daily_rows <- match(signal_dates, daily$Date)
daily_rows <- daily_rows[is.finite(daily_rows)]

feature_names <- ml_feature_columns(daily)
event <- daily[daily_rows, , drop = FALSE]
event <- event[, c("Date", feature_names,
                   "net_return_5d", "expected_market_log_5d", "excess_return_5d",
                   "adverse_5d", "utility_5d",
                   "net_return_10d", "expected_market_log_10d", "excess_return_10d",
                   "adverse_10d", "utility_10d",
                   "net_return_20d", "expected_market_log_20d", "excess_return_20d",
                   "adverse_20d", "utility_20d"), drop = FALSE]
event$event_id <- seq_len(nrow(event))
event$row_index <- daily_rows
event$split <- daily$split[daily_rows]
event$original_tchr_setup <- TRUE
event$original_tchr_flag <- 1
event <- event[, c("event_id", "row_index", "Date", "split", "original_tchr_setup",
                   "original_tchr_flag", setdiff(names(event), c("event_id", "row_index", "Date", "split", "original_tchr_setup", "original_tchr_flag"))), drop = FALSE]
attr(event, "ml_feature_names") <- feature_names

folds <- purged_event_walk_forward_folds(
  event, min_train = 20L, validation_size = 8L, test_size = 8L,
  step = 8L, train_embargo_sessions = 10L, test_gap_sessions = 10L
)
out <- run_daily_transfer_walk_forward(
  daily = daily,
  event = event,
  folds = folds,
  horizon = "5d",
  feature_mode = "correlation_pruned",
  evaluation_subset = "all",
  weight_half_life = 1000
)

dir.create("ml/results/tchr_integration", showWarnings = FALSE, recursive = TRUE)
write.csv(event, "ml/results/tchr_integration/tchr_entry_events.csv", row.names = FALSE)
write.csv(out$fold_metrics, "ml/results/tchr_integration/fold_metrics.csv", row.names = FALSE)
write.csv(out$predictions, "ml/results/tchr_integration/predictions.csv", row.names = FALSE)
write.csv(out$selections, "ml/results/tchr_integration/selections.csv", row.names = FALSE)
write.csv(out$summary, "ml/results/tchr_integration/summary.csv", row.names = FALSE)
saveRDS(list(event = event, folds = folds, output = out),
        "ml/results/tchr_integration/integration_models.rds", version = 3L)
print(out$fold_metrics)
print(out$summary)
