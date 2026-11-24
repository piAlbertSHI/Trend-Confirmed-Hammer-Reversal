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
source("ml/candidate_regression.R", encoding = "UTF-8")
source("ml/candidate_events.R", encoding = "UTF-8")
source("ml/daily_transfer.R", encoding = "UTF-8")

market <- load_market()
fixed <- fixed_setup()
bars <- fixed$bars
ml <- build_ml_table(market$full)
ml$data <- assign_ml_split(ml$data)
attr(ml$data, "ml_feature_names") <- ml$feature_names
daily <- build_daily_economic_table(ml$data)
base_state <- fixed_positions(bars, fixed$primary, followup_rule())[["TCHR"]]
entry_rows <- which(base_state & !c(FALSE, head(base_state, -1L)))
signal_rows <- entry_rows - 1L
daily_rows <- match(bars$Date[signal_rows], daily$Date)
ret <- return_components(bars$Open, bars$Close)
base_run <- portfolio_log(ret, base_state, fixed$rows, trial_fee)
trades <- position_trades(base_run, bars$Date[fixed$rows], bars$Open[fixed$rows], bars$Close[fixed$rows], trial_fee)
trade_rows <- match(trades$entry_date, bars$Date) - 1L
daily_rows <- match(bars$Date[trade_rows], daily$Date)
exit_rows <- match(trades$exit_date, bars$Date)
entry_bar <- match(trades$entry_date, bars$Date)
exit_bar <- match(trades$exit_date, bars$Date)
adverse <- vapply(seq_along(entry_bar), function(i) {
  min(log(bars$Low[seq.int(entry_bar[i], exit_bar[i])] / bars$Open[entry_bar[i]]))
}, 0.0)
benchmark <- log(bars$Close[exit_bar] / bars$Close[trade_rows])

feature_names <- ml_feature_columns(daily)
event <- daily[daily_rows, c("Date", feature_names), drop = FALSE]
event$event_id <- seq_len(nrow(event))
event$row_index <- daily_rows
event$split <- daily$split[daily_rows]
event$original_tchr_setup <- TRUE
event$original_tchr_flag <- 1
event$trade_net_log <- log1p(trades$net)
event$trade_excess_log <- event$trade_net_log - benchmark
event$trade_adverse_log <- adverse
event$trade_utility_log <- event$trade_excess_log - 0.25 * abs(pmin(adverse, 0))
event$net_return_5d <- event$trade_net_log
event$excess_return_5d <- event$trade_excess_log
event$utility_5d <- event$trade_utility_log
event$adverse_5d <- event$trade_adverse_log
event <- event[, c("event_id", "row_index", "Date", "split", "original_tchr_setup",
                   "original_tchr_flag", setdiff(names(event), c("event_id", "row_index", "Date", "split", "original_tchr_setup", "original_tchr_flag"))), drop = FALSE]
attr(event, "ml_feature_names") <- feature_names

folds <- purged_event_walk_forward_folds(
  event, min_train = 20L, validation_size = 8L, test_size = 8L,
  step = 8L, train_embargo_sessions = 10L, test_gap_sessions = 10L
)

policy_select <- function(rows, scores) {
  utility <- expm1(event$trade_utility_log[rows])
  choices <- lapply(c(.25, .50, .75), function(fraction) {
    top <- select_event_top_fraction(scores, fraction)
    list(fraction = fraction, selected = top$selected,
         threshold = top$threshold,
         value = if (sum(top$selected) >= 3L) mean(utility[top$selected]) else -Inf)
  })
  choices[[which.max(vapply(choices, function(x) x$value, 0.0))]]
}

fold_metrics <- list(); predictions <- list(); selections <- list()
for (i in seq_along(folds)) {
  fold <- folds[[i]]
  features <- prune_event_features(event, fold$train, feature_names, correlation_threshold = .75)
  grid <- candidate_parameter_grid()
  choices <- lapply(names(grid), function(name) {
    fit <- fit_event_xgb_regression(
      event, fold$train, fold$validation, features, "trade_utility_log",
      nrounds = 300L, early_stopping_rounds = 40L,
      seed = 20250530L + i, model_params = grid[[name]]
    )
    score <- fit$predict(fold$validation)
    policy <- policy_select(fold$validation, score)
    list(name = name, fit = fit, policy = policy)
  })
  choice <- choices[[which.max(vapply(choices, function(x) x$policy$value, 0.0))]]
  test_score <- choice$fit$predict(fold$test)
  selected <- select_event_top_fraction(test_score, choice$policy$fraction)$selected
  net <- expm1(event$trade_net_log[fold$test])
  excess <- expm1(event$trade_excess_log[fold$test])
  utility <- expm1(event$trade_utility_log[fold$test])
  adverse <- expm1(event$trade_adverse_log[fold$test])
  fold_metrics[[i]] <- data.frame(
    fold = fold$id, events = length(fold$test), selected = sum(selected),
    selection_rate = mean(selected),
    uplift_mean_net_bp = (mean(net[selected]) - mean(net)) * 10000,
    uplift_mean_excess_bp = (mean(excess[selected]) - mean(excess)) * 10000,
    uplift_mean_utility_bp = (mean(utility[selected]) - mean(utility)) * 10000,
    adverse_change_bp = (mean(adverse[selected]) - mean(adverse)) * 10000,
    specification = choice$name, policy_fraction = choice$policy$fraction,
    validation_utility_bp = choice$policy$value * 10000,
    test_start = fold$test_start, test_end = fold$test_end,
    stringsAsFactors = FALSE
  )
  predictions[[i]] <- data.frame(
    fold = fold$id, event_id = event$event_id[fold$test], Date = event$Date[fold$test],
    score = test_score, selected = selected,
    net_return = event$trade_net_log[fold$test], excess_return = event$trade_excess_log[fold$test],
    utility = event$trade_utility_log[fold$test], adverse = event$trade_adverse_log[fold$test],
    stringsAsFactors = FALSE
  )
  selections[[i]] <- data.frame(fold = fold$id, specification = choice$name,
                                policy_fraction = choice$policy$fraction,
                                validation_utility_bp = choice$policy$value * 10000,
                                features = paste(features, collapse = "|"), stringsAsFactors = FALSE)
}
fold_metrics <- do.call(rbind, fold_metrics)
predictions <- do.call(rbind, predictions)
selections <- do.call(rbind, selections)
net <- fold_metrics$uplift_mean_net_bp
excess <- fold_metrics$uplift_mean_excess_bp
utility <- fold_metrics$uplift_mean_utility_bp
summary <- data.frame(
  folds = nrow(fold_metrics), test_events = sum(fold_metrics$events),
  mean_uplift_net_bp = mean(net), median_uplift_net_bp = median(net), positive_net_rate = mean(net > 0),
  mean_uplift_excess_bp = mean(excess), median_uplift_excess_bp = median(excess), positive_excess_rate = mean(excess > 0),
  mean_uplift_utility_bp = mean(utility), median_uplift_utility_bp = median(utility), positive_utility_rate = mean(utility > 0),
  mean_adverse_change_bp = mean(fold_metrics$adverse_change_bp), stringsAsFactors = FALSE
)
dir.create("ml/results/tchr_integration/trade_model", showWarnings = FALSE, recursive = TRUE)
write.csv(event, "ml/results/tchr_integration/trade_model/events.csv", row.names = FALSE)
write.csv(fold_metrics, "ml/results/tchr_integration/trade_model/fold_metrics.csv", row.names = FALSE)
write.csv(predictions, "ml/results/tchr_integration/trade_model/predictions.csv", row.names = FALSE)
write.csv(selections, "ml/results/tchr_integration/trade_model/selections.csv", row.names = FALSE)
write.csv(summary, "ml/results/tchr_integration/trade_model/summary.csv", row.names = FALSE)
print(fold_metrics)
print(summary)
