#!/usr/bin/env Rscript
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  setwd(dirname(dirname(normalizePath(sub("^--file=", "", script_arg[1L])))))
}

packages <- c("TTR", "digest", "jsonlite", "glmnet", "xgboost")
missing <- packages[!vapply(packages, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing)) stop("Install packages: ", paste(missing, collapse = ", "))

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
source("strategy/ml_overlay.R", encoding = "UTF-8")

market <- load_market()
fixed <- fixed_setup()
ml <- build_ml_table(market$full)
ml$data <- assign_ml_split(ml$data)
attr(ml$data, "ml_feature_names") <- ml$feature_names
daily <- build_daily_economic_table(ml$data)
overlay <- run_tchr_ml_overlay(
  bars = fixed$bars,
  daily = daily,
  base_state = fixed_positions(fixed$bars, fixed$primary, followup_rule())[["TCHR"]],
  fixed_rows = fixed$rows,
  fee_bps = trial_fee,
  weighted_models_path = "ml/results/tchr_integration/final/frozen_overlay_bundle.rds",
  selection_path = "strategy/fixed/results/tchr_ml_overlay_thresholds.csv"
)

result_dir <- "strategy/fixed/results"
dir.create(result_dir, showWarnings = FALSE, recursive = TRUE)
write.csv(overlay$fold_metrics, file.path(result_dir, "tchr_ml_overlay_folds.csv"), row.names = FALSE)
write.csv(overlay$predictions, file.path(result_dir, "tchr_ml_overlay_predictions.csv"), row.names = FALSE)
write.csv(overlay$thresholds, file.path(result_dir, "tchr_ml_overlay_thresholds.csv"), row.names = FALSE)
write.csv(overlay$portfolio_summary, file.path(result_dir, "tchr_ml_overlay_portfolio.csv"), row.names = FALSE)
write.csv(overlay$trades, file.path(result_dir, "tchr_base_trades_for_overlay.csv"), row.names = FALSE)
write.csv(data.frame(
  date = fixed$bars$Date[fixed$rows],
  tchr_position = overlay$base_state[fixed$rows],
  ml_overlay_position = overlay$filtered_state[fixed$rows],
  stringsAsFactors = FALSE
), file.path(result_dir, "tchr_ml_overlay_daily.csv"), row.names = FALSE)
saveRDS(overlay, file.path(result_dir, "tchr_ml_overlay.rds"), version = 3L)
print(overlay$fold_metrics)
print(overlay$portfolio_summary)
