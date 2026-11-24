#!/usr/bin/env Rscript
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  setwd(dirname(dirname(normalizePath(sub("^--file=", "", script_arg[1L])))))
}

args <- commandArgs(trailingOnly = TRUE)
paths_arg <- grep("^--paths=", args, value = TRUE)
n_paths <- 500L
if (length(paths_arg)) n_paths <- as.integer(sub("^--paths=", "", paths_arg[1L]))
if (!is.finite(n_paths) || n_paths < 1L) stop("--paths must be a positive integer")

packages <- c("TTR", "Matrix", "digest", "jsonlite", "rugarch", "glmnet", "xgboost")
missing <- packages[!vapply(packages, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing)) stop("Install packages: ", paste(missing, collapse = ", "))

source("R/data.R", encoding = "UTF-8")
source("R/strategy.R", encoding = "UTF-8")
source("R/research.R", encoding = "UTF-8")
source("R/legacy.R", encoding = "UTF-8")
source("R/null_models.R", encoding = "UTF-8")
source("strategy/engine.R", encoding = "UTF-8")
source("strategy/evaluate.R", encoding = "UTF-8")
source("strategy/pullback.R", encoding = "UTF-8")
source("strategy/followup.R", encoding = "UTF-8")
source("strategy/fixed/backtest.R", encoding = "UTF-8")
source("strategy/fixed/randomness.R", encoding = "UTF-8")
source("strategy/randomness.R", encoding = "UTF-8")
source("strategy/ml_overlay.R", encoding = "UTF-8")
source("ml/features.R", encoding = "UTF-8")
source("ml/splits.R", encoding = "UTF-8")
source("ml/preprocess.R", encoding = "UTF-8")
source("ml/metrics.R", encoding = "UTF-8")
source("ml/event_models.R", encoding = "UTF-8")
source("ml/event_regression.R", encoding = "UTF-8")
source("ml/candidate_events.R", encoding = "UTF-8")
source("ml/candidate_regression.R", encoding = "UTF-8")
source("ml/daily_transfer.R", encoding = "UTF-8")
source("strategy/fixed/ml_randomness.R", encoding = "UTF-8")

history <- readRDS("strategy/fixed/results/historical_study.rds")$history
overlay_context <- list(
  saved = readRDS("ml/results/tchr_integration/final/frozen_overlay_bundle.rds"),
  thresholds = read.csv("strategy/fixed/results/tchr_ml_overlay_thresholds.csv",
                        stringsAsFactors = FALSE),
  volume = history$setup$bars$Volume,
  run_id = "5d__correlation_pruned__hl_1000"
)
out <- ml_market_null(history, overlay_context, n_paths = n_paths)
result_dir <- "strategy/fixed/results"
write.csv(out$draws, file.path(result_dir, "ml_overlay_model_paths.csv"), row.names = FALSE)
write.csv(out$summary, file.path(result_dir, "ml_overlay_model_tail_tests.csv"), row.names = FALSE)
write.csv(out$observed, file.path(result_dir, "ml_overlay_observed_stats.csv"), row.names = FALSE)
write.csv(out$span, file.path(result_dir, "ml_overlay_bootstrap_span.csv"), row.names = FALSE)
write.csv(out$diagnostics, file.path(result_dir, "ml_overlay_model_diagnostics.csv"), row.names = FALSE)
write.csv(out$coefficients, file.path(result_dir, "ml_overlay_model_coefficients.csv"), row.names = FALSE)
if (nrow(out$optimization)) {
  write.csv(out$optimization, file.path(result_dir, "ml_overlay_egarch_optimization.csv"), row.names = FALSE)
}
print(out$observed)
print(out$summary)
