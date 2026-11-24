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
source("ml/features.R", encoding = "UTF-8")
source("ml/splits.R", encoding = "UTF-8")
source("ml/preprocess.R", encoding = "UTF-8")
source("ml/metrics.R", encoding = "UTF-8")
source("ml/event_data.R", encoding = "UTF-8")
source("ml/event_models.R", encoding = "UTF-8")
source("ml/event_walk_forward.R", encoding = "UTF-8")
source("ml/event_regression.R", encoding = "UTF-8")
source("ml/candidate_events.R", encoding = "UTF-8")
source("ml/candidate_regression.R", encoding = "UTF-8")
source("ml/candidate_policy_nogate.R", encoding = "UTF-8")
source("ml/daily_transfer.R", encoding = "UTF-8")

result_dir <- "ml/results/daily_transfer_weighted"
dir.create(result_dir, showWarnings = FALSE, recursive = TRUE)
market <- load_market()$full
ml <- build_ml_table(market)
ml$data <- assign_ml_split(ml$data)
attr(ml$data, "ml_feature_names") <- ml$feature_names
candidate <- build_candidate_event_table(ml$data)
event <- candidate$data
daily <- build_daily_economic_table(ml$data)
folds <- purged_event_walk_forward_folds(
  event,
  min_train = 50L,
  validation_size = 15L,
  test_size = 10L,
  step = 10L,
  train_embargo_sessions = 10L,
  test_gap_sessions = 10L
)
runs <- list()
for (half_life in c(1000, 1260, 1500, 1800, 2200, 3000)) {
  key <- paste0("5d__correlation_pruned__hl_", if (is.infinite(half_life)) "full" else half_life)
  runs[[key]] <- run_daily_transfer_walk_forward(
    daily = daily,
    event = event,
    folds = folds,
    horizon = "5d",
    feature_mode = "correlation_pruned",
    evaluation_subset = "all",
    weight_half_life = half_life
  )
}
fold_metrics <- do.call(rbind, lapply(names(runs), function(key) {
  x <- runs[[key]]$fold_metrics
  x$run <- key
  x
}))
predictions <- do.call(rbind, lapply(names(runs), function(key) {
  x <- runs[[key]]$predictions
  x$run <- key
  x
}))
selections <- do.call(rbind, lapply(names(runs), function(key) {
  x <- runs[[key]]$selections
  x$run <- key
  x
}))
summaries <- do.call(rbind, lapply(names(runs), function(key) {
  x <- runs[[key]]$summary
  x$run <- key
  x
}))
write.csv(fold_metrics, file.path(result_dir, "weighted_fold_metrics.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(predictions, file.path(result_dir, "weighted_predictions.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(selections, file.path(result_dir, "weighted_selections.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(summaries, file.path(result_dir, "weighted_summaries.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
saveRDS(list(event = event, daily = daily, folds = folds, runs = runs),
        file.path(result_dir, "weighted_models.rds"), version = 3L)
print(summaries)



