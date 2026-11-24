#!/usr/bin/env Rscript
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  setwd(dirname(dirname(normalizePath(sub("^--file=", "", script_arg[1L])))))
}

phase <- commandArgs(trailingOnly = TRUE)
if (!length(phase)) phase <- "all"
if (length(phase) != 1L || !phase %in% c("diagnostics", "walk_forward", "all")) {
  stop("Usage: Rscript ml/event_regression_run.R [diagnostics|walk_forward|all]")
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

result_dir <- "ml/results/event_regression"
dir.create(result_dir, showWarnings = FALSE, recursive = TRUE)

market <- load_market()$full
ml <- build_ml_table(market)
ml$data <- assign_ml_split(ml$data)
attr(ml$data, "ml_feature_names") <- ml$feature_names
events <- build_event_table(ml$data, min_gap = 5L)
event <- events$data
candidate_features <- events$model_features

write.csv(event, file.path(result_dir, "event_table.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(
  event_feature_diagnostics(event, selected = candidate_features),
  file.path(result_dir, "feature_diagnostics.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
write.csv(
  event_feature_correlation(event, selected = candidate_features),
  file.path(result_dir, "feature_correlations.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

cat("Independent TCHR events: ", nrow(event), "\n", sep = "")
cat("Candidate features: ", length(candidate_features), "\n", sep = "")
if (phase == "diagnostics") {
  quit(save = "no", status = 0L)
}

set.seed(20250530)
folds <- purged_event_walk_forward_folds(
  event,
  min_train = 20L,
  validation_size = 8L,
  test_size = 6L,
  step = 6L,
  train_embargo_sessions = 10L,
  test_gap_sessions = 10L
)
fold_table <- do.call(rbind, lapply(folds, function(fold) {
  data.frame(
    id = fold$id,
    train_start = fold$train_start,
    train_end = fold$train_end,
    validation_start = fold$validation_start,
    validation_end = fold$validation_end,
    test_start = fold$test_start,
    test_end = fold$test_end,
    train_events = length(fold$train),
    validation_events = length(fold$validation),
    test_events = length(fold$test),
    train_embargo_sessions = fold$train_embargo_sessions,
    test_gap_sessions = fold$test_gap_sessions,
    stringsAsFactors = FALSE
  )
}))
write.csv(fold_table, file.path(result_dir, "purged_walk_forward_folds.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

specs <- list(
  net_return_5d = "net_return_5d",
  excess_return_5d = "excess_return_5d"
)
feature_modes <- c("fixed", "correlation_pruned")
runs <- list()
for (target in names(specs)) {
  for (feature_mode in feature_modes) {
    key <- paste(target, feature_mode, sep = "__")
    runs[[key]] <- run_event_regression_walk_forward(
      event = event,
      folds = folds,
      feature_names = candidate_features,
      target = specs[[target]],
      feature_mode = feature_mode,
      selection_fraction = 0.5
    )
  }
}

fold_metrics <- do.call(rbind, lapply(names(runs), function(key) {
  out <- runs[[key]]$fold_metrics
  out$run <- key
  out
}))
predictions <- do.call(rbind, lapply(names(runs), function(key) {
  out <- runs[[key]]$predictions
  out$run <- key
  out
}))
feature_selection <- do.call(rbind, lapply(names(runs), function(key) {
  out <- runs[[key]]$feature_selection
  out$run <- key
  out
}))
summaries <- do.call(rbind, lapply(names(runs), function(key) {
  out <- runs[[key]]$summary
  out$run <- key
  out
}))
row.names(fold_metrics) <- NULL
row.names(predictions) <- NULL
row.names(feature_selection) <- NULL
row.names(summaries) <- NULL

write.csv(fold_metrics, file.path(result_dir, "regression_fold_metrics.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(predictions, file.path(result_dir, "regression_predictions.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(feature_selection, file.path(result_dir, "regression_feature_selection.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(summaries, file.path(result_dir, "regression_summaries.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
config <- list(
  analysis_cutoff = as.character(ml_fixed_end),
  target_scale = 10000,
  nrounds = 600L,
  early_stopping_rounds = 60L,
  feature_correlation_threshold = 0.75,
  selection_fraction = 0.5,
  params = runs[[1L]]$models[[1L]]$params
)
writeLines(
  jsonlite::toJSON(config, auto_unbox = TRUE, pretty = TRUE),
  file.path(result_dir, "regression_model_config.json")
)

for (key in names(runs)) {
  for (fold_id in names(runs[[key]]$models)) {
    model <- runs[[key]]$models[[fold_id]]
    write.csv(
      model$importance,
      file.path(result_dir, paste0(key, "__", fold_id, "_importance.csv")),
      row.names = FALSE, fileEncoding = "UTF-8"
    )
    write.csv(
      model$training_log,
      file.path(result_dir, paste0(key, "__", fold_id, "_training_log.csv")),
      row.names = FALSE, fileEncoding = "UTF-8"
    )
  }
}

writeLines(
  jsonlite::toJSON(
    list(
      analysis_cutoff = as.character(ml_fixed_end),
      event_count = nrow(event),
      candidate_features = candidate_features,
      folds = fold_table,
      summaries = summaries
    ),
    dataframe = "rows", auto_unbox = TRUE, pretty = TRUE, na = "null"
  ),
  file.path(result_dir, "regression_summary.json")
)
saveRDS(
  list(
    event = event,
    candidate_features = candidate_features,
    folds = folds,
    runs = runs
  ),
  file.path(result_dir, "regression_models.rds"),
  version = 3L
)
print(summaries)
cat(jsonlite::toJSON(summaries, dataframe = "rows", auto_unbox = TRUE, na = "null"), "\n")


