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
source("ml/candidate_policy.R", encoding = "UTF-8")

result_dir <- "ml/results/candidate_regression"
dir.create(result_dir, showWarnings = FALSE, recursive = TRUE)

market <- load_market()$full
ml <- build_ml_table(market)
ml$data <- assign_ml_split(ml$data)
attr(ml$data, "ml_feature_names") <- ml$feature_names
candidate <- build_candidate_event_table(ml$data)
event <- candidate$data
candidate_features <- ml_feature_columns(event)

write.csv(event, file.path(result_dir, "candidate_event_table.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(candidate_event_diagnostics(candidate),
          file.path(result_dir, "candidate_diagnostics.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

set.seed(20250530)
folds <- purged_event_walk_forward_folds(
  event,
  min_train = 50L,
  validation_size = 15L,
  test_size = 10L,
  step = 10L,
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

runs <- list()
for (horizon in c("5d", "10d", "20d")) {
  for (feature_mode in c("fixed", "correlation_pruned")) {
    key <- paste(horizon, feature_mode, sep = "__")
    runs[[key]] <- run_candidate_nested_walk_forward(
      event = event,
      folds = folds,
      horizon = horizon,
      feature_names = candidate_features,
      feature_mode = feature_mode,
      target_type = "utility"
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
selections <- do.call(rbind, lapply(names(runs), function(key) {
  out <- runs[[key]]$selections
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
row.names(selections) <- NULL
row.names(summaries) <- NULL

write.csv(fold_metrics, file.path(result_dir, "candidate_fold_metrics.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(predictions, file.path(result_dir, "candidate_predictions.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(selections, file.path(result_dir, "candidate_selections.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(summaries, file.path(result_dir, "candidate_summaries.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

for (key in names(runs)) {
  for (fold_id in names(runs[[key]]$models)) {
    model <- runs[[key]]$models[[fold_id]]
    write.csv(
      model$grid,
      file.path(result_dir, paste0(key, "__", fold_id, "_grid.csv")),
      row.names = FALSE, fileEncoding = "UTF-8"
    )
    write.csv(
      model$fit$importance,
      file.path(result_dir, paste0(key, "__", fold_id, "_importance.csv")),
      row.names = FALSE, fileEncoding = "UTF-8"
    )
    write.csv(
      model$fit$training_log,
      file.path(result_dir, paste0(key, "__", fold_id, "_training_log.csv")),
      row.names = FALSE, fileEncoding = "UTF-8"
    )
  }
}
writeLines(
  jsonlite::toJSON(
    list(
      analysis_cutoff = as.character(ml_fixed_end),
      events = nrow(event),
      folds = fold_table,
      candidate_diagnostics = candidate_event_diagnostics(candidate),
      summaries = summaries
    ),
    dataframe = "rows", auto_unbox = TRUE, pretty = TRUE, na = "null"
  ),
  file.path(result_dir, "candidate_summary.json")
)
saveRDS(
  list(
    event = event,
    folds = folds,
    runs = runs
  ),
  file.path(result_dir, "candidate_models.rds"),
  version = 3L
)
print(summaries)
cat(jsonlite::toJSON(summaries, dataframe = "rows", auto_unbox = TRUE, na = "null"), "\n")



