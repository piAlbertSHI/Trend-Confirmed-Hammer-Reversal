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

result_dir <- "ml/results/candidate_original_regression"
dir.create(result_dir, showWarnings = FALSE, recursive = TRUE)

market <- load_market()$full
ml <- build_ml_table(market)
ml$data <- assign_ml_split(ml$data)
attr(ml$data, "ml_feature_names") <- ml$feature_names
candidate <- build_candidate_event_table(ml$data)
event <- candidate$data
feature_names <- ml_feature_columns(event)

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
    validation_original_events = sum(event$original_tchr_setup[fold$validation]),
    test_original_events = sum(event$original_tchr_setup[fold$test]),
    train_embargo_sessions = fold$train_embargo_sessions,
    test_gap_sessions = fold$test_gap_sessions,
    stringsAsFactors = FALSE
  )
}))
write.csv(fold_table, file.path(result_dir, "purged_walk_forward_folds.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

runs <- list()
for (target_type in c("utility", "net_return", "excess_return")) {
  for (feature_mode in c("fixed", "correlation_pruned")) {
    key <- paste(target_type, feature_mode, sep = "__")
    runs[[key]] <- run_candidate_nested_walk_forward(
      event = event,
      folds = folds,
      horizon = "5d",
      feature_names = feature_names,
      feature_mode = feature_mode,
      target_type = target_type,
      evaluation_subset = "original"
    )
  }
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
write.csv(fold_metrics, file.path(result_dir, "original_fold_metrics.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(predictions, file.path(result_dir, "original_predictions.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(selections, file.path(result_dir, "original_selections.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(summaries, file.path(result_dir, "original_summaries.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(fold_table, file.path(result_dir, "purged_walk_forward_folds.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
writeLines(
  jsonlite::toJSON(
    list(
      analysis_cutoff = as.character(ml_fixed_end),
      events = nrow(event),
      original_events = sum(event$original_tchr_setup),
      folds = fold_table,
      summaries = summaries
    ),
    dataframe = "rows", auto_unbox = TRUE, pretty = TRUE, na = "null"
  ),
  file.path(result_dir, "original_summary.json")
)
saveRDS(
  list(event = event, folds = folds, runs = runs),
  file.path(result_dir, "original_models.rds"),
  version = 3L
)
print(summaries)



