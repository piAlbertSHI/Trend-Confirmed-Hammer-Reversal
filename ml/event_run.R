#!/usr/bin/env Rscript
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  setwd(dirname(dirname(normalizePath(sub("^--file=", "", script_arg[1L])))))
}

phase <- commandArgs(trailingOnly = TRUE)
if (!length(phase)) phase <- "all"
if (length(phase) != 1L || !phase %in% c("diagnostics", "walk_forward", "all")) {
  stop("Usage: Rscript ml/event_run.R [diagnostics|walk_forward|all]")
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

result_dir <- "ml/results/event"
dir.create(result_dir, showWarnings = FALSE, recursive = TRUE)

market <- load_market()$full
ml <- build_ml_table(market)
ml$data <- assign_ml_split(ml$data)
attr(ml$data, "ml_feature_names") <- ml$feature_names
events <- build_event_table(ml$data, min_gap = 5L)
event <- events$data
model_features <- events$model_features

write.csv(event, file.path(result_dir, "event_table.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(event_label_diagnostics(event),
          file.path(result_dir, "label_diagnostics.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(event_feature_diagnostics(event, selected = model_features),
          file.path(result_dir, "feature_diagnostics.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(event_feature_correlation(event, selected = model_features),
          file.path(result_dir, "feature_correlations.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(event_year_counts(event), file.path(result_dir, "event_year_counts.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
writeLines(
  jsonlite::toJSON(
    list(
      analysis_cutoff = as.character(ml_fixed_end),
      event_count = nrow(event),
      independent_gap_sessions = events$min_gap,
      model_features = model_features,
      label_diagnostics = event_label_diagnostics(event)
    ),
    dataframe = "rows", auto_unbox = TRUE, pretty = TRUE, na = "null"
  ),
  file.path(result_dir, "event_data_summary.json")
)

cat("Independent TCHR events: ", nrow(event), "\n", sep = "")
cat("Pre-specified event features: ", length(model_features), "\n", sep = "")
print(event_label_diagnostics(event))
print(event_feature_diagnostics(event, selected = model_features))

if (phase == "diagnostics") {
  quit(save = "no", status = 0L)
}

set.seed(20250530)
folds <- event_walk_forward_folds(
  event,
  min_train = 20L,
  validation_size = 8L,
  test_size = 6L,
  step = 6L,
  embargo_sessions = 10L
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
    embargo_sessions = fold$embargo_sessions,
    stringsAsFactors = FALSE
  )
}))
write.csv(fold_table, file.path(result_dir, "walk_forward_folds.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

targets <- c("label_net_5d", "label_excess_5d")
selection_rules <- c("validation_threshold", "top_fraction")
walk_results <- list()
for (target in targets) {
  for (selection_rule in selection_rules) {
    key <- paste(target, selection_rule, sep = "__")
    walk_results[[key]] <- run_event_walk_forward(
      event = event,
      folds = folds,
      feature_names = model_features,
      target = target,
      selection_rule = selection_rule,
      top_fraction = 0.5
    )
  }
}

fold_metrics <- do.call(rbind, lapply(names(walk_results), function(key) {
  out <- walk_results[[key]]$fold_metrics
  out$run <- key
  out
}))
predictions <- do.call(rbind, lapply(names(walk_results), function(key) {
  out <- walk_results[[key]]$predictions
  out$run <- key
  out
}))
summaries <- do.call(rbind, lapply(names(walk_results), function(key) {
  out <- walk_results[[key]]$summary
  parts <- strsplit(key, "__", fixed = TRUE)[[1L]]
  out$target <- parts[1L]
  out$selection_rule <- parts[2L]
  out$run <- key
  out
}))
row.names(fold_metrics) <- NULL
row.names(predictions) <- NULL
row.names(summaries) <- NULL

write.csv(fold_metrics, file.path(result_dir, "walk_forward_fold_metrics.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(predictions, file.path(result_dir, "walk_forward_predictions.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(summaries, file.path(result_dir, "walk_forward_summaries.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

for (key in names(walk_results)) {
  models <- walk_results[[key]]$models
  for (fold_id in names(models)) {
    write.csv(
      models[[fold_id]]$importance,
      file.path(result_dir, paste0(key, "__", fold_id, "_importance.csv")),
      row.names = FALSE, fileEncoding = "UTF-8"
    )
    write.csv(
      models[[fold_id]]$training_log,
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
      model_features = model_features,
      folds = fold_table,
      summaries = summaries
    ),
    dataframe = "rows", auto_unbox = TRUE, pretty = TRUE, na = "null"
  ),
  file.path(result_dir, "walk_forward_summary.json")
)
saveRDS(
  list(
    event = event,
    model_features = model_features,
    folds = folds,
    walk_forward = walk_results
  ),
  file.path(result_dir, "walk_forward_models.rds"),
  version = 3L
)
print(fold_metrics)
print(summaries)
cat(jsonlite::toJSON(summaries, dataframe = "rows", auto_unbox = TRUE, na = "null"), "\n")

