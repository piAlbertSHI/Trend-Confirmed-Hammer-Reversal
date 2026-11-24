#!/usr/bin/env Rscript
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  setwd(dirname(dirname(normalizePath(sub("^--file=", "", script_arg[1L])))))
}

phase <- commandArgs(trailingOnly = TRUE)
if (!length(phase)) phase <- "all"
if (length(phase) != 1L || !phase %in% c("check", "models", "all")) {
  stop("Usage: Rscript ml/run.R [check|models|all]")
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
source("ml/models.R", encoding = "UTF-8")

result_dir <- "ml/results"
dir.create(result_dir, showWarnings = FALSE, recursive = TRUE)
market <- load_market()$full
ml <- build_ml_table(market)
ml$data <- assign_ml_split(ml$data)
attr(ml$data, "ml_feature_names") <- ml$feature_names

write_ml_inputs <- function(ml) {
  write.csv(ml$quality, file.path(result_dir, "data_quality.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(feature_dictionary(ml$data), file.path(result_dir, "feature_dictionary.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(split_summary(ml$data), file.path(result_dir, "time_splits.csv"),
            row.names = FALSE, fileEncoding = "UTF-8")
  data_signature <- vapply(
    c("R/data.R", "R/strategy.R", "ml/features.R", "ml/splits.R",
      "ml/preprocess.R", "ml/metrics.R", "ml/models.R", "ml/run.R",
      unname(market_files)),
    function(path) digest::digest(file = path, algo = "sha256"),
    ""
  )
  writeLines(
    jsonlite::toJSON(
      list(
        analysis_cutoff = as.character(ml_fixed_end),
        data_window = list(start = as.character(min(ml$data$Date)),
                           end = as.character(max(ml$data$Date)),
                           sessions = nrow(ml$data)),
        fee_bps = ml$fee_bps,
        feature_count = length(ml$feature_names),
        input_hashes = data_signature
      ),
      auto_unbox = TRUE, pretty = TRUE
    ),
    file.path(result_dir, "run_metadata.json")
  )
}

write_ml_inputs(ml)
cat("Data checks completed for ", nrow(ml$data), " sessions and ",
    length(ml$feature_names), " features.\n", sep = "")
print(ml$quality)
print(split_summary(ml$data))

if (phase == "check") {
  cat(jsonlite::toJSON(list(quality = ml$quality, splits = split_summary(ml$data)),
                       dataframe = "rows", auto_unbox = TRUE, na = "null"), "\n")
  quit(save = "no", status = 0L)
}

train_rows <- split_rows(ml$data, "train")
validation_rows <- split_rows(ml$data, "validation")
holdout_rows <- split_rows(ml$data, "holdout")
if (length(train_rows) < 100L || length(validation_rows) < 20L || length(holdout_rows) < 20L) {
  stop("One of the declared time splits has too few labeled rows.")
}

set.seed(20250530)
cat("Fitting Elastic Net with chronological cross-validation...\n")
elastic <- fit_elastic_net(ml$data, train_rows, validation_rows)
cat("Fitting regularized XGBoost with validation early stopping...\n")
xgb <- fit_xgboost(ml$data, train_rows, validation_rows)

holdout_y <- target_vector(ml$data, holdout_rows, "target_positive_5d")
elastic_holdout <- elastic$predict(holdout_rows)
xgb_holdout <- xgb$predict(holdout_rows)
elastic_holdout_tagged <- structure(elastic_holdout, rows = holdout_rows)
xgb_holdout_tagged <- structure(xgb_holdout, rows = holdout_rows)

metric_row <- function(model, split, y, p) {
  out <- classification_metrics(y, p)
  out$model <- model
  out$split <- split
  out[c("model", "split", setdiff(names(out), c("model", "split")))]
}
model_metrics <- rbind(
  metric_row("Elastic Net", "validation",
             target_vector(ml$data, validation_rows),
             elastic$validation_prediction),
  metric_row("XGBoost", "validation",
             target_vector(ml$data, validation_rows),
             xgb$validation_prediction),
  metric_row("Elastic Net", "holdout", holdout_y, elastic_holdout),
  metric_row("XGBoost", "holdout", holdout_y, xgb_holdout)
)
write.csv(model_metrics, file.path(result_dir, "model_metrics.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

event_rows <- prepare_event_rows(ml$data, holdout_rows)
thresholds <- seq(0.50, 0.80, by = 0.05)
event_results <- do.call(rbind, c(
  list(event_metrics(ml$data, event_rows, model = "TCHR setup")),
  lapply(thresholds, function(threshold) {
    rbind(
      event_metrics(ml$data, event_rows, elastic_holdout_tagged,
                    threshold = threshold, model = "Elastic Net"),
      event_metrics(ml$data, event_rows, xgb_holdout_tagged,
                    threshold = threshold, model = "XGBoost")
    )
  })
))
write.csv(event_results, file.path(result_dir, "event_metrics.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

write.csv(elastic$cv, file.path(result_dir, "elastic_net_cv.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(xgb$importance, file.path(result_dir, "xgboost_importance.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(xgb$training_log, file.path(result_dir, "xgboost_training_log.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

folds <- time_cv_folds(ml$data, split = "train", blocks = 4L,
                       embargo = 10L, target = "target_positive_5d")
fold_table <- do.call(rbind, lapply(folds, function(fold) {
  data.frame(
    id = fold$id,
    train_start = fold$train_start,
    train_end = fold$train_end,
    validation_start = fold$validation_start,
    validation_end = fold$validation_end,
    train_rows = length(fold$train),
    validation_rows = length(fold$validation),
    embargo_sessions = 10L
  )
}))
write.csv(fold_table, file.path(result_dir, "time_cv_folds.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

saveRDS(
  list(
    data_window = ml$window,
    feature_names = ml$feature_names,
    split_definitions = ml_split_definitions(),
    elastic_net = elastic,
    xgboost = xgb
  ),
  file.path(result_dir, "models.rds"),
  version = 3L
)

summary <- list(
  window = list(start = as.character(min(ml$data$Date)),
                end = as.character(max(ml$data$Date)),
                sessions = nrow(ml$data)),
  features = length(ml$feature_names),
  event_rows_holdout = length(event_rows),
  elastic_net = list(alpha = elastic$alpha, lambda = elastic$lambda,
                     validation = elastic$validation_metrics,
                     holdout = classification_metrics(holdout_y, elastic_holdout)),
  xgboost = list(best_iteration = xgb$best_iteration,
                 validation = xgb$validation_metrics,
                 holdout = classification_metrics(holdout_y, xgb_holdout)),
  output_dir = normalizePath(result_dir, winslash = "/", mustWork = FALSE)
)
writeLines(
  jsonlite::toJSON(summary, dataframe = "rows", auto_unbox = TRUE,
                   pretty = TRUE, na = "null"),
  file.path(result_dir, "run_summary.json")
)
cat(jsonlite::toJSON(summary, dataframe = "rows", auto_unbox = TRUE, na = "null"), "\n")




