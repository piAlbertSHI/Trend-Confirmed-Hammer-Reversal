#!/usr/bin/env Rscript
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) {
  setwd(dirname(dirname(normalizePath(sub("^--file=", "", script_arg[1L])))))
}
source("ml/candidate_random_baseline.R", encoding = "UTF-8")
result_dir <- "ml/results/candidate_regression"
predictions <- read.csv(
  file.path(result_dir, "candidate_predictions.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
out <- run_candidate_random_baseline(predictions, iterations = 10000L, seed = 20250530L)
write.csv(out$fold_metrics, file.path(result_dir, "random_baseline_fold_metrics.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(out$summaries, file.path(result_dir, "random_baseline_summaries.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
writeLines(
  jsonlite::toJSON(
    list(iterations = 10000L, summaries = out$summaries),
    dataframe = "rows", auto_unbox = TRUE, pretty = TRUE, na = "null"
  ),
  file.path(result_dir, "random_baseline_summary.json")
)
print(out$summaries)

