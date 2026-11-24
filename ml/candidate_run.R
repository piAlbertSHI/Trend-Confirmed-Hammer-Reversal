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
source("ml/candidate_events.R", encoding = "UTF-8")

result_dir <- "ml/results/candidates"
dir.create(result_dir, showWarnings = FALSE, recursive = TRUE)

market <- load_market()$full
ml <- build_ml_table(market)
ml$data <- assign_ml_split(ml$data)
attr(ml$data, "ml_feature_names") <- ml$feature_names
candidate <- build_candidate_event_table(ml$data)
write.csv(candidate$data, file.path(result_dir, "candidate_event_table.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
diagnostics <- candidate_event_diagnostics(candidate)
write.csv(diagnostics, file.path(result_dir, "candidate_diagnostics.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
year_counts <- as.data.frame(table(year = format(candidate$data$Date, "%Y")),
                             stringsAsFactors = FALSE)
names(year_counts)[2L] <- "events"
write.csv(year_counts, file.path(result_dir, "candidate_year_counts.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
writeLines(
  jsonlite::toJSON(
    list(
      analysis_cutoff = as.character(ml_fixed_end),
      parameters = candidate$parameters,
      events = nrow(candidate$data),
      original_tchr_events = sum(candidate$data$original_tchr_setup),
      candidate_only_events = sum(candidate$data$candidate_only),
      diagnostics = diagnostics
    ),
    dataframe = "rows", auto_unbox = TRUE, pretty = TRUE, na = "null"
  ),
  file.path(result_dir, "candidate_summary.json")
)
print(candidate$parameters)
print(diagnostics)
cat(jsonlite::toJSON(diagnostics, dataframe = "rows", auto_unbox = TRUE, na = "null"), "\n")

