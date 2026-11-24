# Full numerical replay of the archived variants. Run from the project root.
source("R/data.R", encoding = "UTF-8")
source("R/strategy.R", encoding = "UTF-8")
source("R/legacy.R", encoding = "UTF-8")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop("Usage: Rscript archive/replay.R [output.rds]")
output <- if (length(args)) args[1L] else "archive/replay.rds"
archive <- read_archive(market_files[["archive"]])
stopifnot(digest::digest(file = market_files[["archive"]], algo = "sha256") ==
            market_hashes[["archive"]])
result <- legacy_draws(archive)
result$input_sha <- market_hashes[["archive"]]
result$implementation_sha <- digest::digest(file = "R/legacy.R", algo = "sha256")
result$environment <- capture.output(sessionInfo())
stopifnot(identical(as.integer(table(factor(result$draws$model,
                levels = c("Random walk", "AR(1)", "GARCH-M", "EGARCH")))),
                    c(500L, 500L, 500L, 50L)))
saveRDS(result, output, version = 3L)
cat(jsonlite::toJSON(legacy_summary(result$draws, 718.48), dataframe = "rows",
                     digits = 5L, na = "null"), "\n")
