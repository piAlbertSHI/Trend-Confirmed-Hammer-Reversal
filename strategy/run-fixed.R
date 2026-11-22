#!/usr/bin/env Rscript
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) setwd(dirname(dirname(normalizePath(sub("^--file=", "", script_arg[1L])))))

phase <- commandArgs(trailingOnly = TRUE)
if (!length(phase)) phase <- "all"
if (length(phase) != 1L || !phase %in% c("all", "analysis", "report", "verify")) {
  stop("Usage: Rscript strategy/run-fixed.R [all|analysis|report|verify]")
}
pkgs <- c("TTR", "Matrix", "digest", "jsonlite", "rugarch")
if (phase %in% c("all", "report")) pkgs <- c(pkgs, "ggplot2", "scales", "svglite", "ragg", "htmltools", "base64enc")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing)) stop("Install packages: ", paste(missing, collapse = ", "))

files <- c("R/data.R", "R/strategy.R", "R/research.R", "R/legacy.R", "R/null_models.R",
           "strategy/engine.R", "strategy/evaluate.R", "strategy/pullback.R", "strategy/followup.R",
           "strategy/randomness.R", "strategy/fixed/backtest.R", "strategy/fixed/randomness.R")
for (file in files) source(file, encoding = "UTF-8")

fixed_dir <- "strategy/fixed/results"
archive <- file.path(fixed_dir, "historical_study.rds")
inputs <- c(files, "strategy/fixed/protocol.md", unname(market_files))
signature <- setNames(vapply(inputs, function(p) digest::digest(file = p, algo = "sha256"), ""), inputs)

if (phase %in% c("all", "analysis")) {
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  history <- fixed_research()
  models <- fixed_market_null(history)
  outputs <- list(sample = history$diagnostic, performance = history$performance,
                  daily = history$daily, tchr_trades = history$trades, candidates = history$all_rules,
                  block_intervals = history$intervals, simultaneous_rule_test = history$max_t,
                  model_paths = models$draws, model_tail_tests = models$summary,
                  model_coefficients = models$coefficients, model_diagnostics = models$diagnostics,
                  simulation_sample = models$span, egarch_optimization = models$optimization)
  dir.create(fixed_dir, showWarnings = FALSE, recursive = TRUE)
  for (name in names(outputs)) {
    write.csv(outputs[[name]], file.path(fixed_dir, paste0(name, ".csv")),
              row.names = FALSE, fileEncoding = "UTF-8", na = "")
  }
  writeLines(capture.output(sessionInfo()), file.path(fixed_dir, "session-info.txt"), useBytes = TRUE)
  saveRDS(list(history = history, models = models, signature = signature), archive, version = 3L)
} else {
  saved <- readRDS(archive)
  stopifnot(identical(saved$signature, signature))
  history <- saved$history
  models <- saved$models
}

fee_result <- subset(history$performance, fee_bps == trial_fee)
pick <- fee_result[fee_result$rule == "TCHR", ]
day <- history$daily
stopifnot(nrow(day) == 5973L, min(day$date) == fixed_start, max(day$date) == fixed_end,
          pick$net > pick$benchmark_net, pick$net > 0,
          abs(expm1(sum(day[["TCHR log"]])) - pick$net) < 1e-11,
          abs(expm1(sum(day[["Buy and hold log"]])) - pick$benchmark_net) < 1e-11,
          abs(prod(1 + history$trades$net) - (1 + pick$net)) < 1e-11,
          all(table(models$draws$model) == 500L), nrow(history$all_rules) == 51L,
          all(history$max_t$primary_adjusted_p >= history$max_t$primary_p),
          models$span$first == fixed_start, models$span$last == fixed_end,
          abs(expm1(models$observed[["active_log"]]) -
                ((1 + pick$net) / (1 + pick$benchmark_net) - 1)) < 1e-11)

if (phase %in% c("all", "report")) {
  if (!l10n_info()[["UTF-8"]]) {
    locales <- if (.Platform$OS.type == "windows") "English_United States.utf8" else c("C.UTF-8", "en_US.UTF-8")
    for (locale in locales) {
      suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
      if (l10n_info()[["UTF-8"]]) break
    }
    stopifnot(l10n_info()[["UTF-8"]])
  }
  source("R/figures.R", encoding = "UTF-8")
  source("report/render.R", encoding = "UTF-8")
  source("strategy/fixed/present.R", encoding = "UTF-8")
  make_fixed_figures(history, models)
  make_fixed_report(history, models)
}
if (phase == "verify") {
  stopifnot(length(list.files("strategy/fixed/figures", pattern = "[.]svg$")) == 5L,
            file.exists("strategy/report.html"), file.info("strategy/report.html")$size > 100000)
}
cat(jsonlite::toJSON(list(sample = models$span,
                         performance = fee_result[c("rule", "trades", "net", "benchmark_net", "drawdown")],
                         models = subset(models$summary, metric == "active_log")[c("model", "paths", "p")],
                         multiple_comparison = subset(history$max_t, block == 20L),
                         verified = TRUE), dataframe = "rows", digits = 6L, na = "null"), "\n")
