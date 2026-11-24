#!/usr/bin/env Rscript
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) setwd(dirname(dirname(normalizePath(sub("^--file=", "", script_arg[1L])))))

phase <- commandArgs(trailingOnly = TRUE)
if (!length(phase)) phase <- "all"
if (length(phase) != 1L || !phase %in% c("all", "analysis", "backtest", "pullback", "followup", "randomness", "report", "test")) {
  stop("Usage: Rscript strategy/run.R [all|analysis|backtest|pullback|followup|randomness|report|test]")
}
pkgs <- c("TTR", "Matrix", "digest", "jsonlite")
if (phase %in% c("all", "analysis", "randomness", "report")) pkgs <- c(pkgs, "rugarch")
if (phase %in% c("all", "report")) pkgs <- c(pkgs, "ggplot2", "scales", "svglite", "ragg", "htmltools", "base64enc")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing)) stop("Install packages: ", paste(missing, collapse = ", "))

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
for (file in c("R/data.R", "R/strategy.R", "R/research.R", "R/legacy.R", "R/null_models.R",
               "strategy/engine.R", "strategy/inputs.R", "strategy/evaluate.R", "strategy/backtest.R")) {
  source(file, encoding = "UTF-8")
}
source("strategy/pullback.R", encoding = "UTF-8")
source("strategy/followup.R", encoding = "UTF-8")
if (phase %in% c("test", "backtest", "analysis", "all")) {
  source("strategy/tests.R", encoding = "UTF-8")
  strategy_checks()
}
if (phase == "test") {
  cat('{"strategy_checks":"passed"}', "\n")
  quit(save = "no")
}

if (phase %in% c("all", "analysis", "backtest")) {
  x <- save_backtest(run_strategy_backtest())
} else {
  x <- read_backtest()
}

out <- backtest_brief(x)
if (phase %in% c("all", "analysis", "pullback")) {
  pullback <- save_pullbacks(x)
  out$pullback <- pullback_brief(pullback)
} else if (phase == "report") {
  pullback <- read_pullbacks(x)
}
if (phase %in% c("all", "analysis", "followup")) {
  if (phase == "followup") pullback <- read_pullbacks(x)
  followup <- save_followup(x, pullback)
  out$earlier_market <- subset(followup$early, rule == "P06" & fee_bps == trial_fee)[
    c("trades", "net", "benchmark_net")]
} else if (phase == "report") {
  followup <- read_followup(pullback)
}
if (phase %in% c("all", "analysis", "randomness", "report")) source("strategy/randomness.R", encoding = "UTF-8")
if (phase %in% c("all", "analysis", "randomness")) {
  null <- save_randomness(randomness_study(x))
  out$randomness <- subset(null$summary, period == "Out of sample" & metric == "Selected vs index")
} else if (phase == "report") {
  null <- read_randomness(x)
}
if (phase %in% c("all", "report")) {
  if (!l10n_info()[["UTF-8"]]) {
    candidates <- if (.Platform$OS.type == "windows") "English_United States.utf8" else c("C.UTF-8", "en_US.UTF-8")
    for (locale in candidates) {
      suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
      if (l10n_info()[["UTF-8"]]) break
    }
    stopifnot(l10n_info()[["UTF-8"]])
  }
  source("R/figures.R", encoding = "UTF-8")
  source("report/render.R", encoding = "UTF-8")
  source("strategy/figures.R", encoding = "UTF-8")
  source("strategy/present.R", encoding = "UTF-8")
  render_strategy_figures(x, null, pullback, followup)
  render_strategy_report(x, null, pullback, followup)
}
cat(jsonlite::toJSON(out, dataframe = "rows", digits = 6L, na = "null"), "\n")
