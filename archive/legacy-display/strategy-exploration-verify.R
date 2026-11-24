#!/usr/bin/env Rscript
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script_arg)) setwd(dirname(dirname(normalizePath(sub("^--file=", "", script_arg[1L])))))
for (file in c("R/data.R", "R/strategy.R", "R/research.R", "R/legacy.R", "R/null_models.R",
               "strategy/engine.R", "strategy/inputs.R", "strategy/evaluate.R", "strategy/backtest.R",
               "strategy/pullback.R", "strategy/followup.R", "strategy/randomness.R")) {
  source(file, encoding = "UTF-8")
}

x <- read_backtest()
p <- read_pullbacks(x)
f <- read_followup(p)
n <- read_randomness(x)

# Repeat the decision with only the data that existed when the development window ended.
before <- x$market$full[x$market$full$Date <= as.Date("2016-12-31"), ]
rates <- return_components(before$Open, before$Close)
bars <- position_matrix(before, x$configurations)
rows <- seq.int(warmup + 1L, nrow(before))
phases <- training_blocks(rows, as.Date(c("2005-12-31", "2010-12-31")), before$Date)
trained <- training_table(rates, bars, x$configurations, rows, phases)
stopifnot(identical(frozen_choices(trained)$id, x$selected$id),
          identical(trained$rank, x$training$rank),
          nrow(x$configurations) == 33L, nrow(p$ranking) == 18L)
stopifnot(x$primary$id == "R16", x$primary$slow == 100L,
          f$rule$id == "P06", f$audit$count_overlaps == 56L,
          f$audit$last < min(x$market$archive$Date), x$fund$audit$matched_sessions == 4631L,
          !any(x$walk$annual$train_end >= x$walk$annual$first))

market <- x$market$full
q <- subset(x$scores, instrument == "^HSI" & rule == "Selected" & period == "Out of sample" & fee_bps == 5)
journal <- x$trades
stopifnot(q$trades == nrow(journal), all(journal$entry_date <= journal$exit_date),
          all(journal$entry == market$Open[match(journal$entry_date, market$Date)]),
          all(journal$exit[journal$exit_at == "open"] ==
                market$Open[match(journal$exit_date[journal$exit_at == "open"], market$Date)]),
          abs(prod(1 + journal$net) - 1 - q$net) < 1e-12,
          abs(expm1(sum(x$data_oos[["Selected log"]])) - q$net) < 1e-12)

selected <- subset(x$scores, rule == "Selected" & period == "Out of sample" & fee_bps == 5 & instrument == "^HSI")
full <- subset(x$scores, rule == "Selected" & period == "Complete" & fee_bps == 5 & instrument == "^HSI")
late <- subset(p$scores, rule == "P06" & period == "Out of sample" & fee_bps == 5 & instrument == "^HSI")
earlier <- subset(f$early, rule == "P06" & fee_bps == 5)
stopifnot(full$passes_both, !selected$passes_both, late$passes_both, !earlier$passes_both,
          abs(late$net - expm1(sum(f$oos_daily[["P06 log"]]))) < 1e-12,
          abs(earlier$net - expm1(sum(f$early_daily[["P06 log"]]))) < 1e-12)

stopifnot(all(table(n$draws$model, n$draws$period) == 500L),
          all(n$summary$draws == 500L),
          n$metadata$calibration_last < n$metadata$simulation_first,
          all(is.finite(n$draws$p06_active_log)),
          all(n$summary$p_one_sided == (1 + n$summary$at_least_observed) / 501),
          all(x$family$adjusted_p >= x$family$unadjusted_p),
          all(f$adjusted$adjusted_p >= f$adjusted$unadjusted_p),
          n$random_entry$summary$reps == 999L)
obs <- subset(n$observed, period == "Out of sample")
stopifnot(abs(expm1(obs$net_log) - selected$net) < 1e-12,
          abs(expm1(obs$p06_net_log) - late$net) < 1e-12,
          length(list.files("strategy/figures", pattern = "[.]svg$")) == 7L,
          length(list.files("strategy/figures", pattern = "[.]png$")) == 7L,
          file.info("strategy/report.html")$size > 100000L)

cat(jsonlite::toJSON(list(verified = "passed", candidates = nrow(x$configurations) + nrow(p$ranking),
                          annual_folds = nrow(x$walk$annual), audited_trades = nrow(journal),
                          simulated_price_paths = nrow(subset(n$draws, period == "Out of sample")),
                          earlier_holdout_sessions = earlier$sessions, n_figures = 7L),
                      auto_unbox = TRUE), "\n")
