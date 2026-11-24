#!/usr/bin/env Rscript

tail_tests <- read.csv("strategy/fixed/results/ml_overlay_model_tail_tests.csv",
                       stringsAsFactors = FALSE)
transfer <- read.csv("ml/results/tchr_integration/final/overlay_stability_audit.csv",
                     stringsAsFactors = FALSE)
required_models <- c("Random walk", "AR(1)", "GARCH-M", "EGARCH")
model_rows <- tail_tests[tail_tests$model %in% required_models, , drop = FALSE]
matched <- model_rows[model_rows$metric == "ml_matched_log", , drop = FALSE]
active <- model_rows[model_rows$metric == "ml_active_log", , drop = FALSE]
overlay <- model_rows[model_rows$metric == "ml_overlay_log", , drop = FALSE]
paths_complete <- all(model_rows$paths == 500L) &&
  setequal(unique(model_rows$model), required_models)
checks <- data.frame(
  check = c(
    "walk_forward_overlay_stable",
    "four_bootstrap_models_complete",
    "matched_active_p_below_0_05_in_all_models",
    "active_p_below_0_05_in_at_least_two_models",
    "observed_matched_active_positive"
  ),
  value = c(
    isTRUE(transfer$stable_gain[1L]),
    paths_complete,
    all(matched$p < 0.05),
    sum(active$p < 0.05) >= 2L,
    all(matched$observed > 0)
  ),
  stringsAsFactors = FALSE
)
summary <- data.frame(
  stable_closure = all(checks$value),
  transfer_test_events = transfer$test_events[1L],
  transfer_mean_trade_uplift_bp = transfer$mean_trade_uplift_bp[1L],
  transfer_trade_uplift_ci_low_bp = transfer$trade_uplift_ci_low_bp[1L],
  transfer_random_p = transfer$random_trade_p[1L],
  bootstrap_models = length(unique(model_rows$model)),
  bootstrap_paths_per_model = min(model_rows$paths),
  matched_active_models_below_0_05 = sum(matched$p < 0.05),
  active_models_below_0_05 = sum(active$p < 0.05),
  matched_active_min_p = min(matched$p),
  matched_active_max_p = max(matched$p),
  base_compound = transfer$base_compound[1L],
  overlay_compound = transfer$overlay_compound[1L],
  base_max_drawdown = transfer$base_max_drawdown[1L],
  overlay_max_drawdown = transfer$overlay_max_drawdown[1L],
  stringsAsFactors = FALSE
)
dir.create("ml/results/tchr_integration/final", showWarnings = FALSE, recursive = TRUE)
write.csv(checks, "ml/results/tchr_integration/final/unified_closure_checks.csv", row.names = FALSE)
write.csv(summary, "ml/results/tchr_integration/final/unified_closure_summary.csv", row.names = FALSE)
write.csv(model_rows, "ml/results/tchr_integration/final/unified_bootstrap_tail_tests.csv", row.names = FALSE)
print(checks)
print(summary)
print(matched[, c("model", "observed", "p", "q025", "median", "q975")])
print(active[, c("model", "observed", "p", "q025", "median", "q975")])
print(overlay[, c("model", "observed", "p", "q025", "median", "q975")])
if (!isTRUE(summary$stable_closure)) stop("Unified closure audit failed.")
