#!/usr/bin/env Rscript

result_dir <- "ml/results/daily_transfer_weighted"
run_id <- "5d__correlation_pruned__hl_1000"
summaries <- read.csv(file.path(result_dir, "weighted_summaries.csv"),
                      stringsAsFactors = FALSE)
random <- read.csv(file.path(result_dir, "random_baseline_summaries.csv"),
                   stringsAsFactors = FALSE)
drawdown <- read.csv(file.path(result_dir, "drawdown_hl1000_overall.csv"),
                     stringsAsFactors = FALSE)
folds <- read.csv(file.path(result_dir, "weighted_fold_metrics.csv"),
                  stringsAsFactors = FALSE)
folds <- folds[folds$run == run_id, , drop = FALSE]
observed <- summaries[summaries$run == run_id, , drop = FALSE]
null <- random[random$run == run_id, , drop = FALSE]
dd <- drawdown[drawdown$run == run_id, , drop = FALSE]
if (nrow(observed) != 1L || nrow(null) != 1L || nrow(dd) != 1L) {
  stop("Final audit inputs are incomplete for ", run_id)
}

checks <- data.frame(
  check = c(
    "test_events_at_least_40",
    "net_positive_fold_rate_at_least_75pct",
    "excess_positive_fold_rate_at_least_75pct",
    "utility_positive_fold_rate_at_least_75pct",
    "net_median_positive",
    "excess_median_positive",
    "utility_median_positive",
    "net_bootstrap_lower_above_zero",
    "excess_bootstrap_lower_above_zero",
    "utility_bootstrap_lower_above_zero",
    "random_net_p_below_0_05",
    "random_excess_p_below_0_05",
    "random_utility_p_below_0_05",
    "mean_adverse_not_worse",
    "worst_adverse_not_worse",
    "max_drawdown_not_worse"
  ),
  value = c(
    observed$test_events >= 40,
    observed$positive_net_uplift_rate >= 0.75,
    observed$positive_excess_uplift_rate >= 0.75,
    observed$positive_utility_uplift_rate >= 0.75,
    observed$median_uplift_net_bp > 0,
    observed$median_uplift_excess_bp > 0,
    observed$median_uplift_utility_bp > 0,
    observed$net_uplift_ci_low_bp > 0,
    observed$excess_uplift_ci_low_bp > 0,
    observed$utility_uplift_ci_low_bp > 0,
    null$random_p_net < 0.05,
    null$random_p_excess < 0.05,
    null$random_p_utility < 0.05,
    dd$selected_mean_adverse_bp >= dd$baseline_mean_adverse_bp,
    dd$selected_worst_adverse_bp >= dd$baseline_worst_adverse_bp,
    dd$drawdown_not_worse
  ),
  stringsAsFactors = FALSE
)
audit <- data.frame(
  run = run_id,
  stable_gain = all(checks$value),
  checks_passed = sum(checks$value),
  checks_total = nrow(checks),
  mean_net_uplift_bp = observed$mean_uplift_net_bp,
  net_ci_low_bp = observed$net_uplift_ci_low_bp,
  mean_excess_uplift_bp = observed$mean_uplift_excess_bp,
  excess_ci_low_bp = observed$excess_uplift_ci_low_bp,
  mean_utility_uplift_bp = observed$mean_uplift_utility_bp,
  utility_ci_low_bp = observed$utility_uplift_ci_low_bp,
  random_p_net = null$random_p_net,
  random_p_excess = null$random_p_excess,
  random_p_utility = null$random_p_utility,
  baseline_mean_adverse_bp = dd$baseline_mean_adverse_bp,
  selected_mean_adverse_bp = dd$selected_mean_adverse_bp,
  baseline_worst_adverse_bp = dd$baseline_worst_adverse_bp,
  selected_worst_adverse_bp = dd$selected_worst_adverse_bp,
  baseline_max_drawdown_bp = dd$baseline_max_drawdown_bp,
  selected_max_drawdown_bp = dd$selected_max_drawdown_bp,
  stringsAsFactors = FALSE
)
dir.create("ml/results/daily_final", showWarnings = FALSE, recursive = TRUE)
write.csv(checks, "ml/results/daily_final/stability_checks.csv", row.names = FALSE)
write.csv(audit, "ml/results/daily_final/stability_audit.csv", row.names = FALSE)
print(checks)
print(audit)
if (!isTRUE(audit$stable_gain)) stop("Stable-gain gate failed.")
