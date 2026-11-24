#!/usr/bin/env Rscript

source("ml/metrics.R", encoding = "UTF-8")
source("ml/event_regression.R", encoding = "UTF-8")
source("ml/candidate_random_baseline.R", encoding = "UTF-8")
source("ml/drawdown_audit.R", encoding = "UTF-8")

result_dir <- "ml/results/daily_transfer_weighted"
predictions <- read.csv(file.path(result_dir, "weighted_predictions.csv"),
                        stringsAsFactors = FALSE)
selections <- read.csv(file.path(result_dir, "weighted_selections.csv"),
                       stringsAsFactors = FALSE)

# Select the decay half-life inside each outer-fold validation window. The
# corresponding test rows are then taken from that fold's precomputed run.
selection_rows <- selections[
  selections$horizon == "5d" &
    selections$feature_mode == "correlation_pruned" &
    selections$evaluation_subset == "all",
  , drop = FALSE
]
selection_rows$half_life <- as.integer(sub(".*_hl_", "", selection_rows$run))
fold_ids <- unique(selection_rows$fold)
chosen <- lapply(fold_ids, function(fold_id) {
  z <- selection_rows[selection_rows$fold == fold_id, , drop = FALSE]
  z[which.max(z$validation_mean_utility_bp), , drop = FALSE]
})
chosen <- do.call(rbind, chosen)
chosen_predictions <- do.call(rbind, lapply(seq_len(nrow(chosen)), function(i) {
  z <- predictions[
    predictions$run == chosen$run[i] & predictions$fold == chosen$fold[i],
    , drop = FALSE
  ]
  z$nested_half_life <- chosen$half_life[i]
  z
}))

metric_rows <- lapply(split(chosen_predictions, chosen_predictions$fold), function(z) {
  selected <- as.logical(z$selected)
  selected[is.na(selected)] <- FALSE
  net <- expm1(z$net_return)
  excess <- expm1(z$excess_return)
  utility <- expm1(z$utility)
  data.frame(
    fold = z$fold[1L],
    half_life = z$nested_half_life[1L],
    events = nrow(z),
    selected = sum(selected),
    selection_rate = mean(selected),
    uplift_mean_net_bp = if (sum(selected)) (mean(net[selected]) - mean(net)) * 10000 else 0,
    uplift_mean_excess_bp = if (sum(selected)) (mean(excess[selected]) - mean(excess)) * 10000 else 0,
    uplift_mean_utility_bp = if (sum(selected)) (mean(utility[selected]) - mean(utility)) * 10000 else 0,
    stringsAsFactors = FALSE
  )
})
metric_rows <- do.call(rbind, metric_rows)
net <- metric_rows$uplift_mean_net_bp
excess <- metric_rows$uplift_mean_excess_bp
utility <- metric_rows$uplift_mean_utility_bp
net_ci <- bootstrap_mean_ci(net, seed = 20250530L)
excess_ci <- bootstrap_mean_ci(excess, seed = 20250531L)
utility_ci <- bootstrap_mean_ci(utility, seed = 20250532L)
summary <- data.frame(
  folds = nrow(metric_rows),
  test_events = sum(metric_rows$events),
  mean_uplift_net_bp = mean(net),
  median_uplift_net_bp = median(net),
  positive_net_rate = mean(net > 0),
  net_ci_low_bp = net_ci[["lower"]],
  mean_uplift_excess_bp = mean(excess),
  median_uplift_excess_bp = median(excess),
  positive_excess_rate = mean(excess > 0),
  excess_ci_low_bp = excess_ci[["lower"]],
  mean_uplift_utility_bp = mean(utility),
  median_uplift_utility_bp = median(utility),
  positive_utility_rate = mean(utility > 0),
  utility_ci_low_bp = utility_ci[["lower"]],
  mean_selected_rate = mean(metric_rows$selection_rate),
  stringsAsFactors = FALSE
)
dir.create(file.path(result_dir, "nested_decay"), showWarnings = FALSE,
          recursive = TRUE)
write.csv(chosen, file.path(result_dir, "nested_decay", "selections.csv"), row.names = FALSE)
write.csv(chosen_predictions, file.path(result_dir, "nested_decay", "predictions.csv"), row.names = FALSE)
write.csv(metric_rows, file.path(result_dir, "nested_decay", "fold_metrics.csv"), row.names = FALSE)
write.csv(summary, file.path(result_dir, "nested_decay", "summary.csv"), row.names = FALSE)

# Reuse the standard audit by adding a run identifier expected by the helper.
chosen_predictions$run <- "nested_decay"
random <- run_candidate_random_baseline(chosen_predictions, iterations = 10000L,
                                        seed = 20250530L)
write.csv(random$summaries, file.path(result_dir, "nested_decay", "random_summary.csv"), row.names = FALSE)
write.csv(random$fold_metrics, file.path(result_dir, "nested_decay", "random_fold_metrics.csv"), row.names = FALSE)
event_table <- read.csv("ml/results/candidates/candidate_event_table.csv",
                        stringsAsFactors = FALSE)
dd <- run_drawdown_audit(chosen_predictions, event_table, "nested_decay")
write.csv(dd$folds, file.path(result_dir, "nested_decay", "drawdown_folds.csv"), row.names = FALSE)
write.csv(dd$overall, file.path(result_dir, "nested_decay", "drawdown_overall.csv"), row.names = FALSE)
print(chosen)
print(metric_rows)
print(summary)
print(random$summaries)
print(dd$overall)
