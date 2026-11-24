#!/usr/bin/env Rscript
source("ml/metrics.R", encoding = "UTF-8")
source("ml/event_regression.R", encoding = "UTF-8")
source("ml/candidate_random_baseline.R", encoding = "UTF-8")
source("ml/drawdown_audit.R", encoding = "UTF-8")

root <- "ml/results/daily_transfer_weighted"
p <- read.csv(file.path(root, "nested_decay", "predictions.csv"), stringsAsFactors = FALSE)
r <- read.csv("ml/results/daily_risk_transfer/risk_predictions.csv", stringsAsFactors = FALSE)
s <- read.csv("ml/results/daily_risk_transfer/risk_selections.csv", stringsAsFactors = FALSE)

z <- merge(p, r[, c("fold", "event_id", "adverse_score")],
           by = c("fold", "event_id"), all.x = TRUE, sort = FALSE)
threshold <- s[, c("fold", "risk_threshold"), drop = FALSE]
z <- merge(z, threshold, by = "fold", all.x = TRUE, sort = FALSE)
z$selected_base <- as.logical(z$selected)
z$selected_base[is.na(z$selected_base)] <- FALSE
z$selected <- z$selected_base & is.finite(z$adverse_score) &
  z$adverse_score >= z$risk_threshold
z$run <- "nested_risk_filter"

metrics <- do.call(rbind, lapply(split(z, z$fold), function(q) {
  selected <- q$selected
  net <- expm1(q$net_return)
  excess <- expm1(q$excess_return)
  utility <- expm1(q$utility)
  data.frame(
    fold = q$fold[1L], events = nrow(q), selected = sum(selected),
    selection_rate = mean(selected),
    uplift_mean_net_bp = if (sum(selected)) (mean(net[selected]) - mean(net)) * 10000 else 0,
    uplift_mean_excess_bp = if (sum(selected)) (mean(excess[selected]) - mean(excess)) * 10000 else 0,
    uplift_mean_utility_bp = if (sum(selected)) (mean(utility[selected]) - mean(utility)) * 10000 else 0,
    stringsAsFactors = FALSE
  )
}))
net <- metrics$uplift_mean_net_bp
excess <- metrics$uplift_mean_excess_bp
utility <- metrics$uplift_mean_utility_bp
summary <- data.frame(
  folds = nrow(metrics), test_events = sum(metrics$events),
  mean_uplift_net_bp = mean(net), median_uplift_net_bp = median(net),
  positive_net_rate = mean(net > 0), net_ci_low_bp = bootstrap_mean_ci(net, seed = 20250530L)[["lower"]],
  mean_uplift_excess_bp = mean(excess), median_uplift_excess_bp = median(excess),
  positive_excess_rate = mean(excess > 0), excess_ci_low_bp = bootstrap_mean_ci(excess, seed = 20250531L)[["lower"]],
  mean_uplift_utility_bp = mean(utility), median_uplift_utility_bp = median(utility),
  positive_utility_rate = mean(utility > 0), utility_ci_low_bp = bootstrap_mean_ci(utility, seed = 20250532L)[["lower"]],
  mean_selected_rate = mean(metrics$selection_rate), stringsAsFactors = FALSE
)
dir.create(file.path(root, "nested_risk_filter"), showWarnings = FALSE, recursive = TRUE)
write.csv(z, file.path(root, "nested_risk_filter", "predictions.csv"), row.names = FALSE)
write.csv(metrics, file.path(root, "nested_risk_filter", "fold_metrics.csv"), row.names = FALSE)
write.csv(summary, file.path(root, "nested_risk_filter", "summary.csv"), row.names = FALSE)
random <- run_candidate_random_baseline(z, iterations = 10000L, seed = 20250530L)
write.csv(random$summaries, file.path(root, "nested_risk_filter", "random_summary.csv"), row.names = FALSE)
write.csv(random$fold_metrics, file.path(root, "nested_risk_filter", "random_fold_metrics.csv"), row.names = FALSE)
event_table <- read.csv("ml/results/candidates/candidate_event_table.csv", stringsAsFactors = FALSE)
dd <- run_drawdown_audit(z, event_table, "nested_risk_filter")
write.csv(dd$folds, file.path(root, "nested_risk_filter", "drawdown_folds.csv"), row.names = FALSE)
write.csv(dd$overall, file.path(root, "nested_risk_filter", "drawdown_overall.csv"), row.names = FALSE)
print(metrics)
print(summary)
print(random$summaries)
print(dd$overall)
