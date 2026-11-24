source("ml/candidate_random_baseline.R", encoding = "UTF-8")
p <- read.csv("ml/results/daily_risk_transfer/risk_predictions.csv", stringsAsFactors = FALSE)
p$run <- "daily_risk_transfer__hl_1260"
o <- run_candidate_random_baseline(p, iterations = 10000L, seed = 20250530L)
write.csv(o$fold_metrics, "ml/results/daily_risk_transfer/random_baseline_fold_metrics.csv", row.names = FALSE)
write.csv(o$summaries, "ml/results/daily_risk_transfer/random_baseline_summaries.csv", row.names = FALSE)
print(o$summaries)
