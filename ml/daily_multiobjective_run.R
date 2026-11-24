source("R/data.R", encoding = "UTF-8")
source("R/strategy.R", encoding = "UTF-8")
source("ml/features.R", encoding = "UTF-8")
source("ml/splits.R", encoding = "UTF-8")
source("ml/preprocess.R", encoding = "UTF-8")
source("ml/metrics.R", encoding = "UTF-8")
source("ml/event_data.R", encoding = "UTF-8")
source("ml/event_models.R", encoding = "UTF-8")
source("ml/event_walk_forward.R", encoding = "UTF-8")
source("ml/event_regression.R", encoding = "UTF-8")
source("ml/candidate_events.R", encoding = "UTF-8")
source("ml/candidate_regression.R", encoding = "UTF-8")
source("ml/daily_transfer.R", encoding = "UTF-8")
source("ml/daily_multiobjective.R", encoding = "UTF-8")
market <- load_market()$full
ml <- build_ml_table(market)
ml$data <- assign_ml_split(ml$data)
attr(ml$data, "ml_feature_names") <- ml$feature_names
candidate <- build_candidate_event_table(ml$data)
event <- candidate$data
daily <- build_daily_economic_table(ml$data)
folds <- purged_event_walk_forward_folds(
  event, min_train = 50L, validation_size = 15L, test_size = 10L,
  step = 10L, train_embargo_sessions = 10L, test_gap_sessions = 10L
)
outputs <- run_daily_multiobjective(
  daily, event, folds, risk_weights = c(0, 0.25, 0.5, 1),
  weight_half_life = 1260
)
dir.create("ml/results/daily_multiobjective", showWarnings = FALSE, recursive = TRUE)
summaries <- do.call(rbind, lapply(names(outputs), function(n) outputs[[n]]$summary))
write.csv(summaries, "ml/results/daily_multiobjective/summaries.csv", row.names = FALSE)
for (n in names(outputs)) {
  write.csv(outputs[[n]]$fold_metrics, paste0("ml/results/daily_multiobjective/", n, "_fold_metrics.csv"), row.names = FALSE)
  write.csv(outputs[[n]]$predictions, paste0("ml/results/daily_multiobjective/", n, "_predictions.csv"), row.names = FALSE)
}
print(summaries)

