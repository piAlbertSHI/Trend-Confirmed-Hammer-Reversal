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
source("ml/candidate_policy_nogate.R", encoding = "UTF-8")
source("ml/daily_transfer.R", encoding = "UTF-8")
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
runs <- list()
for (horizon in c("10d", "20d")) {
  key <- paste0(horizon, "__correlation_pruned__hl_1260")
  runs[[key]] <- run_daily_transfer_walk_forward(
    daily, event, folds, horizon = horizon,
    feature_mode = "correlation_pruned",
    evaluation_subset = "all", weight_half_life = 1260
  )
}
summaries <- do.call(rbind, lapply(names(runs), function(key) {
  x <- runs[[key]]$summary
  x$run <- key
  x
}))
dir.create("ml/results/daily_transfer_weighted", showWarnings = FALSE, recursive = TRUE)
write.csv(summaries, "ml/results/daily_transfer_weighted/hl1260_horizon_summaries.csv", row.names = FALSE)
for (key in names(runs)) {
  write.csv(runs[[key]]$fold_metrics, paste0("ml/results/daily_transfer_weighted/", key, "_fold_metrics.csv"), row.names = FALSE)
  write.csv(runs[[key]]$predictions, paste0("ml/results/daily_transfer_weighted/", key, "_predictions.csv"), row.names = FALSE)
}
print(summaries)


