#!/usr/bin/env Rscript
run_id <- "5d__correlation_pruned__hl_1000"
source_models <- readRDS("ml/results/daily_transfer_weighted/weighted_models.rds")
source_thresholds <- read.csv(
  "strategy/fixed/results/tchr_ml_overlay_thresholds.csv",
  stringsAsFactors = FALSE
)
models <- lapply(source_models$runs[[run_id]]$models, function(model) {
  list(
    fit = list(
      model = model$fit$model,
      recipe = model$fit$recipe,
      target_scale = model$fit$target_scale,
      feature_names = model$fit$feature_names
    ),
    features = model$features
  )
})
bundle <- list(
  run_id = run_id,
  folds = source_models$folds,
  runs = setNames(list(list(models = models)), run_id),
  thresholds = source_thresholds
)
dir.create("ml/results/tchr_integration/final", showWarnings = FALSE, recursive = TRUE)
saveRDS(bundle, "ml/results/tchr_integration/final/frozen_overlay_bundle.rds", version = 3L)
cat("Wrote frozen overlay bundle.\n")
