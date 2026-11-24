#!/usr/bin/env Rscript
source("ml/metrics.R", encoding = "UTF-8")
source("ml/event_regression.R", encoding = "UTF-8")

result_dir <- "strategy/fixed/results"
folds <- read.csv(file.path(result_dir, "tchr_ml_overlay_folds.csv"), stringsAsFactors = FALSE)
predictions <- read.csv(file.path(result_dir, "tchr_ml_overlay_predictions.csv"), stringsAsFactors = FALSE)
portfolio <- read.csv(file.path(result_dir, "tchr_ml_overlay_portfolio.csv"), stringsAsFactors = FALSE)
trade_uplift <- folds$uplift_trade_net_bp
ci <- bootstrap_mean_ci(trade_uplift, seed = 20250530L)

fold_ids <- unique(predictions$fold)
iterations <- 10000L
observed <- numeric(length(fold_ids))
random <- matrix(0, iterations, length(fold_ids))
for (j in seq_along(fold_ids)) {
  z <- predictions[predictions$fold == fold_ids[j], , drop = FALSE]
  selected <- as.logical(z$selected)
  selected[is.na(selected)] <- FALSE
  k <- sum(selected)
  y <- z$trade_net
  observed[j] <- if (k) (mean(y[selected]) - mean(y)) * 10000 else 0
  set.seed(20250530L + j)
  if (k > 0L && k < nrow(z)) {
    for (i in seq_len(iterations)) {
      pick <- sample.int(nrow(z), k)
      random[i, j] <- (mean(y[pick]) - mean(y)) * 10000
    }
  }
}
random_mean <- rowMeans(random)
random_p <- (sum(random_mean >= mean(observed)) + 1) / (iterations + 1)
base <- portfolio[portfolio$rule == "TCHR", , drop = FALSE]
overlay <- portfolio[portfolio$rule == "TCHR + ML overlay", , drop = FALSE]
checks <- data.frame(
  check = c("test_events_at_least_30",
            "positive_trade_uplift_rate_at_least_75pct",
            "trade_uplift_median_positive",
            "trade_uplift_bootstrap_lower_above_zero",
            "random_trade_baseline_p_below_0_05",
            "portfolio_max_drawdown_not_worse"),
  value = c(
    sum(folds$events) >= 30L,
    mean(trade_uplift > 0) >= 0.75,
    median(trade_uplift) > 0,
    ci[["lower"]] > 0,
    random_p < 0.05,
    overlay$max_drawdown[1L] >= base$max_drawdown[1L]
  ),
  stringsAsFactors = FALSE
)
audit <- data.frame(
  run = "TCHR + transferred ML overlay",
  stable_gain = all(checks$value),
  folds = nrow(folds),
  test_events = sum(folds$events),
  mean_trade_uplift_bp = mean(trade_uplift),
  median_trade_uplift_bp = median(trade_uplift),
  positive_trade_uplift_rate = mean(trade_uplift > 0),
  trade_uplift_ci_low_bp = ci[["lower"]],
  random_trade_p = random_p,
  base_compound = base$compound[1L],
  overlay_compound = overlay$compound[1L],
  base_max_drawdown = base$max_drawdown[1L],
  overlay_max_drawdown = overlay$max_drawdown[1L],
  base_sharpe = base$sharpe[1L],
  overlay_sharpe = overlay$sharpe[1L],
  stringsAsFactors = FALSE
)
dir.create("ml/results/tchr_integration/final", showWarnings = FALSE, recursive = TRUE)
write.csv(checks, "ml/results/tchr_integration/final/overlay_stability_checks.csv", row.names = FALSE)
write.csv(audit, "ml/results/tchr_integration/final/overlay_stability_audit.csv", row.names = FALSE)
print(folds); print(checks); print(audit)
if (!isTRUE(audit$stable_gain)) stop("TCHR overlay stability gate failed.")
