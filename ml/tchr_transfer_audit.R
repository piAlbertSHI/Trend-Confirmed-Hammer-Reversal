source("ml/metrics.R", encoding = "UTF-8")
source("ml/event_regression.R", encoding = "UTF-8")

root <- "ml/results/tchr_integration/transfer"
f <- read.csv(file.path(root, "fold_metrics.csv"), stringsAsFactors = FALSE)
p <- read.csv(file.path(root, "predictions.csv"), stringsAsFactors = FALSE)
if (!nrow(f) || !nrow(p)) stop("TCHR transfer output is empty.")

trade_uplift <- f$uplift_trade_net_bp
ci <- bootstrap_mean_ci(trade_uplift, seed = 20250530L)
iterations <- 10000L
random_mean <- numeric(iterations)

# Permutation null at the same selected count within each fold.
folds <- unique(p$fold)
observed <- numeric(length(folds)); random <- matrix(0, iterations, length(folds))
for (j in seq_along(folds)) {
  z <- p[p$fold == folds[j], , drop = FALSE]
  selected <- as.logical(z$selected); selected[is.na(selected)] <- FALSE
  k <- sum(selected); y <- z$trade_net
  observed[j] <- if (k) (mean(y[selected]) - mean(y)) * 10000 else 0
  set.seed(20250530L + j)
  if (k > 0L && k < nrow(z)) {
    for (i in seq_len(iterations)) {
      pick <- sample.int(nrow(z), k)
      random[i, j] <- (mean(y[pick]) - mean(y)) * 10000
    }
  }
}
aggregate_random <- rowMeans(random)
random_p <- (sum(aggregate_random >= mean(observed)) + 1) / (iterations + 1)
checks <- data.frame(
  check = c("test_events_at_least_30", "positive_trade_uplift_rate_at_least_75pct",
            "trade_uplift_median_positive", "trade_uplift_bootstrap_lower_above_zero",
            "random_trade_baseline_p_below_0_05"),
  value = c(sum(f$events) >= 30L, mean(trade_uplift > 0) >= .75,
            median(trade_uplift) > 0, ci[["lower"]] > 0, random_p < .05),
  stringsAsFactors = FALSE
)
dir.create("ml/results/tchr_integration/final", showWarnings = FALSE, recursive = TRUE)
write.csv(checks, "ml/results/tchr_integration/final/stability_checks.csv", row.names = FALSE)
write.csv(data.frame(folds = nrow(f), test_events = sum(f$events),
                     mean_trade_uplift_bp = mean(trade_uplift),
                     median_trade_uplift_bp = median(trade_uplift),
                     positive_trade_uplift_rate = mean(trade_uplift > 0),
                     trade_uplift_ci_low_bp = ci[["lower"]],
                     random_trade_p = random_p,
                     stable_gain = all(checks$value)),
          "ml/results/tchr_integration/final/stability_audit.csv", row.names = FALSE)
print(f); print(checks); print(random_p); print(ci)
if (!all(checks$value)) stop("TCHR transfer stability gate failed.")
