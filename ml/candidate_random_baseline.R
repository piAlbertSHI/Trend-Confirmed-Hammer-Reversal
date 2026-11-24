run_candidate_random_baseline <- function(predictions,
                                           iterations = 10000L,
                                           seed = 20250530L) {
  runs <- unique(predictions$run)
  fold_rows <- list()
  summary_rows <- list()
  for (run_index in seq_along(runs)) {
    run_id <- runs[run_index]
    run_data <- predictions[predictions$run == run_id, , drop = FALSE]
    folds <- unique(run_data$fold)
    observed_net <- numeric(length(folds))
    observed_excess <- numeric(length(folds))
    observed_utility <- numeric(length(folds))
    random_net <- matrix(NA_real_, iterations, length(folds))
    random_excess <- matrix(NA_real_, iterations, length(folds))
    random_utility <- matrix(NA_real_, iterations, length(folds))
    for (j in seq_along(folds)) {
      fold_data <- run_data[run_data$fold == folds[j], , drop = FALSE]
      selected <- as.logical(fold_data$selected)
      selected[is.na(selected)] <- FALSE
      selected_count <- sum(selected)
      net <- expm1(fold_data$net_return)
      excess <- expm1(fold_data$excess_return)
      utility <- expm1(fold_data$utility)
      observed_net[j] <- if (selected_count) (mean(net[selected]) - mean(net)) * 10000 else 0
      observed_excess[j] <- if (selected_count) {
        (mean(excess[selected]) - mean(excess)) * 10000
      } else 0
      observed_utility[j] <- if (selected_count) {
        (mean(utility[selected]) - mean(utility)) * 10000
      } else 0
      set.seed(seed + run_index * 100L + j)
      if (selected_count <= 0L || selected_count >= nrow(fold_data)) {
        random_net[, j] <- 0
        random_excess[, j] <- 0
        random_utility[, j] <- 0
      } else {
        for (i in seq_len(iterations)) {
          chosen <- sample.int(nrow(fold_data), selected_count, replace = FALSE)
          random_net[i, j] <- (mean(net[chosen]) - mean(net)) * 10000
          random_excess[i, j] <- (mean(excess[chosen]) - mean(excess)) * 10000
          random_utility[i, j] <- (mean(utility[chosen]) - mean(utility)) * 10000
        }
      }
      fold_rows[[length(fold_rows) + 1L]] <- data.frame(
        run = run_id,
        fold = folds[j],
        events = nrow(fold_data),
        selected = selected_count,
        observed_uplift_net_bp = observed_net[j],
        observed_uplift_excess_bp = observed_excess[j],
        observed_uplift_utility_bp = observed_utility[j],
        random_lower_uplift_net_bp = as.numeric(stats::quantile(random_net[, j], 0.025)),
        random_upper_uplift_net_bp = as.numeric(stats::quantile(random_net[, j], 0.975)),
        random_p_net = (sum(random_net[, j] >= observed_net[j]) + 1) / (iterations + 1),
        random_lower_uplift_excess_bp = as.numeric(stats::quantile(random_excess[, j], 0.025)),
        random_upper_uplift_excess_bp = as.numeric(stats::quantile(random_excess[, j], 0.975)),
        random_p_excess = (sum(random_excess[, j] >= observed_excess[j]) + 1) / (iterations + 1),
        random_lower_uplift_utility_bp = as.numeric(stats::quantile(random_utility[, j], 0.025)),
        random_upper_uplift_utility_bp = as.numeric(stats::quantile(random_utility[, j], 0.975)),
        random_p_utility = (sum(random_utility[, j] >= observed_utility[j]) + 1) / (iterations + 1),
        stringsAsFactors = FALSE
      )
    }
    aggregate_net <- rowMeans(random_net)
    aggregate_excess <- rowMeans(random_excess)
    aggregate_utility <- rowMeans(random_utility)
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      run = run_id,
      folds = length(folds),
      test_events = nrow(run_data),
      observed_mean_uplift_net_bp = mean(observed_net),
      random_lower_uplift_net_bp = as.numeric(stats::quantile(aggregate_net, 0.025)),
      random_upper_uplift_net_bp = as.numeric(stats::quantile(aggregate_net, 0.975)),
      random_p_net = (sum(aggregate_net >= mean(observed_net)) + 1) / (iterations + 1),
      observed_mean_uplift_excess_bp = mean(observed_excess),
      random_lower_uplift_excess_bp = as.numeric(stats::quantile(aggregate_excess, 0.025)),
      random_upper_uplift_excess_bp = as.numeric(stats::quantile(aggregate_excess, 0.975)),
      random_p_excess = (sum(aggregate_excess >= mean(observed_excess)) + 1) / (iterations + 1),
      observed_mean_uplift_utility_bp = mean(observed_utility),
      random_lower_uplift_utility_bp = as.numeric(stats::quantile(aggregate_utility, 0.025)),
      random_upper_uplift_utility_bp = as.numeric(stats::quantile(aggregate_utility, 0.975)),
      random_p_utility = (sum(aggregate_utility >= mean(observed_utility)) + 1) / (iterations + 1),
      stringsAsFactors = FALSE
    )
  }
  list(
    fold_metrics = do.call(rbind, fold_rows),
    summaries = do.call(rbind, summary_rows)
  )
}

