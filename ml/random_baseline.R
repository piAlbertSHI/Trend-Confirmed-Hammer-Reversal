random_selection_metrics <- function(event_rows, selected_count, iterations = 10000L) {
  n <- nrow(event_rows)
  net <- expm1(event_rows$net_return_5d)
  excess <- expm1(event_rows$excess_return_5d)
  baseline_net <- mean(net)
  baseline_excess <- mean(excess)
  if (selected_count <= 0L || selected_count >= n) {
    return(data.frame(
      random_mean_uplift_net_bp = 0,
      random_lower_uplift_net_bp = 0,
      random_upper_uplift_net_bp = 0,
      random_mean_uplift_excess_bp = 0,
      random_lower_uplift_excess_bp = 0,
      random_upper_uplift_excess_bp = 0,
      stringsAsFactors = FALSE
    ))
  }
  random_net <- numeric(iterations)
  random_excess <- numeric(iterations)
  for (i in seq_len(iterations)) {
    chosen <- sample.int(n, selected_count, replace = FALSE)
    random_net[i] <- (mean(net[chosen]) - baseline_net) * 10000
    random_excess[i] <- (mean(excess[chosen]) - baseline_excess) * 10000
  }
  data.frame(
    random_mean_uplift_net_bp = mean(random_net),
    random_lower_uplift_net_bp = as.numeric(stats::quantile(random_net, 0.025)),
    random_upper_uplift_net_bp = as.numeric(stats::quantile(random_net, 0.975)),
    random_mean_uplift_excess_bp = mean(random_excess),
    random_lower_uplift_excess_bp = as.numeric(stats::quantile(random_excess, 0.025)),
    random_upper_uplift_excess_bp = as.numeric(stats::quantile(random_excess, 0.975)),
    stringsAsFactors = FALSE
  )
}

run_random_baseline <- function(predictions,
                                iterations = 10000L,
                                seed = 20250530L) {
  runs <- unique(predictions$run)
  fold_rows <- list()
  summary_rows <- list()
  for (run_id in runs) {
    run_data <- predictions[predictions$run == run_id, , drop = FALSE]
    folds <- unique(run_data$fold)
    observed_net <- numeric(length(folds))
    observed_excess <- numeric(length(folds))
    random_net <- matrix(NA_real_, nrow = iterations, ncol = length(folds))
    random_excess <- matrix(NA_real_, nrow = iterations, ncol = length(folds))
    for (j in seq_along(folds)) {
      fold_data <- run_data[run_data$fold == folds[j], , drop = FALSE]
      selected <- as.logical(fold_data$selected)
      selected[is.na(selected)] <- FALSE
      selected_count <- sum(selected)
      net <- expm1(fold_data$net_return_5d)
      excess <- expm1(fold_data$excess_return_5d)
      observed_net[j] <- if (selected_count) {
        (mean(net[selected]) - mean(net)) * 10000
      } else 0
      observed_excess[j] <- if (selected_count) {
        (mean(excess[selected]) - mean(excess)) * 10000
      } else 0

      set.seed(seed + j + match(run_id, runs) * 100L)
      if (selected_count <= 0L || selected_count >= nrow(fold_data)) {
        random_net[, j] <- 0
        random_excess[, j] <- 0
      } else {
        for (i in seq_len(iterations)) {
          chosen <- sample.int(nrow(fold_data), selected_count, replace = FALSE)
          random_net[i, j] <- (mean(net[chosen]) - mean(net)) * 10000
          random_excess[i, j] <- (mean(excess[chosen]) - mean(excess)) * 10000
        }
      }
      fold_rows[[length(fold_rows) + 1L]] <- data.frame(
        run = run_id,
        fold = folds[j],
        events = nrow(fold_data),
        selected = selected_count,
        observed_uplift_net_bp = observed_net[j],
        observed_uplift_excess_bp = observed_excess[j],
        random_mean_uplift_net_bp = mean(random_net[, j]),
        random_lower_uplift_net_bp = as.numeric(stats::quantile(random_net[, j], 0.025)),
        random_upper_uplift_net_bp = as.numeric(stats::quantile(random_net[, j], 0.975)),
        random_mean_uplift_excess_bp = mean(random_excess[, j]),
        random_lower_uplift_excess_bp = as.numeric(stats::quantile(random_excess[, j], 0.025)),
        random_upper_uplift_excess_bp = as.numeric(stats::quantile(random_excess[, j], 0.975)),
        random_p_net = (sum(random_net[, j] >= observed_net[j]) + 1) / (iterations + 1),
        random_p_excess = (sum(random_excess[, j] >= observed_excess[j]) + 1) / (iterations + 1),
        stringsAsFactors = FALSE
      )
    }
    aggregate_net <- rowMeans(random_net)
    aggregate_excess <- rowMeans(random_excess)
    observed_mean_net <- mean(observed_net)
    observed_mean_excess <- mean(observed_excess)
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      run = run_id,
      folds = length(folds),
      test_events = nrow(run_data),
      observed_mean_uplift_net_bp = observed_mean_net,
      random_mean_uplift_net_bp = mean(aggregate_net),
      random_lower_uplift_net_bp = as.numeric(stats::quantile(aggregate_net, 0.025)),
      random_upper_uplift_net_bp = as.numeric(stats::quantile(aggregate_net, 0.975)),
      random_p_net = (sum(aggregate_net >= observed_mean_net) + 1) / (iterations + 1),
      observed_mean_uplift_excess_bp = observed_mean_excess,
      random_mean_uplift_excess_bp = mean(aggregate_excess),
      random_lower_uplift_excess_bp = as.numeric(stats::quantile(aggregate_excess, 0.025)),
      random_upper_uplift_excess_bp = as.numeric(stats::quantile(aggregate_excess, 0.975)),
      random_p_excess = (sum(aggregate_excess >= observed_mean_excess) + 1) / (iterations + 1),
      stringsAsFactors = FALSE
    )
  }
  list(
    fold_metrics = do.call(rbind, fold_rows),
    summaries = do.call(rbind, summary_rows)
  )
}

