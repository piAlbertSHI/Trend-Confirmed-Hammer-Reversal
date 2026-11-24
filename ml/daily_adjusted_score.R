standardize_with_reference <- function(x, reference) {
  spread <- stats::sd(reference, na.rm = TRUE)
  if (!is.finite(spread) || spread < 1e-8) spread <- 1
  (x - mean(reference, na.rm = TRUE)) / spread
}

run_daily_adjusted_walk_forward <- function(daily,
                                            event,
                                            folds,
                                            half_life = 1500,
                                            alphas = c(-0.5, 0, 0.5, 1),
                                            feature_mode = "correlation_pruned") {
  fold_metrics <- list()
  predictions <- list()
  selections <- list()
  for (fold_index in seq_along(folds)) {
    fold <- folds[[fold_index]]
    used_features <- prune_event_features(
      event, fold$train, ml_feature_columns(event), correlation_threshold = 0.75
    )
    validation_events <- fold$validation
    test_events <- fold$test
    validation_start <- min(event$Date[fold$validation])
    test_start <- min(event$Date[fold$test])
    train_cutoff <- min(event$row_index[fold$validation]) - 10L
    train_daily <- which(
      seq_len(nrow(daily)) < train_cutoff &
        daily$warmup_complete &
        is.finite(daily$utility_5d)
    )
    validation_daily <- which(
      daily$Date >= validation_start &
        daily$Date < test_start - 10L &
        daily$warmup_complete &
        is.finite(daily$utility_5d)
    )
    grid_fit <- fit_daily_model_grid(
      daily, train_daily, validation_daily, used_features, "utility_5d",
      seed = 20250530L + fold_index, weight_half_life = half_life
    )
    trend_validation <- event$sma100_slope20[validation_events]
    choices <- list()
    for (specification in names(grid_fit$fits)) {
      fit <- grid_fit$fits[[specification]]
      raw_validation <- fit$predict(event$row_index[validation_events])
      raw_test <- fit$predict(event$row_index[test_events])
      for (alpha in alphas) {
        adjusted_validation <- standardize_with_reference(raw_validation, raw_validation) +
          alpha * standardize_with_reference(trend_validation, trend_validation)
        policy <- select_validation_policy(
          event, validation_events, adjusted_validation, "5d"
        )
        value <- if (is.finite(policy$validation_mean_utility_bp)) {
          policy$validation_mean_utility_bp
        } else 0
        choices[[length(choices) + 1L]] <- list(
          specification = specification,
          fit = fit,
          raw_test = raw_test,
          alpha = alpha,
          policy = policy,
          score = value,
          raw_validation = raw_validation
        )
      }
    }
    scores <- vapply(choices, function(choice) choice$score, 0.0)
    selected_choice <- choices[[which.max(scores)]]
    trend_test <- event$sma100_slope20[test_events]
    adjusted_test <- standardize_with_reference(
      selected_choice$raw_test, selected_choice$raw_validation
    ) + selected_choice$alpha * standardize_with_reference(
      trend_test, trend_validation
    )
    selected <- select_event_policy_for_test(
      adjusted_test,
      selected_choice$policy,
      event = event,
      rows = test_events
    )
    metrics <- candidate_economic_metrics(
      event, test_events, adjusted_test, selected, "5d", fold$id,
      feature_mode, selected_choice$specification, selected_choice$policy
    )
    metrics$alpha <- selected_choice$alpha
    metrics$validation_objective <- selected_choice$score
    metrics$test_start <- fold$test_start
    metrics$test_end <- fold$test_end
    fold_metrics[[length(fold_metrics) + 1L]] <- metrics
    predictions[[length(predictions) + 1L]] <- data.frame(
      fold = fold$id,
      Date = event$Date[test_events],
      event_id = event$event_id[test_events],
      score = adjusted_test,
      selected = selected,
      alpha = selected_choice$alpha,
      net_return = event$net_return_5d[test_events],
      excess_return = event$excess_return_5d[test_events],
      utility = event$utility_5d[test_events],
      stringsAsFactors = FALSE
    )
    selections[[length(selections) + 1L]] <- data.frame(
      fold = fold$id,
      specification = selected_choice$specification,
      alpha = selected_choice$alpha,
      policy = selected_choice$policy$rule,
      policy_fraction = selected_choice$policy$fraction,
      policy_threshold = selected_choice$policy$threshold,
      validation_objective = selected_choice$score,
      stringsAsFactors = FALSE
    )
  }
  fold_metrics <- do.call(rbind, fold_metrics)
  predictions <- do.call(rbind, predictions)
  selections <- do.call(rbind, selections)
  net <- ifelse(is.finite(fold_metrics$uplift_mean_net_bp), fold_metrics$uplift_mean_net_bp, 0)
  excess <- ifelse(is.finite(fold_metrics$uplift_mean_excess_bp), fold_metrics$uplift_mean_excess_bp, 0)
  utility <- ifelse(is.finite(fold_metrics$uplift_mean_utility_bp), fold_metrics$uplift_mean_utility_bp, 0)
  net_ci <- bootstrap_mean_ci(net, seed = 20250530L)
  excess_ci <- bootstrap_mean_ci(excess, seed = 20250531L)
  utility_ci <- bootstrap_mean_ci(utility, seed = 20250532L)
  positive_net_rate <- mean(net > 0)
  positive_excess_rate <- mean(excess > 0)
  positive_utility_rate <- mean(utility > 0)
  summary <- data.frame(
    folds = nrow(fold_metrics),
    test_events = sum(fold_metrics$events),
    mean_uplift_net_bp = mean(net),
    median_uplift_net_bp = median(net),
    positive_net_rate = positive_net_rate,
    net_ci_low_bp = net_ci[["lower"]],
    mean_uplift_excess_bp = mean(excess),
    median_uplift_excess_bp = median(excess),
    positive_excess_rate = positive_excess_rate,
    excess_ci_low_bp = excess_ci[["lower"]],
    mean_uplift_utility_bp = mean(utility),
    median_uplift_utility_bp = median(utility),
    positive_utility_rate = positive_utility_rate,
    utility_ci_low_bp = utility_ci[["lower"]],
    mean_selected_rate = mean(fold_metrics$selection_rate),
    mean_alpha = mean(selections$alpha),
    stable_gain = isTRUE(
      sum(fold_metrics$events) >= 40L &&
        positive_net_rate >= 0.5 &&
        median(net) > 0 &&
        net_ci[["lower"]] > 0 &&
        positive_excess_rate >= 0.5 &&
        median(excess) > 0 &&
        excess_ci[["lower"]] > 0 &&
        positive_utility_rate >= 0.5 &&
        median(utility) > 0 &&
        utility_ci[["lower"]] > 0
    ),
    stringsAsFactors = FALSE
  )
  list(fold_metrics = fold_metrics, predictions = predictions,
       selections = selections, summary = summary)
}


