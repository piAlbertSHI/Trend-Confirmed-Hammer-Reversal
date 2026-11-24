risk_policy_select <- function(event,
                                rows,
                                utility_scores,
                                adverse_scores,
                                horizon,
                                minimum_events = 5L) {
  utility <- expm1(event[[paste0("utility_", horizon)]][rows])
  policies <- list(no_trade = list(
    selected = rep(FALSE, length(rows)),
    rule = "no_trade",
    fraction = 0,
    utility_threshold = NA_real_,
    risk_threshold = NA_real_
  ))
  risk_thresholds <- c(
    all = -Inf,
    lower_quartile = as.numeric(stats::quantile(adverse_scores, 0.25, na.rm = TRUE)),
    median = stats::median(adverse_scores, na.rm = TRUE),
    upper_quartile = as.numeric(stats::quantile(adverse_scores, 0.75, na.rm = TRUE))
  )
  for (risk_name in names(risk_thresholds)) {
    for (fraction in c(0.25, 0.50, 0.75)) {
      top <- select_event_top_fraction(utility_scores, fraction = fraction)
      selected <- top$selected &
        is.finite(adverse_scores) &
        adverse_scores >= risk_thresholds[[risk_name]]
      key <- paste("top", fraction, risk_name, sep = "__")
      policies[[key]] <- list(
        selected = selected,
        rule = key,
        fraction = fraction,
        utility_threshold = top$threshold,
        risk_threshold = risk_thresholds[[risk_name]]
      )
    }
  }
  values <- vapply(policies, function(policy) {
    selected <- policy$selected
    if (sum(selected) < minimum_events) return(-Inf)
    mean(utility[selected])
  }, 0.0)
  choice <- names(which.max(values))
  policy <- policies[[choice]]
  policy$validation_mean_utility_bp <- if (is.finite(values[[choice]])) {
    values[[choice]] * 10000
  } else NA_real_
  policy
}

run_daily_risk_walk_forward <- function(daily,
                                         event,
                                         folds,
                                         feature_mode = "correlation_pruned",
                                         evaluation_subset = "all",
                                         weight_half_life = 1260) {
  feature_names <- ml_feature_columns(event)
  fold_metrics <- list()
  predictions <- list()
  selections <- list()
  models <- list()
  horizon <- "5d"
  utility_target <- "utility_5d"
  adverse_target <- "adverse_5d"

  for (fold_index in seq_along(folds)) {
    fold <- folds[[fold_index]]
    used_features <- if (feature_mode == "correlation_pruned") {
      prune_event_features(event, fold$train, feature_names, correlation_threshold = 0.75)
    } else feature_names
    validation_events <- if (evaluation_subset == "original") {
      fold$validation[event$original_tchr_setup[fold$validation]]
    } else fold$validation
    test_events <- if (evaluation_subset == "original") {
      fold$test[event$original_tchr_setup[fold$test]]
    } else fold$test
    if (length(validation_events) < 5L || length(test_events) < 3L) next

    validation_start <- min(event$Date[fold$validation])
    test_start <- min(event$Date[fold$test])
    train_cutoff <- min(event$row_index[fold$validation]) - 10L
    train_daily <- which(
      seq_len(nrow(daily)) < train_cutoff &
        daily$warmup_complete &
        is.finite(daily[[utility_target]]) &
        is.finite(daily[[adverse_target]])
    )
    validation_daily <- which(
      daily$Date >= validation_start &
        daily$Date < test_start - 10L &
        daily$warmup_complete &
        is.finite(daily[[utility_target]]) &
        is.finite(daily[[adverse_target]])
    )
    if (length(train_daily) < 200L || length(validation_daily) < 100L) next

    utility_fit <- fit_daily_model_grid(
      daily, train_daily, validation_daily, used_features, utility_target,
      seed = 20250530L + fold_index, weight_half_life = weight_half_life
    )
    adverse_fit <- fit_daily_model_grid(
      daily, train_daily, validation_daily, used_features, adverse_target,
      seed = 20350530L + fold_index, weight_half_life = weight_half_life
    )
    validation_utility <- utility_fit$fit$predict(event$row_index[validation_events])
    validation_adverse <- adverse_fit$fit$predict(event$row_index[validation_events])
    policy <- risk_policy_select(
      event, validation_events, validation_utility, validation_adverse, horizon
    )
    test_utility <- utility_fit$fit$predict(event$row_index[test_events])
    test_adverse <- adverse_fit$fit$predict(event$row_index[test_events])
    selected <- if (policy$rule == "no_trade") {
      rep(FALSE, length(test_events))
    } else {
      is.finite(test_utility) &
        test_utility >= policy$utility_threshold &
        is.finite(test_adverse) &
        test_adverse >= policy$risk_threshold
    }

    net <- expm1(event$net_return_5d[test_events])
    excess <- expm1(event$excess_return_5d[test_events])
    utility <- expm1(event$utility_5d[test_events])
    adverse <- expm1(event$adverse_5d[test_events])
    selected_net <- net[selected]
    selected_excess <- excess[selected]
    selected_utility <- utility[selected]
    selected_adverse <- adverse[selected]
    fold_metrics[[length(fold_metrics) + 1L]] <- data.frame(
      fold = fold$id,
      events = length(test_events),
      selected = sum(selected),
      selection_rate = mean(selected),
      baseline_mean_net_bp = mean(net) * 10000,
      selected_mean_net_bp = if (length(selected_net)) mean(selected_net) * 10000 else NA_real_,
      uplift_mean_net_bp = if (length(selected_net)) (mean(selected_net) - mean(net)) * 10000 else NA_real_,
      baseline_mean_excess_bp = mean(excess) * 10000,
      selected_mean_excess_bp = if (length(selected_excess)) mean(selected_excess) * 10000 else NA_real_,
      uplift_mean_excess_bp = if (length(selected_excess)) (mean(selected_excess) - mean(excess)) * 10000 else NA_real_,
      baseline_mean_utility_bp = mean(utility) * 10000,
      selected_mean_utility_bp = if (length(selected_utility)) mean(selected_utility) * 10000 else NA_real_,
      uplift_mean_utility_bp = if (length(selected_utility)) (mean(selected_utility) - mean(utility)) * 10000 else NA_real_,
      baseline_mean_adverse_bp = mean(adverse) * 10000,
      selected_mean_adverse_bp = if (length(selected_adverse)) mean(selected_adverse) * 10000 else NA_real_,
      test_start = fold$test_start,
      test_end = fold$test_end,
      policy = policy$rule,
      validation_mean_utility_bp = policy$validation_mean_utility_bp,
      stringsAsFactors = FALSE
    )
    predictions[[length(predictions) + 1L]] <- data.frame(
      fold = fold$id,
      Date = event$Date[test_events],
      event_id = event$event_id[test_events],
      utility_score = test_utility,
      adverse_score = test_adverse,
      selected = selected,
      net_return = event$net_return_5d[test_events],
      excess_return = event$excess_return_5d[test_events],
      utility = event$utility_5d[test_events],
      adverse = event$adverse_5d[test_events],
      stringsAsFactors = FALSE
    )
    selections[[length(selections) + 1L]] <- data.frame(
      fold = fold$id,
      policy = policy$rule,
      utility_threshold = policy$utility_threshold,
      risk_threshold = policy$risk_threshold,
      validation_mean_utility_bp = policy$validation_mean_utility_bp,
      utility_specification = utility_fit$selected$specification,
      adverse_specification = adverse_fit$selected$specification,
      feature_count = length(used_features),
      features = paste(used_features, collapse = "|"),
      stringsAsFactors = FALSE
    )
    models[[fold$id]] <- list(utility = utility_fit, adverse = adverse_fit)
  }

  fold_metrics <- do.call(rbind, fold_metrics)
  predictions <- do.call(rbind, predictions)
  selections <- do.call(rbind, selections)
  net_uplift <- ifelse(is.finite(fold_metrics$uplift_mean_net_bp), fold_metrics$uplift_mean_net_bp, 0)
  excess_uplift <- ifelse(is.finite(fold_metrics$uplift_mean_excess_bp), fold_metrics$uplift_mean_excess_bp, 0)
  utility_uplift <- ifelse(is.finite(fold_metrics$uplift_mean_utility_bp), fold_metrics$uplift_mean_utility_bp, 0)
  adverse_change <- ifelse(
    is.finite(fold_metrics$selected_mean_adverse_bp - fold_metrics$baseline_mean_adverse_bp),
    fold_metrics$selected_mean_adverse_bp - fold_metrics$baseline_mean_adverse_bp,
    0
  )
  net_ci <- bootstrap_mean_ci(net_uplift, seed = 20250530L)
  excess_ci <- bootstrap_mean_ci(excess_uplift, seed = 20250531L)
  utility_ci <- bootstrap_mean_ci(utility_uplift, seed = 20250532L)
  mean_net <- mean(net_uplift)
  median_net <- median(net_uplift)
  positive_net <- mean(net_uplift > 0)
  mean_excess <- mean(excess_uplift)
  median_excess <- median(excess_uplift)
  positive_excess <- mean(excess_uplift > 0)
  mean_utility <- mean(utility_uplift)
  median_utility <- median(utility_uplift)
  positive_utility <- mean(utility_uplift > 0)
  mean_adverse_change <- mean(adverse_change)
  summary <- data.frame(
    folds = nrow(fold_metrics),
    test_events = sum(fold_metrics$events),
    mean_uplift_net_bp = mean_net,
    median_uplift_net_bp = median_net,
    positive_net_uplift_rate = positive_net,
    net_uplift_ci_low_bp = net_ci[["lower"]],
    mean_uplift_excess_bp = mean_excess,
    median_uplift_excess_bp = median_excess,
    positive_excess_uplift_rate = positive_excess,
    excess_uplift_ci_low_bp = excess_ci[["lower"]],
    mean_uplift_utility_bp = mean_utility,
    median_uplift_utility_bp = median_utility,
    positive_utility_uplift_rate = positive_utility,
    utility_uplift_ci_low_bp = utility_ci[["lower"]],
    mean_selected_rate = mean(fold_metrics$selection_rate, na.rm = TRUE),
    mean_adverse_change_bp = mean_adverse_change,
    stable_gain = isTRUE(
      sum(fold_metrics$events) >= 40L &&
        positive_net >= 0.5 &&
        median_net > 0 &&
        net_ci[["lower"]] > 0 &&
        positive_excess >= 0.5 &&
        median_excess > 0 &&
        excess_ci[["lower"]] > 0 &&
        positive_utility >= 0.5 &&
        median_utility > 0 &&
        utility_ci[["lower"]] > 0 &&
        mean_adverse_change >= 0
    ),
    stringsAsFactors = FALSE
  )
  list(fold_metrics = fold_metrics, predictions = predictions,
       selections = selections, summary = summary, models = models)
}






