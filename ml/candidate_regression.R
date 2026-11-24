candidate_parameter_grid <- function() {
  list(
    depth_1_low_penalty = list(max_depth = 1L, min_child_weight = 1, lambda = 1, alpha = 0),
    depth_2_low_penalty = list(max_depth = 2L, min_child_weight = 1, lambda = 1, alpha = 0),
    depth_3_low_penalty = list(max_depth = 3L, min_child_weight = 1, lambda = 1, alpha = 0),
    depth_2_regularized = list(max_depth = 2L, min_child_weight = 2, lambda = 5, alpha = 0.25),
    depth_2_heavy_child = list(max_depth = 2L, min_child_weight = 4, lambda = 5, alpha = 0.25),
    depth_1_regularized = list(max_depth = 1L, min_child_weight = 2, lambda = 5, alpha = 0.25)
  )
}

fit_candidate_model_grid <- function(event,
                                     train_rows,
                                     validation_rows,
                                     feature_names,
                                     target,
                                     seed = 20250530L) {
  grid <- candidate_parameter_grid()
  fits <- list()
  scores <- lapply(names(grid), function(spec_name) {
    fit <- fit_event_xgb_regression(
      data = event,
      train_rows = train_rows,
      validation_rows = validation_rows,
      feature_names = feature_names,
      target = target,
      nrounds = 300L,
      early_stopping_rounds = 40L,
      seed = seed,
      model_params = grid[[spec_name]]
    )
    y <- as.numeric(event[[target]][validation_rows])
    prediction <- fit$validation_prediction
    fits[[spec_name]] <<- fit
    data.frame(
      specification = spec_name,
      validation_rmse = sqrt(mean((prediction - y)^2)),
      validation_mae = mean(abs(prediction - y)),
      validation_rank = if (length(unique(prediction)) > 1L) {
        suppressWarnings(stats::cor(prediction, y, method = "spearman"))
      } else NA_real_,
      best_iteration = fit$best_iteration,
      stringsAsFactors = FALSE
    )
  })
  diagnostics <- do.call(rbind, scores)
  order_index <- order(
    diagnostics$validation_rmse,
    -ifelse(is.finite(diagnostics$validation_rank),
            diagnostics$validation_rank, -Inf),
    match(diagnostics$specification, names(grid))
  )
  selected <- diagnostics[order_index[1L], ]
  list(
    fit = fits[[selected$specification]],
    diagnostics = diagnostics,
    selected = selected,
    grid = grid
  )
}

select_validation_policy <- function(event,
                                     rows,
                                     scores,
                                     horizon,
                                     minimum_events = 5L) {
  utility <- expm1(event[[paste0("utility_", horizon)]][rows])
  policies <- list(
    no_trade = list(selected = rep(FALSE, length(rows)), rule = "no_trade",
                    fraction = 0, threshold = NA_real_)
  )
  for (fraction in c(0.25, 0.50, 0.75)) {
    top <- select_event_top_fraction(scores, fraction = fraction)
    policies[[paste0("top_", fraction)]] <- list(
      selected = top$selected,
      rule = "top_fraction",
      fraction = fraction,
      threshold = top$threshold
    )
  }
  positive <- is.finite(scores) & scores > 0
  if (sum(positive) >= minimum_events) {
    policies[["positive_score"]] <- list(
      selected = positive,
      rule = "positive_score",
      fraction = mean(positive),
      threshold = 0
    )
  }
  values <- vapply(policies, function(policy) {
    selected <- policy$selected
    if (sum(selected) < minimum_events) return(-Inf)
    mean(utility[selected])
  }, 0.0)
  choice <- names(which.max(values))
  selected_policy <- policies[[choice]]
  selected_policy$validation_mean_utility_bp <- if (is.finite(values[[choice]])) {
    values[[choice]] * 10000
  } else NA_real_
  selected_policy
}

candidate_economic_metrics <- function(event,
                                        rows,
                                        scores,
                                        selected,
                                        horizon,
                                        fold_id,
                                        feature_mode,
                                        specification,
                                        policy) {
  net <- expm1(event[[paste0("net_return_", horizon)]][rows])
  excess <- expm1(event[[paste0("excess_return_", horizon)]][rows])
  utility <- expm1(event[[paste0("utility_", horizon)]][rows])
  selected <- as.logical(selected)
  selected[is.na(selected)] <- FALSE
  selected_net <- net[selected]
  selected_excess <- excess[selected]
  selected_utility <- utility[selected]
  mean_rank <- if (length(unique(scores)) > 1L) {
    suppressWarnings(stats::cor(scores, utility, method = "spearman"))
  } else NA_real_
  data.frame(
    fold = fold_id,
    horizon = horizon,
    feature_mode = feature_mode,
    specification = specification,
    policy = policy$rule,
    policy_fraction = policy$fraction,
    policy_threshold = policy$threshold,
    events = length(rows),
    selected = sum(selected),
    selection_rate = mean(selected),
    baseline_mean_net_bp = mean(net) * 10000,
    selected_mean_net_bp = if (length(selected_net)) mean(selected_net) * 10000 else NA_real_,
    uplift_mean_net_bp = if (length(selected_net)) {
      (mean(selected_net) - mean(net)) * 10000
    } else NA_real_,
    baseline_mean_excess_bp = mean(excess) * 10000,
    selected_mean_excess_bp = if (length(selected_excess)) mean(selected_excess) * 10000 else NA_real_,
    uplift_mean_excess_bp = if (length(selected_excess)) {
      (mean(selected_excess) - mean(excess)) * 10000
    } else NA_real_,
    baseline_mean_utility_bp = mean(utility) * 10000,
    selected_mean_utility_bp = if (length(selected_utility)) mean(selected_utility) * 10000 else NA_real_,
    uplift_mean_utility_bp = if (length(selected_utility)) {
      (mean(selected_utility) - mean(utility)) * 10000
    } else NA_real_,
    rank_correlation_utility = mean_rank,
    validation_mean_utility_bp = policy$validation_mean_utility_bp,
    stringsAsFactors = FALSE
  )
}

run_candidate_nested_walk_forward <- function(event,
                                              folds,
                                              horizon,
                                              feature_names,
                                              feature_mode = c("fixed", "correlation_pruned"),
                                              target_type = "utility",
                                              evaluation_subset = c("all", "original")) {
  feature_mode <- match.arg(feature_mode)
  evaluation_subset <- match.arg(evaluation_subset)
  target <- paste0(target_type, "_", horizon)
  fold_metrics <- list()
  predictions <- list()
  selections <- list()
  models <- list()

  for (fold_index in seq_along(folds)) {
    fold <- folds[[fold_index]]
    used_features <- if (feature_mode == "correlation_pruned") {
      prune_event_features(event, fold$train, feature_names, correlation_threshold = 0.75)
    } else feature_names
    validation_rows <- if (evaluation_subset == "original") {
      fold$validation[event$original_tchr_setup[fold$validation]]
    } else fold$validation
    test_rows <- if (evaluation_subset == "original") {
      fold$test[event$original_tchr_setup[fold$test]]
    } else fold$test
    if (length(validation_rows) < 5L || length(test_rows) < 3L) next
    grid_fit <- fit_candidate_model_grid(
      event = event,
      train_rows = fold$train,
      validation_rows = validation_rows,
      feature_names = used_features,
      target = target,
      seed = 20250530L + fold_index
    )
    validation_scores <- grid_fit$fit$validation_prediction
    policy <- select_validation_policy(
      event = event,
      rows = validation_rows,
      scores = validation_scores,
      horizon = horizon
    )
    test_scores <- grid_fit$fit$predict(test_rows)
    metrics <- candidate_economic_metrics(
      event = event,
      rows = test_rows,
      scores = test_scores,
      selected = select_event_policy_for_test(test_scores, policy, event = event, rows = test_rows),
      horizon = horizon,
      fold_id = fold$id,
      feature_mode = feature_mode,
      specification = grid_fit$selected$specification,
      policy = policy
    )
    metrics$best_iteration <- grid_fit$fit$best_iteration
    metrics$validation_rmse <- grid_fit$selected$validation_rmse
    metrics$validation_mae <- grid_fit$selected$validation_mae
    metrics$test_start <- fold$test_start
    metrics$test_end <- fold$test_end
    metrics$evaluation_subset <- evaluation_subset
    fold_metrics[[length(fold_metrics) + 1L]] <- metrics

    test_selected <- select_event_policy_for_test(test_scores, policy, event = event, rows = test_rows)
    predictions[[length(predictions) + 1L]] <- data.frame(
      fold = fold$id,
      Date = event$Date[test_rows],
      event_id = event$event_id[test_rows],
      horizon = horizon,
      feature_mode = feature_mode,
      target = target,
      specification = grid_fit$selected$specification,
      policy = policy$rule,
      policy_fraction = policy$fraction,
      policy_threshold = policy$threshold,
      score = test_scores,
      selected = test_selected,
      evaluation_subset = evaluation_subset,
      net_return = event[[paste0("net_return_", horizon)]][test_rows],
      excess_return = event[[paste0("excess_return_", horizon)]][test_rows],
      utility = event[[paste0("utility_", horizon)]][test_rows],
      stringsAsFactors = FALSE
    )
    selections[[length(selections) + 1L]] <- data.frame(
      fold = fold$id,
      horizon = horizon,
      feature_mode = feature_mode,
      evaluation_subset = evaluation_subset,
      target = target,
      specification = grid_fit$selected$specification,
      policy = policy$rule,
      policy_fraction = policy$fraction,
      policy_threshold = policy$threshold,
      validation_mean_utility_bp = policy$validation_mean_utility_bp,
      feature_count = length(used_features),
      features = paste(used_features, collapse = "|"),
      stringsAsFactors = FALSE
    )
    models[[fold$id]] <- list(
      fit = grid_fit$fit,
      grid = grid_fit$diagnostics,
      selected = grid_fit$selected,
      features = used_features
    )
  }

  fold_metrics <- do.call(rbind, fold_metrics)
  predictions <- do.call(rbind, predictions)
  selections <- do.call(rbind, selections)
  net_ci <- bootstrap_mean_ci(fold_metrics$uplift_mean_net_bp, seed = 20250530L)
  excess_ci <- bootstrap_mean_ci(fold_metrics$uplift_mean_excess_bp, seed = 20250531L)
  utility_ci <- bootstrap_mean_ci(fold_metrics$uplift_mean_utility_bp, seed = 20250532L)
  mean_net <- mean(fold_metrics$uplift_mean_net_bp, na.rm = TRUE)
  median_net <- median(fold_metrics$uplift_mean_net_bp, na.rm = TRUE)
  positive_net <- mean(fold_metrics$uplift_mean_net_bp > 0, na.rm = TRUE)
  mean_excess <- mean(fold_metrics$uplift_mean_excess_bp, na.rm = TRUE)
  median_excess <- median(fold_metrics$uplift_mean_excess_bp, na.rm = TRUE)
  positive_excess <- mean(fold_metrics$uplift_mean_excess_bp > 0, na.rm = TRUE)
  mean_utility <- mean(fold_metrics$uplift_mean_utility_bp, na.rm = TRUE)
  median_utility <- median(fold_metrics$uplift_mean_utility_bp, na.rm = TRUE)
  positive_utility <- mean(fold_metrics$uplift_mean_utility_bp > 0, na.rm = TRUE)
  summary <- data.frame(
    horizon = horizon,
    feature_mode = feature_mode,
    evaluation_subset = evaluation_subset,
    target = target,
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
    stable_gain = isTRUE(
      sum(fold_metrics$events, na.rm = TRUE) >= 40L &&
        positive_net >= 0.75 &&
        median_net > 0 &&
        net_ci[["lower"]] > 0 &&
        positive_excess >= 0.75 &&
        median_excess > 0 &&
        excess_ci[["lower"]] > 0 &&
        positive_utility >= 0.75 &&
        median_utility > 0 &&
        utility_ci[["lower"]] > 0
    ),
    stringsAsFactors = FALSE
  )
  list(
    folds = folds,
    fold_metrics = fold_metrics,
    predictions = predictions,
    selections = selections,
    summary = summary,
    models = models
  )
}

select_event_policy_for_test <- function(scores, policy) {
  if (policy$rule == "no_trade") return(rep(FALSE, length(scores)))
  if (policy$rule == "positive_score") return(is.finite(scores) & scores > 0)
  is.finite(scores) & scores >= policy$threshold
}





