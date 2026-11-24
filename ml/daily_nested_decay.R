inner_decay_selection <- function(daily,
                                  event,
                                  fold,
                                  used_features,
                                  half_lives,
                                  horizon = "5d",
                                  seed = 20250530L) {
  train_events <- fold$train
  inner_size <- min(15L, max(10L, floor(length(train_events) / 4L)))
  if (length(train_events) <= inner_size + 10L) {
    return(list(half_life = half_lives[1L], diagnostics = data.frame()))
  }
  inner_validation_events <- tail(train_events, inner_size)
  inner_validation_start <- min(event$Date[inner_validation_events])
  outer_validation_start <- min(event$Date[fold$validation])
  inner_cutoff <- min(event$row_index[inner_validation_events]) - 10L
  train_rows <- which(
    seq_len(nrow(daily)) < inner_cutoff &
      daily$warmup_complete &
      is.finite(daily[[paste0("utility_", horizon)]])
  )
  validation_rows <- which(
    daily$Date >= inner_validation_start &
      daily$Date < outer_validation_start - 10L &
      daily$warmup_complete &
      is.finite(daily[[paste0("utility_", horizon)]])
  )
  if (length(train_rows) < 200L || length(validation_rows) < 100L) {
    return(list(half_life = half_lives[1L], diagnostics = data.frame()))
  }
  target <- paste0("utility_", horizon)
  diagnostics <- lapply(seq_along(half_lives), function(i) {
    half_life <- half_lives[i]
    grid_fit <- fit_daily_model_grid(
      daily, train_rows, validation_rows, used_features, target,
      seed = seed + i, weight_half_life = half_life
    )
    choices <- lapply(names(grid_fit$fits), function(specification) {
      fit <- grid_fit$fits[[specification]]
      score <- fit$predict(event$row_index[inner_validation_events])
      policy <- select_validation_policy(
        event, inner_validation_events, score, horizon
      )
      list(specification = specification, policy = policy)
    })
    objective <- vapply(
      choices,
      function(x) if (is.finite(x$policy$validation_mean_utility_bp)) {
        x$policy$validation_mean_utility_bp
      } else -Inf,
      0.0
    )
    best <- which.max(objective)
    data.frame(
      half_life = half_life,
      inner_utility_bp = objective[best],
      inner_specification = choices[[best]]$specification,
      inner_policy = choices[[best]]$policy$rule,
      stringsAsFactors = FALSE
    )
  })
  diagnostics <- do.call(rbind, diagnostics)
  best <- order(-diagnostics$inner_utility_bp, diagnostics$half_life)[1L]
  list(half_life = diagnostics$half_life[best], diagnostics = diagnostics)
}

run_daily_nested_decay_walk_forward <- function(daily,
                                                event,
                                                folds,
                                                half_lives = c(1000, 1260, 1500, 1800, 2200, 3000),
                                                horizon = "5d",
                                                feature_mode = "correlation_pruned") {
  fold_metrics <- list()
  predictions <- list()
  selections <- list()
  inner_diagnostics <- list()
  target <- paste0("utility_", horizon)
  feature_names <- ml_feature_columns(event)
  for (fold_index in seq_along(folds)) {
    fold <- folds[[fold_index]]
    used_features <- if (feature_mode == "correlation_pruned") {
      prune_event_features(event, fold$train, feature_names, correlation_threshold = 0.75)
    } else feature_names
    decay <- inner_decay_selection(
      daily, event, fold, used_features, half_lives, horizon,
      seed = 20250530L + fold_index * 100L
    )
    inner_diagnostics[[length(inner_diagnostics) + 1L]] <- data.frame(
      fold = fold$id, decay$diagnostics, stringsAsFactors = FALSE
    )
    validation_events <- fold$validation
    test_events <- fold$test
    validation_start <- min(event$Date[validation_events])
    test_start <- min(event$Date[test_events])
    train_cutoff <- min(event$row_index[validation_events]) - 10L
    train_rows <- which(
      seq_len(nrow(daily)) < train_cutoff &
        daily$warmup_complete & is.finite(daily[[target]])
    )
    validation_rows <- which(
      daily$Date >= validation_start &
        daily$Date < test_start - 10L &
        daily$warmup_complete & is.finite(daily[[target]])
    )
    grid_fit <- fit_daily_model_grid(
      daily, train_rows, validation_rows, used_features, target,
      seed = 20250530L + fold_index,
      weight_half_life = decay$half_life
    )
    choices <- lapply(names(grid_fit$fits), function(specification) {
      fit <- grid_fit$fits[[specification]]
      validation_scores <- fit$predict(event$row_index[validation_events])
      policy <- select_validation_policy(
        event, validation_events, validation_scores, horizon
      )
      value <- if (is.finite(policy$validation_mean_utility_bp)) {
        policy$validation_mean_utility_bp
      } else 0
      list(specification = specification, fit = fit,
           policy = policy, value = value)
    })
    choice <- choices[[which.max(vapply(choices, function(x) x$value, 0.0))]]
    test_scores <- choice$fit$predict(event$row_index[test_events])
    selected <- select_event_policy_for_test(test_scores, choice$policy,
                                             event = event, rows = test_events)
    metrics <- candidate_economic_metrics(
      event, test_events, test_scores, selected, horizon, fold$id,
      feature_mode, choice$specification, choice$policy
    )
    metrics$half_life <- decay$half_life
    metrics$validation_mean_utility_bp <- choice$value
    metrics$test_start <- fold$test_start
    metrics$test_end <- fold$test_end
    fold_metrics[[length(fold_metrics) + 1L]] <- metrics
    predictions[[length(predictions) + 1L]] <- data.frame(
      fold = fold$id,
      Date = event$Date[test_events],
      event_id = event$event_id[test_events],
      horizon = horizon,
      feature_mode = feature_mode,
      specification = choice$specification,
      policy = choice$policy$rule,
      policy_fraction = choice$policy$fraction,
      policy_threshold = choice$policy$threshold,
      half_life = decay$half_life,
      score = test_scores,
      selected = selected,
      net_return = event[[paste0("net_return_", horizon)]][test_events],
      excess_return = event[[paste0("excess_return_", horizon)]][test_events],
      utility = event[[paste0("utility_", horizon)]][test_events],
      stringsAsFactors = FALSE
    )
    selections[[length(selections) + 1L]] <- data.frame(
      fold = fold$id,
      half_life = decay$half_life,
      specification = choice$specification,
      policy = choice$policy$rule,
      policy_fraction = choice$policy$fraction,
      policy_threshold = choice$policy$threshold,
      validation_mean_utility_bp = choice$value,
      stringsAsFactors = FALSE
    )
  }
  fold_metrics <- do.call(rbind, fold_metrics)
  predictions <- do.call(rbind, predictions)
  selections <- do.call(rbind, selections)
  inner_diagnostics <- do.call(rbind, inner_diagnostics)
  net <- ifelse(is.finite(fold_metrics$uplift_mean_net_bp), fold_metrics$uplift_mean_net_bp, 0)
  excess <- ifelse(is.finite(fold_metrics$uplift_mean_excess_bp), fold_metrics$uplift_mean_excess_bp, 0)
  utility <- ifelse(is.finite(fold_metrics$uplift_mean_utility_bp), fold_metrics$uplift_mean_utility_bp, 0)
  net_ci <- bootstrap_mean_ci(net, seed = 20250530L)
  excess_ci <- bootstrap_mean_ci(excess, seed = 20250531L)
  utility_ci <- bootstrap_mean_ci(utility, seed = 20250532L)
  summary <- data.frame(
    folds = nrow(fold_metrics), test_events = sum(fold_metrics$events),
    mean_uplift_net_bp = mean(net), median_uplift_net_bp = median(net),
    positive_net_rate = mean(net > 0), net_ci_low_bp = net_ci[["lower"]],
    mean_uplift_excess_bp = mean(excess), median_uplift_excess_bp = median(excess),
    positive_excess_rate = mean(excess > 0), excess_ci_low_bp = excess_ci[["lower"]],
    mean_uplift_utility_bp = mean(utility), median_uplift_utility_bp = median(utility),
    positive_utility_rate = mean(utility > 0), utility_ci_low_bp = utility_ci[["lower"]],
    mean_selected_rate = mean(fold_metrics$selection_rate), stringsAsFactors = FALSE
  )
  list(fold_metrics = fold_metrics, predictions = predictions,
       selections = selections, inner_diagnostics = inner_diagnostics,
       summary = summary)
}
