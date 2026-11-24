build_daily_economic_table <- function(data,
                                      horizons = c(5L, 10L, 20L)) {
  out <- data
  out$original_tchr_flag <- as.numeric(out$tchr_setup)
  feature_names <- c(ml_feature_columns(data), "original_tchr_flag")
  n <- nrow(out)
  for (horizon in horizons) {
    rows <- seq_len(n - horizon)
    target <- candidate_target_columns(out, rows, horizon)
    columns <- list(
      net = rep(NA_real_, n),
      expected = rep(NA_real_, n),
      excess = rep(NA_real_, n),
      adverse = rep(NA_real_, n),
      utility = rep(NA_real_, n)
    )
    columns$net[rows] <- target$net
    columns$expected[rows] <- target$expected_market
    columns$excess[rows] <- target$excess
    columns$adverse[rows] <- target$adverse
    columns$utility[rows] <- target$utility
    out[[paste0("net_return_", horizon, "d")]] <- columns$net
    out[[paste0("expected_market_log_", horizon, "d")]] <- columns$expected
    out[[paste0("excess_return_", horizon, "d")]] <- columns$excess
    out[[paste0("adverse_", horizon, "d")]] <- columns$adverse
    out[[paste0("utility_", horizon, "d")]] <- columns$utility
  }
  attr(out, "ml_feature_names") <- feature_names
  out
}

time_decay_weights <- function(data, rows, half_life = Inf) {
  if (!is.finite(half_life) || half_life <= 0) return(rep(1, length(rows)))
  age <- as.numeric(max(data$Date[rows]) - data$Date[rows])
  exp(-age / half_life)
}
fit_daily_model_grid <- function(daily,
                                 train_rows,
                                 validation_rows,
                                 feature_names,
                                 target,
                                 seed = 20250530L,
                                 weight_half_life = Inf) {
  grid <- candidate_parameter_grid()
  train_weights <- time_decay_weights(daily, train_rows, weight_half_life)
  validation_weights <- time_decay_weights(daily, validation_rows, weight_half_life)
  fits <- list()
  diagnostics <- lapply(names(grid), function(spec_name) {
    fit <- fit_event_xgb_regression(
      data = daily,
      train_rows = train_rows,
      validation_rows = validation_rows,
      feature_names = feature_names,
      target = target,
      nrounds = 300L,
      early_stopping_rounds = 40L,
      seed = seed,
      model_params = grid[[spec_name]],
      train_weights = train_weights,
      validation_weights = validation_weights
    )
    y <- as.numeric(daily[[target]][validation_rows])
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
  diagnostics <- do.call(rbind, diagnostics)
  choice <- order(
    diagnostics$validation_rmse,
    -ifelse(is.finite(diagnostics$validation_rank),
            diagnostics$validation_rank, -Inf),
    match(diagnostics$specification, names(grid))
  )[1L]
  selected <- diagnostics[choice, ]
  list(
    fit = fits[[selected$specification]],
    fits = fits,
    diagnostics = diagnostics,
    selected = selected,
    grid = grid
  )
}

run_daily_transfer_walk_forward <- function(daily,
                                             event,
                                             folds,
                                             horizon = "5d",
                                             feature_mode = c("fixed", "correlation_pruned"),
                                             evaluation_subset = c("all", "original"),
                                             weight_half_life = Inf) {
  feature_mode <- match.arg(feature_mode)
  evaluation_subset <- match.arg(evaluation_subset)
  target <- paste0("utility_", horizon)
  feature_names <- ml_feature_columns(event)
  fold_metrics <- list()
  predictions <- list()
  selections <- list()
  models <- list()

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
        is.finite(daily[[target]])
    )
    validation_daily <- which(
      daily$Date >= validation_start &
        daily$Date < test_start - 10L &
        daily$warmup_complete &
        is.finite(daily[[target]])
    )
    if (length(train_daily) < 200L || length(validation_daily) < 100L) next

    grid_fit <- fit_daily_model_grid(
      daily = daily,
      train_rows = train_daily,
      validation_rows = validation_daily,
      feature_names = used_features,
      target = target,
      seed = 20250530L + fold_index,
      weight_half_life = weight_half_life
    )
    economic_choices <- lapply(names(grid_fit$fits), function(specification) {
      fit <- grid_fit$fits[[specification]]
      validation_scores <- fit$predict(event$row_index[validation_events])
      policy <- select_validation_policy(
        event = event,
        rows = validation_events,
        scores = validation_scores,
        horizon = horizon
      )
      score <- if (is.finite(policy$validation_mean_utility_bp)) {
        policy$validation_mean_utility_bp
      } else 0
      list(specification = specification, fit = fit,
           validation_scores = validation_scores, policy = policy,
           economic_score = score)
    })
    economic_scores <- vapply(economic_choices, function(choice) choice$economic_score, 0.0)
    choice_index <- which.max(economic_scores)
    economic_choice <- economic_choices[[choice_index]]
    grid_fit$fit <- economic_choice$fit
    grid_fit$selected <- grid_fit$diagnostics[
      match(economic_choice$specification, grid_fit$diagnostics$specification), ,
      drop = FALSE
    ]
    grid_fit$selected$validation_economic_score <- economic_choice$economic_score
    validation_scores <- economic_choice$validation_scores
    policy <- economic_choice$policy
    test_scores <- grid_fit$fit$predict(event$row_index[test_events])
    test_selected <- select_event_policy_for_test(test_scores, policy, event = event, rows = test_events)
    metrics <- candidate_economic_metrics(
      event = event,
      rows = test_events,
      scores = test_scores,
      selected = test_selected,
      horizon = horizon,
      fold_id = fold$id,
      feature_mode = feature_mode,
      specification = grid_fit$selected$specification,
      policy = policy
    )
    metrics$evaluation_subset <- evaluation_subset
    metrics$weight_half_life <- weight_half_life
    metrics$daily_train_rows <- length(train_daily)
    metrics$daily_validation_rows <- length(validation_daily)
    metrics$validation_rmse <- grid_fit$selected$validation_rmse
    metrics$best_iteration <- grid_fit$fit$best_iteration
    metrics$test_start <- fold$test_start
    metrics$test_end <- fold$test_end
    fold_metrics[[length(fold_metrics) + 1L]] <- metrics

    predictions[[length(predictions) + 1L]] <- data.frame(
      fold = fold$id,
      Date = event$Date[test_events],
      event_id = event$event_id[test_events],
      horizon = horizon,
      feature_mode = feature_mode,
      evaluation_subset = evaluation_subset,
      target = target,
      specification = grid_fit$selected$specification,
      policy = policy$rule,
      policy_fraction = policy$fraction,
      policy_threshold = policy$threshold,
      score = test_scores,
      selected = test_selected,
      net_return = event[[paste0("net_return_", horizon)]][test_events],
      excess_return = event[[paste0("excess_return_", horizon)]][test_events],
      utility = event[[paste0("utility_", horizon)]][test_events],
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
      daily_train_rows = length(train_daily),
      daily_validation_rows = length(validation_daily),
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
    weight_half_life = weight_half_life,
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
      sum(fold_metrics$events, na.rm = TRUE) >= 15L &&
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




