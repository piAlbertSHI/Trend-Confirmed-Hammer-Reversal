purged_event_walk_forward_folds <- function(event,
                                            min_train = 20L,
                                            validation_size = 8L,
                                            test_size = 6L,
                                            step = test_size,
                                            train_embargo_sessions = 10L,
                                            test_gap_sessions = 10L) {
  n <- nrow(event)
  if (n < min_train + validation_size + test_size) {
    stop("Too few events for purged walk-forward validation.")
  }
  starts <- seq.int(
    min_train + 1L,
    n - validation_size - test_size + 1L,
    by = step
  )
  folds <- list()
  for (start in starts) {
    train <- seq_len(start - 1L)
    validation <- start:(start + validation_size - 1L)
    test_start <- start + validation_size
    while (test_start <= n - test_size + 1L &&
           event$row_index[test_start] - event$row_index[max(validation)] <= test_gap_sessions) {
      test_start <- test_start + 1L
    }
    if (test_start > n - test_size + 1L) next
    test <- test_start:(test_start + test_size - 1L)

    cutoff <- event$row_index[min(validation)] - as.integer(train_embargo_sessions)
    train <- train[event$row_index[train] < cutoff]
    if (length(train) < min_train || !length(test)) next

    folds[[length(folds) + 1L]] <- list(
      id = paste0("purged_fold_", length(folds) + 1L),
      train = train,
      validation = validation,
      test = test,
      train_start = min(event$Date[train]),
      train_end = max(event$Date[train]),
      validation_start = min(event$Date[validation]),
      validation_end = max(event$Date[validation]),
      test_start = min(event$Date[test]),
      test_end = max(event$Date[test]),
      train_embargo_sessions = train_embargo_sessions,
      test_gap_sessions = event$row_index[min(test)] -
        event$row_index[max(validation)]
    )
  }
  if (length(folds) < 3L) {
    stop("Could not construct at least three purged walk-forward folds.")
  }
  folds
}

prune_event_features <- function(data,
                                 rows,
                                 feature_names,
                                 correlation_threshold = 0.75) {
  x <- as.matrix(data[rows, feature_names, drop = FALSE])
  storage.mode(x) <- "double"
  usable <- vapply(seq_len(ncol(x)), function(j) {
    finite <- is.finite(x[, j])
    sum(finite) >= 4L && length(unique(x[finite])) > 1L
  }, logical(1L))
  x <- x[, usable, drop = FALSE]
  feature_names <- feature_names[usable]
  if (!length(feature_names)) stop("No usable event features remain after diagnostics.")

  for (j in seq_len(ncol(x))) {
    finite <- is.finite(x[, j])
    x[!finite, j] <- median(x[finite, j])
  }
  keep <- character()
  for (name in feature_names) {
    if (!length(keep)) {
      keep <- name
      next
    }
    correlations <- vapply(keep, function(previous) {
      abs(stats::cor(x[, previous], x[, name]))
    }, 0.0)
    correlations <- correlations[is.finite(correlations)]
    if (!length(correlations) || max(correlations) < correlation_threshold) {
      keep <- c(keep, name)
    }
  }
  keep
}

fit_event_xgb_regression <- function(data,
                                     train_rows,
                                     validation_rows,
                                     feature_names,
                                     target,
                                     nrounds = 600L,
                                     early_stopping_rounds = 60L,
                                     seed = 20250530L,
                                     model_params = NULL,
                                     train_weights = NULL,
                                     validation_weights = NULL) {
  model_data <- data
  attr(model_data, "ml_feature_names") <- feature_names
  target_scale <- 10000
  y_train <- as.numeric(model_data[[target]][train_rows]) * target_scale
  y_valid <- as.numeric(model_data[[target]][validation_rows]) * target_scale
  if (length(y_train) < 16L || length(y_valid) < 4L) {
    stop("Insufficient events for event-level regression.")
  }
  if (!all(is.finite(y_train)) || !all(is.finite(y_valid))) {
    stop("Regression target contains non-finite values.")
  }

  train_recipe <- fit_preprocess(model_data, train_rows, scale = FALSE)
  x_train <- apply_preprocess(model_data, train_rows, train_recipe)
  x_valid <- apply_preprocess(model_data, validation_rows, train_recipe)
  train_bounds <- as.numeric(stats::quantile(y_train, c(0.05, 0.95), names = FALSE))
  y_train_fit <- pmin(pmax(y_train, train_bounds[1L]), train_bounds[2L])
  d_train <- xgboost::xgb.DMatrix(x_train, label = y_train_fit,
                                  weight = train_weights)
  d_valid <- xgboost::xgb.DMatrix(x_valid, label = y_valid,
                                  weight = validation_weights)

  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eval_metric = c("rmse", "mae"),
    max_depth = 2L,
    min_child_weight = 1,
    eta = 0.05,
    subsample = 0.95,
    colsample_bytree = 0.90,
    gamma = 0,
    lambda = 5,
    alpha = 0.25,
    tree_method = "hist",
    seed = seed,
    nthread = 1L
  )
  if (!is.null(model_params)) {
    for (name in names(model_params)) params[[name]] <- model_params[[name]]
  }
  train_model <- xgboost::xgb.train(
    params = params,
    data = d_train,
    nrounds = nrounds,
    evals = list(train = d_train, validation = d_valid),
    early_stopping_rounds = early_stopping_rounds,
    callbacks = list(xgboost::xgb.cb.evaluation.log()),
    verbose = 0
  )
  training_log <- attr(train_model, "evaluation_log")
  validation_metric <- grep("^validation_rmse", names(training_log), value = TRUE)
  if (!length(validation_metric)) {
    validation_metric <- grep("^validation_", names(training_log), value = TRUE)
  }
  best_iteration <- if (nrow(training_log) && length(validation_metric)) {
    as.integer(training_log$iter[which.min(training_log[[validation_metric[1L]]])])
  } else {
    as.integer(nrounds)
  }
  best_iteration <- max(1L, min(best_iteration, nrounds))
  validation_prediction <- as.numeric(predict(train_model, d_valid)) / target_scale

  combined_rows <- sort(unique(c(train_rows, validation_rows)))
  final_recipe <- fit_preprocess(model_data, combined_rows, scale = FALSE)
  y_combined <- as.numeric(model_data[[target]][combined_rows]) * target_scale
  final_bounds <- as.numeric(stats::quantile(y_combined, c(0.05, 0.95), names = FALSE))
  y_combined_fit <- pmin(pmax(y_combined, final_bounds[1L]), final_bounds[2L])
  combined_weights <- rep(1, length(combined_rows))
  if (!is.null(train_weights) || !is.null(validation_weights)) {
    combined_source_rows <- c(train_rows, validation_rows)
    combined_source_weights <- c(
      if (is.null(train_weights)) rep(1, length(train_rows)) else train_weights,
      if (is.null(validation_weights)) rep(1, length(validation_rows)) else validation_weights
    )
    combined_weights <- combined_source_weights[
      match(combined_rows, combined_source_rows)
    ]
  }
  d_combined <- xgboost::xgb.DMatrix(
    apply_preprocess(model_data, combined_rows, final_recipe),
    label = y_combined_fit,
    weight = combined_weights
  )
  final_model <- xgboost::xgb.train(
    params = params,
    data = d_combined,
    nrounds = best_iteration,
    verbose = 0
  )
  predict_final <- function(rows) {
    as.numeric(predict(
      final_model,
      xgboost::xgb.DMatrix(apply_preprocess(model_data, rows, final_recipe))
    )) / target_scale
  }

  list(
    name = "Event XGBoost Regression",
    feature_names = feature_names,
    target = target,
    params = params,
    train_bounds = train_bounds,
    final_bounds = final_bounds,
    target_scale = target_scale,
    best_iteration = best_iteration,
    validation_prediction = validation_prediction,
    training_log = training_log,
    predict = predict_final,
    model = final_model,
    recipe = final_recipe,
    importance = xgboost::xgb.importance(model = final_model)
  )
}

bootstrap_mean_ci <- function(values,
                              iterations = 5000L,
                              seed = 20250530L,
                              level = 0.95) {
  values <- values[is.finite(values)]
  if (length(values) < 2L) {
    return(c(mean = if (length(values)) mean(values) else NA_real_,
             lower = NA_real_, upper = NA_real_))
  }
  set.seed(seed)
  means <- replicate(iterations, mean(sample(values, length(values), replace = TRUE)))
  alpha <- (1 - level) / 2
  c(
    mean = mean(values),
    lower = as.numeric(stats::quantile(means, alpha, names = FALSE)),
    upper = as.numeric(stats::quantile(means, 1 - alpha, names = FALSE))
  )
}

event_regression_metrics <- function(event,
                                     rows,
                                     scores,
                                     selected,
                                     fold_id,
                                     target,
                                     feature_mode,
                                     feature_count,
                                     best_iteration) {
  net <- expm1(event$net_return_5d[rows])
  excess <- expm1(event$excess_return_5d[rows])
  selected <- as.logical(selected)
  selected[is.na(selected)] <- FALSE
  selected_net <- net[selected]
  selected_excess <- excess[selected]
  rank_net <- if (length(unique(scores)) > 1L) {
    suppressWarnings(stats::cor(scores, net, method = "spearman"))
  } else NA_real_
  rank_excess <- if (length(unique(scores)) > 1L) {
    suppressWarnings(stats::cor(scores, excess, method = "spearman"))
  } else NA_real_
  data.frame(
    fold = fold_id,
    target = target,
    feature_mode = feature_mode,
    feature_count = feature_count,
    best_iteration = best_iteration,
    events = length(rows),
    selected = sum(selected),
    selection_rate = mean(selected),
    baseline_mean_net_bp = mean(net) * 10000,
    selected_mean_net_bp = if (length(selected_net)) mean(selected_net) * 10000 else NA_real_,
    uplift_mean_net_bp = if (length(selected_net)) {
      (mean(selected_net) - mean(net)) * 10000
    } else NA_real_,
    baseline_mean_excess_bp = mean(excess) * 10000,
    selected_mean_excess_bp = if (length(selected_excess)) {
      mean(selected_excess) * 10000
    } else NA_real_,
    uplift_mean_excess_bp = if (length(selected_excess)) {
      (mean(selected_excess) - mean(excess)) * 10000
    } else NA_real_,
    baseline_compounded_net = expm1(sum(log1p(net))),
    selected_compounded_net = if (length(selected_net)) {
      expm1(sum(log1p(selected_net)))
    } else NA_real_,
    rank_correlation_net = rank_net,
    rank_correlation_excess = rank_excess,
    rmse = sqrt(mean((scores - if (target == "net_return_5d") {
      event$net_return_5d[rows]
    } else event$excess_return_5d[rows])^2)),
    mae = mean(abs(scores - if (target == "net_return_5d") {
      event$net_return_5d[rows]
    } else event$excess_return_5d[rows])),
    stringsAsFactors = FALSE
  )
}

run_event_regression_walk_forward <- function(event,
                                              folds,
                                              feature_names,
                                              target,
                                              feature_mode = c("fixed", "correlation_pruned"),
                                              selection_fraction = 0.5) {
  feature_mode <- match.arg(feature_mode)
  fold_metrics <- list()
  predictions <- list()
  models <- list()
  feature_selection <- list()

  for (fold in folds) {
    used_features <- if (feature_mode == "correlation_pruned") {
      prune_event_features(event, fold$train, feature_names, correlation_threshold = 0.75)
    } else feature_names
    fit <- fit_event_xgb_regression(
      data = event,
      train_rows = fold$train,
      validation_rows = fold$validation,
      feature_names = used_features,
      target = target,
      seed = 20250530L + length(fold_metrics)
    )
    score <- fit$predict(fold$test)
    top <- select_event_top_fraction(score, fraction = selection_fraction)
    metrics <- event_regression_metrics(
      event = event,
      rows = fold$test,
      scores = score,
      selected = top$selected,
      fold_id = fold$id,
      target = target,
      feature_mode = feature_mode,
      feature_count = length(used_features),
      best_iteration = fit$best_iteration
    )
    metrics$test_start <- fold$test_start
    metrics$test_end <- fold$test_end
    fold_metrics[[length(fold_metrics) + 1L]] <- metrics

    predictions[[length(predictions) + 1L]] <- data.frame(
      fold = fold$id,
      Date = event$Date[fold$test],
      event_id = event$event_id[fold$test],
      target = target,
      feature_mode = feature_mode,
      score = score,
      threshold = top$threshold,
      selected = top$selected,
      net_return_5d = event$net_return_5d[fold$test],
      excess_return_5d = event$excess_return_5d[fold$test],
      stringsAsFactors = FALSE
    )
    feature_selection[[length(feature_selection) + 1L]] <- data.frame(
      fold = fold$id,
      feature_mode = feature_mode,
      feature = used_features,
      stringsAsFactors = FALSE
    )
    models[[fold$id]] <- fit
  }

  fold_metrics <- do.call(rbind, fold_metrics)
  predictions <- do.call(rbind, predictions)
  feature_selection <- do.call(rbind, feature_selection)
  net_ci <- bootstrap_mean_ci(fold_metrics$uplift_mean_net_bp, seed = 20250530L)
  excess_ci <- bootstrap_mean_ci(fold_metrics$uplift_mean_excess_bp, seed = 20250531L)
  summary <- data.frame(
    target = target,
    feature_mode = feature_mode,
    folds = nrow(fold_metrics),
    test_events = sum(fold_metrics$events),
    mean_uplift_net_bp = mean(fold_metrics$uplift_mean_net_bp, na.rm = TRUE),
    median_uplift_net_bp = median(fold_metrics$uplift_mean_net_bp, na.rm = TRUE),
    positive_net_uplift_rate = mean(fold_metrics$uplift_mean_net_bp > 0, na.rm = TRUE),
    net_uplift_ci_low_bp = net_ci[["lower"]],
    net_uplift_ci_high_bp = net_ci[["upper"]],
    mean_uplift_excess_bp = mean(fold_metrics$uplift_mean_excess_bp, na.rm = TRUE),
    median_uplift_excess_bp = median(fold_metrics$uplift_mean_excess_bp, na.rm = TRUE),
    positive_excess_uplift_rate = mean(fold_metrics$uplift_mean_excess_bp > 0, na.rm = TRUE),
    excess_uplift_ci_low_bp = excess_ci[["lower"]],
    excess_uplift_ci_high_bp = excess_ci[["upper"]],
    mean_rank_correlation_net = mean(fold_metrics$rank_correlation_net, na.rm = TRUE),
    mean_rank_correlation_excess = mean(fold_metrics$rank_correlation_excess, na.rm = TRUE),
    mean_selected_rate = mean(fold_metrics$selection_rate, na.rm = TRUE),
    stable_gain = isTRUE(
      sum(fold_metrics$events, na.rm = TRUE) >= 40L &&
        positive_net_uplift_rate >= 0.8 &&
        median_uplift_net_bp > 0 &&
        net_uplift_ci_low_bp > 0 &&
        positive_excess_uplift_rate >= 0.6 &&
        median_uplift_excess_bp > 0 &&
        excess_uplift_ci_low_bp > 0
    ),
    stringsAsFactors = FALSE
  )
  list(
    folds = folds,
    fold_metrics = fold_metrics,
    predictions = predictions,
    feature_selection = feature_selection,
    summary = summary,
    models = models
  )
}





