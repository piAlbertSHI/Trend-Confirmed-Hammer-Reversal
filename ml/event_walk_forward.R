event_walk_forward_folds <- function(event,
                                     min_train = 20L,
                                     validation_size = 8L,
                                     test_size = 6L,
                                     step = test_size,
                                     embargo_sessions = 10L) {
  n <- nrow(event)
  if (n < min_train + validation_size + test_size) {
    stop("Too few events for walk-forward validation.")
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
    test <- (start + validation_size):(start + validation_size + test_size - 1L)
    cutoff <- event$row_index[min(validation)] - as.integer(embargo_sessions)
    train <- train[event$row_index[train] < cutoff]
    if (length(train) < min_train || !length(test)) next
    folds[[length(folds) + 1L]] <- list(
      id = paste0("event_fold_", length(folds) + 1L),
      train = train,
      validation = validation,
      test = test,
      train_start = min(event$Date[train]),
      train_end = max(event$Date[train]),
      validation_start = min(event$Date[validation]),
      validation_end = max(event$Date[validation]),
      test_start = min(event$Date[test]),
      test_end = max(event$Date[test]),
      embargo_sessions = embargo_sessions
    )
  }
  if (length(folds) < 3L) stop("Could not construct at least three event walk-forward folds.")
  folds
}

event_selection_metrics <- function(event,
                                    rows,
                                    probabilities,
                                    threshold,
                                    fold_id,
                                    model = "Event XGBoost",
                                    classification_target = "label_net_5d",
                                    selected = NULL,
                                    selection_rule = "validation_threshold",
                                    classification_threshold = 0.5) {
  target <- event$net_return_5d[rows]
  excess <- event$excess_return_5d[rows]
  if (is.null(selected)) {
    selected <- is.finite(probabilities) & probabilities >= threshold
  }
  selected <- as.logical(selected)
  selected[is.na(selected)] <- FALSE
  base_net <- expm1(target)
  base_excess <- expm1(excess)
  chosen_net <- base_net[selected]
  chosen_excess <- base_excess[selected]
  classification <- classification_metrics(
    event[[classification_target]][rows],
    probabilities,
    threshold = classification_threshold
  )
  data.frame(
    fold = fold_id,
    model = model,
    target = classification_target,
    selection_rule = selection_rule,
    events = length(rows),
    selected = sum(selected),
    selection_rate = mean(selected),
    threshold = threshold,
    baseline_mean_net_bp = mean(base_net, na.rm = TRUE) * 10000,
    selected_mean_net_bp = if (length(chosen_net)) mean(chosen_net) * 10000 else NA_real_,
    uplift_mean_net_bp = if (length(chosen_net)) {
      (mean(chosen_net) - mean(base_net, na.rm = TRUE)) * 10000
    } else NA_real_,
    baseline_mean_excess_bp = mean(base_excess, na.rm = TRUE) * 10000,
    selected_mean_excess_bp = if (length(chosen_excess)) mean(chosen_excess) * 10000 else NA_real_,
    uplift_mean_excess_bp = if (length(chosen_excess)) {
      (mean(chosen_excess) - mean(base_excess, na.rm = TRUE)) * 10000
    } else NA_real_,
    baseline_compounded_net = expm1(sum(target, na.rm = TRUE)),
    selected_compounded_net = if (length(chosen_net)) expm1(sum(log1p(chosen_net))) else NA_real_,
    auc = classification$auc,
    pr_auc = classification$pr_auc,
    logloss = classification$logloss,
    stringsAsFactors = FALSE
  )
}

run_event_walk_forward <- function(event,
                                    folds,
                                    feature_names = event_model_feature_set(event),
                                    target = "label_net_5d",
                                    selection_rule = c("validation_threshold", "top_fraction"),
                                    top_fraction = 0.5) {
  selection_rule <- match.arg(selection_rule)
  predictions <- list()
  fold_metrics <- list()
  models <- list()
  for (fold in folds) {
    fit <- fit_event_xgboost(
      data = event,
      train_rows = fold$train,
      validation_rows = fold$validation,
      feature_names = feature_names,
      target = target
    )
    validation_probability <- fit$validation_prediction
    test_probability <- fit$predict(fold$test)
    if (selection_rule == "validation_threshold") {
      threshold <- choose_event_threshold(validation_probability)
      selected <- is.finite(test_probability) & test_probability >= threshold
      classification_threshold <- threshold
    } else {
      top <- select_event_top_fraction(test_probability, fraction = top_fraction)
      threshold <- top$threshold
      selected <- top$selected
      classification_threshold <- 0.5
    }
    metrics <- event_selection_metrics(
      event = event,
      rows = fold$test,
      probabilities = test_probability,
      threshold = threshold,
      fold_id = fold$id,
      classification_target = target,
      selected = selected,
      selection_rule = selection_rule,
      classification_threshold = classification_threshold
    )
    metrics$best_iteration <- fit$best_iteration
    metrics$validation_events <- length(fold$validation)
    metrics$test_start <- fold$test_start
    metrics$test_end <- fold$test_end
    fold_metrics[[length(fold_metrics) + 1L]] <- metrics

    test_predictions <- data.frame(
      fold = fold$id,
      event_id = event$event_id[fold$test],
      Date = event$Date[fold$test],
      target = target,
      selection_rule = selection_rule,
      label_net_5d = event$label_net_5d[fold$test],
      label_excess_5d = event$label_excess_5d[fold$test],
      net_return_5d = event$net_return_5d[fold$test],
      excess_return_5d = event$excess_return_5d[fold$test],
      probability = test_probability,
      threshold = threshold,
      selected = selected,
      stringsAsFactors = FALSE
    )
    predictions[[length(predictions) + 1L]] <- test_predictions
    models[[fold$id]] <- fit
  }

  fold_metrics <- do.call(rbind, fold_metrics)
  predictions <- do.call(rbind, predictions)
  uplift <- fold_metrics$uplift_mean_net_bp
  uplift_excess <- fold_metrics$uplift_mean_excess_bp
  summary <- data.frame(
    folds = nrow(fold_metrics),
    test_events = sum(fold_metrics$events),
    positive_uplift_folds = sum(uplift > 0, na.rm = TRUE),
    positive_uplift_rate = mean(uplift > 0, na.rm = TRUE),
    mean_uplift_net_bp = mean(uplift, na.rm = TRUE),
    median_uplift_net_bp = median(uplift, na.rm = TRUE),
    mean_uplift_excess_bp = mean(uplift_excess, na.rm = TRUE),
    median_uplift_excess_bp = median(uplift_excess, na.rm = TRUE),
    positive_excess_uplift_rate = mean(uplift_excess > 0, na.rm = TRUE),
    mean_selected_rate = mean(fold_metrics$selection_rate, na.rm = TRUE),
    mean_test_auc = mean(fold_metrics$auc, na.rm = TRUE),
    mean_test_pr_auc = mean(fold_metrics$pr_auc, na.rm = TRUE),
    stable_gain = isTRUE(
      sum(fold_metrics$events, na.rm = TRUE) >= 40L &&
        mean(uplift, na.rm = TRUE) > 0 &&
        mean(uplift > 0, na.rm = TRUE) >= 0.8 &&
        median(uplift, na.rm = TRUE) > 0 &&
        mean(uplift_excess, na.rm = TRUE) > 0 &&
        median(uplift_excess, na.rm = TRUE) > 0 &&
        mean(uplift_excess > 0, na.rm = TRUE) >= 0.6
    ),
    stringsAsFactors = FALSE
  )
  list(
    folds = folds,
    fold_metrics = fold_metrics,
    predictions = predictions,
    summary = summary,
    models = models
  )
}


