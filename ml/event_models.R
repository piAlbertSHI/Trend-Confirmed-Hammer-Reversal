fit_event_xgboost <- function(data,
                              train_rows,
                              validation_rows,
                              feature_names = event_model_feature_set(data),
                              target = "label_net_5d",
                              nrounds = 600L,
                              early_stopping_rounds = 60L,
                              seed = 20250530L) {
  model_data <- data
  attr(model_data, "ml_feature_names") <- feature_names
  train_rows <- finite_target_rows(model_data, train_rows, target)
  validation_rows <- finite_target_rows(model_data, validation_rows, target)
  if (length(train_rows) < 16L || length(validation_rows) < 4L) {
    stop("Insufficient events for event-level XGBoost.")
  }
  y_train <- target_vector(model_data, train_rows, target)
  y_valid <- target_vector(model_data, validation_rows, target)
  if (length(unique(y_train)) < 2L) stop("Event training labels contain one class.")
  if (length(unique(y_valid)) < 2L) {
    warning("Event validation block contains one class. AUC will be undefined.")
  }

  train_recipe <- fit_preprocess(model_data, train_rows, scale = FALSE)
  x_train <- apply_preprocess(model_data, train_rows, train_recipe)
  x_valid <- apply_preprocess(model_data, validation_rows, train_recipe)
  d_train <- xgboost::xgb.DMatrix(x_train, label = y_train)
  d_valid <- xgboost::xgb.DMatrix(x_valid, label = y_valid)

  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss", "aucpr"),
    max_depth = 2L,
    min_child_weight = 4,
    eta = 0.03,
    subsample = 0.90,
    colsample_bytree = 0.90,
    gamma = 0.25,
    lambda = 25,
    alpha = 2,
    max_delta_step = 1,
    tree_method = "hist",
    seed = seed,
    nthread = 1L
  )
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
  validation_metric <- grep("^validation_logloss", names(training_log), value = TRUE)
  if (!length(validation_metric)) {
    validation_metric <- grep("^validation_", names(training_log), value = TRUE)
  }
  best_iteration <- if (nrow(training_log) && length(validation_metric)) {
    as.integer(training_log$iter[which.min(training_log[[validation_metric[1L]]])])
  } else {
    as.integer(nrounds)
  }
  best_iteration <- max(1L, min(best_iteration, nrounds))
  validation_prediction <- as.numeric(predict(train_model, d_valid))

  combined_rows <- sort(unique(c(train_rows, validation_rows)))
  final_recipe <- fit_preprocess(model_data, combined_rows, scale = FALSE)
  d_combined <- xgboost::xgb.DMatrix(
    apply_preprocess(model_data, combined_rows, final_recipe),
    label = target_vector(model_data, combined_rows, target)
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
    ))
  }

  list(
    name = "Event XGBoost",
    feature_names = feature_names,
    params = params,
    best_iteration = best_iteration,
    validation_prediction = validation_prediction,
    validation_metrics = classification_metrics(y_valid, validation_prediction),
    training_log = training_log,
    predict = predict_final,
    model = final_model,
    recipe = final_recipe,
    importance = xgboost::xgb.importance(model = final_model)
  )
}

choose_event_threshold <- function(probabilities,
                                    selection_fraction = 0.5,
                                    min_selected = 2L) {
  probabilities <- as.numeric(probabilities)
  probabilities <- probabilities[is.finite(probabilities)]
  if (!length(probabilities)) return(NA_real_)
  k <- max(as.integer(min_selected),
           ceiling(length(probabilities) * selection_fraction))
  k <- min(k, length(probabilities))
  sort(probabilities, decreasing = TRUE)[k]
}


select_event_top_fraction <- function(probabilities, fraction = 0.5) {
  probabilities <- as.numeric(probabilities)
  n <- length(probabilities)
  selected <- rep(FALSE, n)
  finite <- which(is.finite(probabilities))
  if (!length(finite)) return(list(selected = selected, threshold = NA_real_))
  k <- max(1L, min(length(finite), ceiling(length(finite) * fraction)))
  ranked <- finite[order(probabilities[finite], decreasing = TRUE, method = "radix")]
  chosen <- ranked[seq_len(k)]
  selected[chosen] <- TRUE
  list(selected = selected, threshold = min(probabilities[chosen]))
}
