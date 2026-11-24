fit_elastic_net <- function(data,
                           train_rows,
                           validation_rows,
                           target = "target_positive_5d",
                           alpha = 0.5,
                           lambda_grid = exp(seq(log(1.5), log(0.0005), length.out = 70L))) {
  train_rows <- finite_target_rows(data, train_rows, target)
  validation_rows <- finite_target_rows(data, validation_rows, target)
  if (length(train_rows) < 100L || length(validation_rows) < 20L) {
    stop("Insufficient observations for Elastic Net training.")
  }

  folds <- time_cv_folds(data, split = "train", blocks = 4L,
                         embargo = 10L, target = target)
  losses <- matrix(NA_real_, nrow = length(folds), ncol = length(lambda_grid))
  for (i in seq_along(folds)) {
    fold <- folds[[i]]
    fold_train <- finite_target_rows(data, fold$train, target)
    fold_valid <- finite_target_rows(data, fold$validation, target)
    recipe <- fit_preprocess(data, fold_train, scale = TRUE)
    x_train <- apply_preprocess(data, fold_train, recipe)
    x_valid <- apply_preprocess(data, fold_valid, recipe)
    y_train <- target_vector(data, fold_train, target)
    y_valid <- target_vector(data, fold_valid, target)
    fit <- glmnet::glmnet(
      x = x_train,
      y = y_train,
      family = "binomial",
      alpha = alpha,
      lambda = lambda_grid,
      standardize = FALSE,
      type.measure = "deviance"
    )
    prediction <- predict(fit, newx = x_valid, type = "response")
    losses[i, ] <- vapply(seq_along(lambda_grid), function(j) {
      logloss_binary(y_valid, prediction[, j])
    }, 0.0)
  }

  mean_loss <- colMeans(losses, na.rm = TRUE)
  se_loss <- apply(losses, 2L, sd, na.rm = TRUE) /
    sqrt(colSums(is.finite(losses)))
  best <- which.min(mean_loss)
  lambda <- lambda_grid[best]

  train_recipe <- fit_preprocess(data, train_rows, scale = TRUE)
  x_train <- apply_preprocess(data, train_rows, train_recipe)
  y_train <- target_vector(data, train_rows, target)
  train_model <- glmnet::glmnet(
    x = x_train,
    y = y_train,
    family = "binomial",
    alpha = alpha,
    lambda = lambda,
    standardize = FALSE
  )
  validation_prediction <- as.numeric(predict(
    train_model,
    newx = apply_preprocess(data, validation_rows, train_recipe),
    type = "response"
  ))

  combined_rows <- sort(unique(c(train_rows, validation_rows)))
  final_recipe <- fit_preprocess(data, combined_rows, scale = TRUE)
  final_model <- glmnet::glmnet(
    x = apply_preprocess(data, combined_rows, final_recipe),
    y = target_vector(data, combined_rows, target),
    family = "binomial",
    alpha = alpha,
    lambda = lambda,
    standardize = FALSE
  )

  predict_final <- function(rows) {
    as.numeric(predict(
      final_model,
      newx = apply_preprocess(data, rows, final_recipe),
      type = "response"
    ))
  }

  list(
    name = "Elastic Net",
    alpha = alpha,
    lambda = lambda,
    cv = data.frame(lambda = lambda_grid, mean_logloss = mean_loss, se_logloss = se_loss),
    validation_prediction = validation_prediction,
    validation_metrics = classification_metrics(
      target_vector(data, validation_rows, target),
      validation_prediction
    ),
    predict = predict_final,
    model = final_model,
    recipe = final_recipe
  )
}

fit_xgboost <- function(data,
                        train_rows,
                        validation_rows,
                        target = "target_positive_5d",
                        nrounds = 1800L,
                        early_stopping_rounds = 120L) {
  train_rows <- finite_target_rows(data, train_rows, target)
  validation_rows <- finite_target_rows(data, validation_rows, target)
  if (length(train_rows) < 100L || length(validation_rows) < 20L) {
    stop("Insufficient observations for XGBoost training.")
  }

  train_recipe <- fit_preprocess(data, train_rows, scale = FALSE)
  x_train <- apply_preprocess(data, train_rows, train_recipe)
  x_valid <- apply_preprocess(data, validation_rows, train_recipe)
  y_train <- target_vector(data, train_rows, target)
  y_valid <- target_vector(data, validation_rows, target)
  d_train <- xgboost::xgb.DMatrix(x_train, label = y_train)
  d_valid <- xgboost::xgb.DMatrix(x_valid, label = y_valid)

  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "aucpr",
    max_depth = 6L,
    min_child_weight = 12,
    eta = 0.03,
    subsample = 0.82,
    colsample_bytree = 0.82,
    gamma = 0.2,
    lambda = 20,
    alpha = 1,
    max_delta_step = 1,
    tree_method = "hist",
    seed = 20250530L,
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
  validation_metric <- grep("^validation_aucpr", names(training_log), value = TRUE)
  if (!length(validation_metric)) {
    validation_metric <- grep("^validation_", names(training_log), value = TRUE)
  }
  best_iteration <- if (nrow(training_log) && length(validation_metric)) {
    as.integer(training_log$iter[which.max(training_log[[validation_metric[1L]]])])
  } else {
    as.integer(nrounds)
  }
  best_iteration <- max(1L, min(best_iteration, nrounds))
  validation_prediction <- as.numeric(predict(train_model, d_valid))

  combined_rows <- sort(unique(c(train_rows, validation_rows)))
  final_recipe <- fit_preprocess(data, combined_rows, scale = FALSE)
  d_combined <- xgboost::xgb.DMatrix(
    apply_preprocess(data, combined_rows, final_recipe),
    label = target_vector(data, combined_rows, target)
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
      xgboost::xgb.DMatrix(apply_preprocess(data, rows, final_recipe))
    ))
  }

  list(
    name = "XGBoost",
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






