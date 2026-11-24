multiobjective_policy_candidates <- function(event,
                                           rows,
                                           utility_scores,
                                           adverse_scores,
                                           horizon = "5d",
                                           minimum_events = 5L) {
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
      top <- select_event_top_fraction(utility_scores, fraction)
      selected <- top$selected &
        is.finite(adverse_scores) &
        adverse_scores >= risk_thresholds[[risk_name]]
      policies[[paste("top", fraction, risk_name, sep = "__")]] <- list(
        selected = selected,
        rule = paste("top", fraction, risk_name, sep = "__"),
        fraction = fraction,
        utility_threshold = top$threshold,
        risk_threshold = risk_thresholds[[risk_name]]
      )
    }
  }
  policies
}

choose_multiobjective_policy <- function(event,
                                         rows,
                                         utility_scores,
                                         adverse_scores,
                                         horizon = "5d",
                                         risk_weight = 0.5,
                                         minimum_events = 5L) {
  utility <- expm1(event[[paste0("utility_", horizon)]][rows])
  adverse <- expm1(event[[paste0("adverse_", horizon)]][rows])
  base_utility <- mean(utility) * 10000
  base_adverse <- mean(adverse) * 10000
  policies <- multiobjective_policy_candidates(
    event, rows, utility_scores, adverse_scores, horizon, minimum_events
  )
  objective <- vapply(policies, function(policy) {
    selected <- policy$selected
    if (sum(selected) < minimum_events) return(-Inf)
    utility_change <- mean(utility[selected]) * 10000 - base_utility
    adverse_change <- mean(adverse[selected]) * 10000 - base_adverse
    utility_change + risk_weight * adverse_change
  }, 0.0)
  choice <- names(which.max(objective))
  policy <- policies[[choice]]
  selected <- policy$selected
  policy$validation_utility_change_bp <- if (sum(selected)) {
    mean(utility[selected]) * 10000 - base_utility
  } else 0
  policy$validation_adverse_change_bp <- if (sum(selected)) {
    mean(adverse[selected]) * 10000 - base_adverse
  } else 0
  policy$validation_objective <- objective[[choice]]
  policy$risk_weight <- risk_weight
  policy
}

run_daily_multiobjective <- function(daily,
                                     event,
                                     folds,
                                     risk_weights = c(0, 0.25, 0.5, 1),
                                     weight_half_life = 1260,
                                     feature_mode = "correlation_pruned") {
  outputs <- list()
  for (risk_weight in risk_weights) {
    fold_metrics <- list()
    predictions <- list()
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
          is.finite(daily$utility_5d) &
          is.finite(daily$adverse_5d)
      )
      validation_daily <- which(
        daily$Date >= validation_start &
          daily$Date < test_start - 10L &
          daily$warmup_complete &
          is.finite(daily$utility_5d) &
          is.finite(daily$adverse_5d)
      )
      utility_grid <- fit_daily_model_grid(
        daily, train_daily, validation_daily, used_features, "utility_5d",
        seed = 20250530L + fold_index, weight_half_life = weight_half_life
      )
      adverse_grid <- fit_daily_model_grid(
        daily, train_daily, validation_daily, used_features, "adverse_5d",
        seed = 20350530L + fold_index, weight_half_life = weight_half_life
      )
      choices <- lapply(names(utility_grid$fits), function(specification) {
        util_fit <- utility_grid$fits[[specification]]
        util_validation <- util_fit$predict(event$row_index[validation_events])
        adverse_validation <- adverse_grid$fit$predict(event$row_index[validation_events])
        policy <- choose_multiobjective_policy(
          event, validation_events, util_validation, adverse_validation,
          horizon = "5d", risk_weight = risk_weight
        )
        list(
          specification = specification,
          fit = util_fit,
          util_validation = util_validation,
          adverse_fit = adverse_grid$fit,
          policy = policy
        )
      })
      choice_index <- which.max(vapply(choices, function(x) x$policy$validation_objective, 0.0))
      choice <- choices[[choice_index]]
      util_test <- choice$fit$predict(event$row_index[test_events])
      adverse_test <- choice$adverse_fit$predict(event$row_index[test_events])
      selected <- if (choice$policy$rule == "no_trade") {
        rep(FALSE, length(test_events))
      } else {
        is.finite(util_test) &
          util_test >= choice$policy$utility_threshold &
          is.finite(adverse_test) &
          adverse_test >= choice$policy$risk_threshold
      }
      net <- expm1(event$net_return_5d[test_events])
      excess <- expm1(event$excess_return_5d[test_events])
      utility <- expm1(event$utility_5d[test_events])
      adverse <- expm1(event$adverse_5d[test_events])
      fold_metrics[[length(fold_metrics) + 1L]] <- data.frame(
        fold = fold$id,
        events = length(test_events),
        selected = sum(selected),
        selection_rate = mean(selected),
        uplift_mean_net_bp = if (sum(selected)) (mean(net[selected]) - mean(net)) * 10000 else 0,
        uplift_mean_excess_bp = if (sum(selected)) (mean(excess[selected]) - mean(excess)) * 10000 else 0,
        uplift_mean_utility_bp = if (sum(selected)) (mean(utility[selected]) - mean(utility)) * 10000 else 0,
        adverse_change_bp = if (sum(selected)) (mean(adverse[selected]) - mean(adverse)) * 10000 else 0,
        policy = choice$policy$rule,
        validation_objective = choice$policy$validation_objective,
        stringsAsFactors = FALSE
      )
      predictions[[length(predictions) + 1L]] <- data.frame(
        fold = fold$id,
        Date = event$Date[test_events],
        event_id = event$event_id[test_events],
        selected = selected,
        net_return = event$net_return_5d[test_events],
        excess_return = event$excess_return_5d[test_events],
        utility = event$utility_5d[test_events],
        adverse = event$adverse_5d[test_events],
        stringsAsFactors = FALSE
      )
    }
    fold_metrics <- do.call(rbind, fold_metrics)
    predictions <- do.call(rbind, predictions)
    net <- ifelse(is.finite(fold_metrics$uplift_mean_net_bp), fold_metrics$uplift_mean_net_bp, 0)
    excess <- ifelse(is.finite(fold_metrics$uplift_mean_excess_bp), fold_metrics$uplift_mean_excess_bp, 0)
    utility <- ifelse(is.finite(fold_metrics$uplift_mean_utility_bp), fold_metrics$uplift_mean_utility_bp, 0)
    net_ci <- bootstrap_mean_ci(net, seed = 20250530L)
    excess_ci <- bootstrap_mean_ci(excess, seed = 20250531L)
    utility_ci <- bootstrap_mean_ci(utility, seed = 20250532L)
    outputs[[paste0("risk_", risk_weight)]] <- list(
      fold_metrics = fold_metrics,
      predictions = predictions,
      summary = data.frame(
        risk_weight = risk_weight,
        folds = nrow(fold_metrics),
        test_events = sum(fold_metrics$events),
        mean_uplift_net_bp = mean(net),
        median_uplift_net_bp = median(net),
        positive_net_rate = mean(net > 0),
        net_ci_low_bp = net_ci[["lower"]],
        mean_uplift_excess_bp = mean(excess),
        median_uplift_excess_bp = median(excess),
        positive_excess_rate = mean(excess > 0),
        excess_ci_low_bp = excess_ci[["lower"]],
        mean_uplift_utility_bp = mean(utility),
        median_uplift_utility_bp = median(utility),
        positive_utility_rate = mean(utility > 0),
        utility_ci_low_bp = utility_ci[["lower"]],
        mean_adverse_change_bp = mean(ifelse(is.finite(fold_metrics$adverse_change_bp), fold_metrics$adverse_change_bp, 0)),
        stringsAsFactors = FALSE
      )
    )
  }
  outputs
}

