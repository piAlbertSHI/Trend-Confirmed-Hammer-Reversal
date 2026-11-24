tchr_trade_events <- function(bars, daily, base_state, fixed_rows, fee_bps) {
  ret <- return_components(bars$Open, bars$Close)
  base_run <- portfolio_log(ret, base_state, fixed_rows, fee_bps)
  trades <- position_trades(
    base_run, bars$Date[fixed_rows], bars$Open[fixed_rows],
    bars$Close[fixed_rows], fee_bps
  )
  entry_bar <- match(trades$entry_date, bars$Date)
  exit_bar <- match(trades$exit_date, bars$Date)
  signal_bar <- entry_bar - 1L
  daily_rows <- match(bars$Date[signal_bar], daily$Date)
  benchmark <- log(bars$Close[exit_bar] / bars$Close[signal_bar])
  adverse <- vapply(seq_along(entry_bar), function(i) {
    min(log(bars$Low[seq.int(entry_bar[i], exit_bar[i])] /
              bars$Open[entry_bar[i]]))
  }, 0.0)
  events <- data.frame(
    entry_id = seq_along(entry_bar),
    entry_date = trades$entry_date,
    exit_date = trades$exit_date,
    entry_bar = entry_bar,
    signal_bar = signal_bar,
    row_index = daily_rows,
    net_5d = daily$net_return_5d[daily_rows],
    excess_5d = daily$excess_return_5d[daily_rows],
    utility_5d = daily$utility_5d[daily_rows],
    trade_net = trades$net,
    trade_excess = expm1(log1p(trades$net) - benchmark),
    trade_adverse = expm1(adverse),
    trade_utility = expm1(log1p(trades$net) - benchmark) -
      0.25 * abs(pmin(expm1(adverse), 0)),
    stringsAsFactors = FALSE
  )
  list(events = events, trades = trades, base_run = base_run,
       entry_bar = entry_bar, exit_bar = exit_bar)
}

apply_tchr_ml_entry_filter <- function(base_state, entry_bar, selected) {
  filtered <- base_state
  for (i in which(!is.na(selected) & !selected)) {
    start <- entry_bar[i]
    stop <- start
    while (stop <= length(filtered) && base_state[stop]) stop <- stop + 1L
    filtered[start:min(stop - 1L, length(filtered))] <- FALSE
  }
  filtered
}

predict_overlay_fit <- function(fit, data, rows) {
  if (is.function(fit$predict)) return(fit$predict(rows))
  x <- apply_preprocess(data, rows, fit$recipe)
  as.numeric(predict(fit$model, xgboost::xgb.DMatrix(x))) / fit$target_scale
}

transfer_tchr_ml_scores <- function(tchr_events,
                                    daily,
                                    weighted_models_path,
                                    selection_path,
                                    run_id = "5d__correlation_pruned__hl_1000") {
  saved <- readRDS(weighted_models_path)
  selection <- read.csv(selection_path, stringsAsFactors = FALSE)
  if ("run" %in% names(selection)) {
    selection <- selection[selection$run == run_id, , drop = FALSE]
  }
  fold_metrics <- list()
  predictions <- list()
  thresholds <- list()
  model_runs <- saved$runs[[run_id]]
  if (is.null(model_runs)) stop("Run not found in weighted models: ", run_id)
  for (fold_id in names(model_runs$models)) {
    model <- model_runs$models[[fold_id]]
    selected_policy <- selection[selection$fold == fold_id, , drop = FALSE]
    fold_index <- which(vapply(saved$folds, function(x) x$id == fold_id, logical(1L)))
    if (!length(fold_index) || !nrow(selected_policy)) next
    fold <- saved$folds[[fold_index[1L]]]
    rows <- which(tchr_events$entry_date >= fold$test_start &
                  tchr_events$entry_date <= fold$test_end)
    if (!length(rows)) next
    scores <- predict_overlay_fit(model$fit, daily, tchr_events$row_index[rows])
    policy <- selected_policy$policy[1L]
    threshold <- NA_real_
    if (policy == "top_fraction") {
      # Calibrate the score threshold on realized TCHR entries available before
      # this test window. The calibration grid is fixed in advance, and no
      # future TCHR entry is used to set the threshold.
      calibration_rows <- which(tchr_events$entry_date < fold$test_start)
      calibration_rows <- calibration_rows[is.finite(tchr_events$row_index[calibration_rows])]
      if (length(calibration_rows) >= 4L) {
        calibration_scores <- predict_overlay_fit(
          model$fit, daily, tchr_events$row_index[calibration_rows]
        )
        calibration_utility <- tchr_events$trade_utility[calibration_rows]
        quantiles <- c(0, 0.25, 0.50, 0.75)
        objectives <- vapply(quantiles, function(q) {
          threshold_q <- as.numeric(stats::quantile(
            calibration_scores, probs = q, na.rm = TRUE, names = FALSE
          ))
          chosen <- is.finite(calibration_scores) & calibration_scores >= threshold_q
          if (sum(chosen) < 3L) -Inf else mean(calibration_utility[chosen])
        }, 0.0)
        threshold <- as.numeric(stats::quantile(
          calibration_scores,
          probs = quantiles[which.max(objectives)],
          na.rm = TRUE, names = FALSE
        ))
      } else {
        threshold <- selected_policy$policy_threshold[1L]
      }
      selected <- is.finite(scores) & scores >= threshold
    } else if (policy == "positive_score") {
      selected <- is.finite(scores) & scores > 0
    } else {
      selected <- rep(FALSE, length(rows))
    }
    net <- tchr_events$net_5d[rows]
    excess <- tchr_events$excess_5d[rows]
    utility <- tchr_events$utility_5d[rows]
    trade <- tchr_events$trade_net[rows]
    uplift <- function(values) {
      if (sum(selected)) (mean(values[selected]) - mean(values)) * 10000 else 0
    }
    fold_metrics[[length(fold_metrics) + 1L]] <- data.frame(
      fold = fold_id,
      test_start = fold$test_start,
      test_end = fold$test_end,
      events = length(rows),
      selected = sum(selected),
      selection_rate = mean(selected),
      uplift_5d_net_bp = uplift(expm1(net)),
      uplift_5d_excess_bp = uplift(expm1(excess)),
      uplift_5d_utility_bp = uplift(expm1(utility)),
      uplift_trade_net_bp = uplift(trade),
      stringsAsFactors = FALSE
    )
    predictions[[length(predictions) + 1L]] <- data.frame(
      fold = fold_id,
      entry_id = tchr_events$entry_id[rows],
      entry_date = tchr_events$entry_date[rows],
      score = scores,
      selected = selected,
      net_5d = tchr_events$net_5d[rows],
      excess_5d = tchr_events$excess_5d[rows],
      utility_5d = tchr_events$utility_5d[rows],
      trade_net = tchr_events$trade_net[rows],
      trade_excess = tchr_events$trade_excess[rows],
      threshold = threshold,
      stringsAsFactors = FALSE
    )
    thresholds[[length(thresholds) + 1L]] <- data.frame(
      fold = fold_id,
      test_start = fold$test_start,
      test_end = fold$test_end,
      policy = policy,
      policy_fraction = selected_policy$policy_fraction[1L],
      threshold = threshold,
      stringsAsFactors = FALSE
    )
  }
  if (!length(fold_metrics)) stop("No TCHR events fall inside transfer test folds.")
  list(
    fold_metrics = do.call(rbind, fold_metrics),
    predictions = do.call(rbind, predictions),
    thresholds = do.call(rbind, thresholds),
    saved = saved
  )
}

transfer_tchr_ml_scores_frozen <- function(tchr_events,
                                           daily,
                                           saved,
                                           thresholds,
                                           run_id = "5d__correlation_pruned__hl_1000") {
  model_runs <- saved$runs[[run_id]]
  if (is.null(model_runs)) stop("Run not found in weighted models: ", run_id)
  predictions <- list()
  for (fold_id in names(model_runs$models)) {
    model <- model_runs$models[[fold_id]]
    threshold_row <- thresholds[thresholds$fold == fold_id, , drop = FALSE]
    fold_index <- which(vapply(saved$folds, function(x) x$id == fold_id, logical(1L)))
    if (!length(fold_index) || !nrow(threshold_row)) next
    fold <- saved$folds[[fold_index[1L]]]
    rows <- which(tchr_events$entry_date >= fold$test_start &
                  tchr_events$entry_date <= fold$test_end)
    if (!length(rows)) next
    x <- apply_preprocess(daily, tchr_events$row_index[rows], model$fit$recipe)
    scores <- as.numeric(predict(
      model$fit$model, xgboost::xgb.DMatrix(x)
    )) / model$fit$target_scale
    threshold <- threshold_row$threshold[1L]
    selected <- is.finite(threshold) & is.finite(scores) & scores >= threshold
    predictions[[length(predictions) + 1L]] <- data.frame(
      fold = fold_id,
      entry_id = tchr_events$entry_id[rows],
      entry_date = tchr_events$entry_date[rows],
      score = scores,
      selected = selected,
      threshold = threshold,
      stringsAsFactors = FALSE
    )
  }
  if (!length(predictions)) stop("No frozen TCHR transfer events found.")
  do.call(rbind, predictions)
}

run_tchr_ml_overlay <- function(bars,
                                daily,
                                base_state,
                                fixed_rows,
                                fee_bps,
                                weighted_models_path,
                                selection_path,
                                run_id = "5d__correlation_pruned__hl_1000") {
  events <- tchr_trade_events(bars, daily, base_state, fixed_rows, fee_bps)
  transfer <- transfer_tchr_ml_scores(
    events$events, daily, weighted_models_path, selection_path, run_id
  )
  decision <- rep(NA, nrow(events$events))
  decision[transfer$predictions$entry_id] <- transfer$predictions$selected
  filtered_state <- apply_tchr_ml_entry_filter(
    base_state, events$entry_bar, decision
  )
  ret <- return_components(bars$Open, bars$Close)
  transfer_start <- min(which(!is.na(decision)))
  transfer_rows <- events$entry_bar[transfer_start]:max(fixed_rows)
  base_transfer <- portfolio_log(ret, base_state, transfer_rows, fee_bps)
  filtered_transfer <- portfolio_log(ret, filtered_state, transfer_rows, fee_bps)
  portfolio_row <- function(label, run) {
    metric <- risk_metrics(run$daily)
    data.frame(
      rule = label,
      start = min(bars$Date[transfer_rows]),
      end = max(bars$Date[transfer_rows]),
      sessions = length(transfer_rows),
      compound = unname(metric[["compound"]]),
      annualized = unname(metric[["annualized"]]),
      annual_vol = unname(metric[["annual_vol"]]),
      sharpe = unname(metric[["sharpe"]]),
      max_drawdown = unname(metric[["max_drawdown"]]),
      trades = sum(run$buys),
      exposure = mean(run$weight),
      stringsAsFactors = FALSE
    )
  }
  list(
    events = events$events,
    trades = events$trades,
    predictions = transfer$predictions,
    thresholds = transfer$thresholds,
    fold_metrics = transfer$fold_metrics,
    decision = decision,
    base_state = base_state,
    filtered_state = filtered_state,
    base_transfer = base_transfer,
    filtered_transfer = filtered_transfer,
    portfolio_summary = rbind(
      portfolio_row("TCHR", base_transfer),
      portfolio_row("TCHR + ML overlay", filtered_transfer)
    )
  )
}
