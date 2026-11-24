max_drawdown_from_returns <- function(returns) {
  if (!length(returns)) return(NA_real_)
  wealth <- exp(cumsum(log1p(returns)))
  min(wealth / cummax(wealth) - 1)
}

run_drawdown_audit <- function(predictions, event_table, run_id) {
  p <- predictions[predictions$run == run_id, , drop = FALSE]
  e <- event_table[, c("event_id", "adverse_5d"), drop = FALSE]
  p <- merge(p, e, by = "event_id", all.x = TRUE, sort = FALSE)
  p <- p[order(p$Date), ]
  fold_rows <- lapply(split(p, p$fold), function(z) {
    selected <- as.logical(z$selected)
    selected[is.na(selected)] <- FALSE
    base_net <- expm1(z$net_return)
    chosen_net <- base_net[selected]
    base_adverse <- expm1(z$adverse_5d)
    chosen_adverse <- base_adverse[selected]
    data.frame(
      fold = z$fold[1L],
      events = nrow(z),
      selected = sum(selected),
      baseline_mean_adverse_bp = mean(base_adverse) * 10000,
      selected_mean_adverse_bp = if (length(chosen_adverse)) mean(chosen_adverse) * 10000 else NA_real_,
      baseline_worst_adverse_bp = min(base_adverse) * 10000,
      selected_worst_adverse_bp = if (length(chosen_adverse)) min(chosen_adverse) * 10000 else NA_real_,
      baseline_max_drawdown_bp = max_drawdown_from_returns(base_net) * 10000,
      selected_max_drawdown_bp = max_drawdown_from_returns(chosen_net) * 10000,
      stringsAsFactors = FALSE
    )
  })
  fold_rows <- do.call(rbind, fold_rows)
  selected <- as.logical(p$selected)
  selected[is.na(selected)] <- FALSE
  base_net <- expm1(p$net_return)
  chosen_net <- base_net[selected]
  base_adverse <- expm1(p$adverse_5d)
  chosen_adverse <- base_adverse[selected]
  overall <- data.frame(
    run = run_id,
    test_events = nrow(p),
    selected = sum(selected),
    baseline_mean_adverse_bp = mean(base_adverse) * 10000,
    selected_mean_adverse_bp = if (length(chosen_adverse)) mean(chosen_adverse) * 10000 else NA_real_,
    baseline_worst_adverse_bp = min(base_adverse) * 10000,
    selected_worst_adverse_bp = if (length(chosen_adverse)) min(chosen_adverse) * 10000 else NA_real_,
    baseline_max_drawdown_bp = max_drawdown_from_returns(base_net) * 10000,
    selected_max_drawdown_bp = max_drawdown_from_returns(chosen_net) * 10000,
    drawdown_not_worse = if (length(chosen_net)) {
      max_drawdown_from_returns(chosen_net) >= max_drawdown_from_returns(base_net)
    } else TRUE,
    stringsAsFactors = FALSE
  )
  list(folds = fold_rows, overall = overall)
}

