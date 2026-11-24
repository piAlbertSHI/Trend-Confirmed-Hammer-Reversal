select_validation_policy <- function(event,
                                     rows,
                                     scores,
                                     horizon,
                                     minimum_events = 5L) {
  utility <- expm1(event[[paste0("utility_", horizon)]][rows])
  gate_specs <- list(
    list(name = "all", value = NA_real_, mask = rep(TRUE, length(rows)))
  )
  if ("sma100_slope20" %in% names(event)) {
    gate_specs[[length(gate_specs) + 1L]] <- list(
      name = "trend_positive",
      value = 0,
      mask = is.finite(event$sma100_slope20[rows]) &
        event$sma100_slope20[rows] >= 0
    )
  }
  if ("drawdown100" %in% names(event)) {
    threshold <- stats::median(event$drawdown100[rows], na.rm = TRUE)
    gate_specs[[length(gate_specs) + 1L]] <- list(
      name = "drawdown_above_median",
      value = threshold,
      mask = is.finite(event$drawdown100[rows]) &
        event$drawdown100[rows] >= threshold
    )
  }
  if ("volatility20" %in% names(event)) {
    threshold <- stats::median(event$volatility20[rows], na.rm = TRUE)
    gate_specs[[length(gate_specs) + 1L]] <- list(
      name = "volatility_below_median",
      value = threshold,
      mask = is.finite(event$volatility20[rows]) &
        event$volatility20[rows] <= threshold
    )
  }

  policies <- list(no_trade = list(
    selected = rep(FALSE, length(rows)),
    rule = "no_trade",
    fraction = 0,
    threshold = NA_real_,
    gate_name = "all",
    gate_value = NA_real_
  ))
  for (gate in gate_specs) {
    for (fraction in c(0.25, 0.50, 0.75)) {
      top <- select_event_top_fraction(scores, fraction = fraction)
      key <- paste("top", fraction, gate$name, sep = "__")
      policies[[key]] <- list(
        selected = top$selected & gate$mask,
        rule = key,
        fraction = fraction,
        threshold = top$threshold,
        gate_name = gate$name,
        gate_value = gate$value
      )
    }
    positive <- is.finite(scores) & scores > 0 & gate$mask
    if (sum(positive) >= minimum_events) {
      key <- paste("positive_score", gate$name, sep = "__")
      policies[[key]] <- list(
        selected = positive,
        rule = key,
        fraction = mean(positive),
        threshold = 0,
        gate_name = gate$name,
        gate_value = gate$value
      )
    }
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

select_event_policy_for_test <- function(scores, policy, event = NULL, rows = NULL) {
  if (policy$rule == "no_trade") return(rep(FALSE, length(scores)))
  selected <- if (grepl("^positive_score", policy$rule)) {
    is.finite(scores) & scores > 0
  } else {
    is.finite(scores) & scores >= policy$threshold
  }
  gate_name <- if (is.null(policy$gate_name)) "all" else policy$gate_name
  if (gate_name != "all") {
    if (is.null(event) || is.null(rows)) {
      stop("Event rows are required for a regime gate.")
    }
    if (gate_name == "trend_positive") {
      selected <- selected & event$sma100_slope20[rows] >= policy$gate_value
    } else if (gate_name == "drawdown_above_median") {
      selected <- selected & event$drawdown100[rows] >= policy$gate_value
    } else if (gate_name == "volatility_below_median") {
      selected <- selected & event$volatility20[rows] <= policy$gate_value
    }
  }
  selected[is.na(selected)] <- FALSE
  selected
}

