select_validation_policy <- function(event,
                                     rows,
                                     scores,
                                     horizon,
                                     minimum_events = 5L) {
  utility <- expm1(event[[paste0("utility_", horizon)]][rows])
  adverse <- expm1(event[[paste0("adverse_", horizon)]][rows])
  policies <- list(no_trade = list(
    selected = rep(FALSE, length(rows)),
    rule = "no_trade",
    fraction = 0,
    threshold = NA_real_,
    gate_name = "all",
    gate_value = NA_real_
  ))
  for (fraction in c(0.25, 0.50, 0.75)) {
    top <- select_event_top_fraction(scores, fraction = fraction)
    policies[[paste0("top_", fraction)]] <- list(
      selected = top$selected,
      rule = "top_fraction",
      fraction = fraction,
      threshold = top$threshold,
      gate_name = "all",
      gate_value = NA_real_
    )
  }
  positive <- is.finite(scores) & scores > 0
  if (sum(positive) >= minimum_events) {
    policies[["positive_score"]] <- list(
      selected = positive,
      rule = "positive_score",
      fraction = mean(positive),
      threshold = 0,
      gate_name = "all",
      gate_value = NA_real_
    )
  }
  base_adverse <- mean(adverse)
  values <- vapply(policies, function(policy) {
    selected <- policy$selected
    if (sum(selected) < minimum_events) return(-Inf)
    selected_adverse <- mean(adverse[selected])
    if (selected_adverse < base_adverse) return(-Inf)
    mean(utility[selected])
  }, 0.0)
  if (all(!is.finite(values))) {
    values <- vapply(policies, function(policy) {
      selected <- policy$selected
      if (sum(selected) < minimum_events) return(-Inf)
      mean(utility[selected])
    }, 0.0)
  }
  choice <- names(which.max(values))
  selected_policy <- policies[[choice]]
  selected_policy$validation_mean_utility_bp <- if (is.finite(values[[choice]])) {
    values[[choice]] * 10000
  } else NA_real_
  selected_policy
}

select_event_policy_for_test <- function(scores, policy, event = NULL, rows = NULL) {
  if (policy$rule == "no_trade") return(rep(FALSE, length(scores)))
  if (policy$rule == "positive_score") return(is.finite(scores) & scores > 0)
  selected <- is.finite(scores) & scores >= policy$threshold
  selected[is.na(selected)] <- FALSE
  selected
}

