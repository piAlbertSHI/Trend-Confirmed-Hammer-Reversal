event_model_feature_set <- function(data) {
  preferred <- c(
    "lower_shadow_to_body", "upper_shadow_to_body", "body_to_range",
    "close_location", "close_to_sma20", "close_to_sma100",
    "sma100_slope20", "return_5d", "return_20d", "drawdown100",
    "volatility20", "rsi14"
  )
  available <- intersect(preferred, ml_feature_columns(data))
  stable <- vapply(available, function(name) {
    x <- as.numeric(data[[name]])
    length(unique(x[is.finite(x)])) > 1L
  }, logical(1L))
  available[stable]
}

build_event_table <- function(data, min_gap = 5L) {
  stopifnot(inherits(data$Date, "Date"))
  candidate_rows <- which(
    data$warmup_complete &
      !is.na(data$target_net_5d) &
      !is.na(data$target_excess_5d)
  )
  event_rows <- prepare_event_rows(data, candidate_rows, min_gap = min_gap)
  if (length(event_rows) < 30L) stop("Too few independent TCHR events for event research.")

  feature_names <- ml_feature_columns(data)
  exit_rows <- event_rows + 5L
  benchmark_log <- log(data$Close[exit_rows] / data$Close[event_rows])
  event <- data.frame(
    event_id = seq_along(event_rows),
    row_index = event_rows,
    Date = data$Date[event_rows],
    split = data$split[event_rows],
    data[event_rows, feature_names, drop = FALSE],
    label_net_5d = as.integer(data$target_positive_5d[event_rows]),
    label_excess_5d = as.integer(data$target_positive_excess_5d[event_rows]),
    net_return_5d = data$target_net_5d[event_rows],
    excess_return_5d = data$target_excess_5d[event_rows],
    benchmark_log_5d = benchmark_log,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  attr(event, "ml_feature_names") <- feature_names
  list(
    data = event,
    event_rows = event_rows,
    feature_names = feature_names,
    model_features = event_model_feature_set(event),
    min_gap = min_gap
  )
}

event_label_diagnostics <- function(event) {
  specs <- data.frame(
    label = c("label_net_5d", "label_excess_5d"),
    return = c("net_return_5d", "excess_return_5d"),
    stringsAsFactors = FALSE
  )
  out <- lapply(seq_len(nrow(specs)), function(i) {
    y <- event[[specs$label[i]]]
    r <- event[[specs$return[i]]]
    data.frame(
      label = specs$label[i],
      events = length(y),
      positive = sum(y == 1L, na.rm = TRUE),
      negative = sum(y == 0L, na.rm = TRUE),
      positive_rate = mean(y == 1L, na.rm = TRUE),
      mean_return_bp = mean(expm1(r), na.rm = TRUE) * 10000,
      median_return_bp = median(expm1(r), na.rm = TRUE) * 10000,
      sd_return_bp = sd(expm1(r), na.rm = TRUE) * 10000,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

event_feature_diagnostics <- function(event,
                                      label = "label_net_5d",
                                      selected = event_model_feature_set(event)) {
  feature_names <- ml_feature_columns(event)
  y <- event[[label]]
  rows <- lapply(feature_names, function(name) {
    x <- as.numeric(event[[name]])
    finite <- is.finite(x)
    positive <- finite & y == 1L
    negative <- finite & y == 0L
    pooled_sd <- sd(x[finite])
    effect <- if (sum(positive) >= 2L && sum(negative) >= 2L && is.finite(pooled_sd) &&
                   pooled_sd > 0) {
      (mean(x[positive]) - mean(x[negative])) / pooled_sd
    } else NA_real_
    data.frame(
      feature = name,
      selected_for_event_model = name %in% selected,
      missing_rate = mean(!finite),
      unique_values = length(unique(x[finite])),
      mean = if (any(finite)) mean(x[finite]) else NA_real_,
      sd = if (sum(finite) > 1L) sd(x[finite]) else NA_real_,
      min = if (any(finite)) min(x[finite]) else NA_real_,
      max = if (any(finite)) max(x[finite]) else NA_real_,
      positive_mean = if (any(positive)) mean(x[positive]) else NA_real_,
      negative_mean = if (any(negative)) mean(x[negative]) else NA_real_,
      standardized_mean_difference = effect,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

event_year_counts <- function(event) {
  years <- format(event$Date, "%Y")
  out <- as.data.frame(table(year = years), stringsAsFactors = FALSE)
  names(out)[2L] <- "events"
  out
}

event_feature_correlation <- function(event, selected = event_model_feature_set(event)) {
  x <- as.matrix(event[selected])
  storage.mode(x) <- "double"
  x <- apply(x, 2L, function(v) {
    v[!is.finite(v)] <- median(v[is.finite(v)])
    v
  })
  corr <- stats::cor(x)
  idx <- which(upper.tri(corr), arr.ind = TRUE)
  if (!nrow(idx)) return(data.frame())
  out <- data.frame(
    feature_1 = colnames(corr)[idx[, 1L]],
    feature_2 = colnames(corr)[idx[, 2L]],
    correlation = corr[idx],
    stringsAsFactors = FALSE
  )
  out[order(-abs(out$correlation)), , drop = FALSE]
}

