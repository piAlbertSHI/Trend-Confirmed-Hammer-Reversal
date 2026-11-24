ml_feature_columns <- function(data) {
  cols <- attr(data, "ml_feature_names")
  if (is.null(cols) || !length(cols)) {
    stop("Feature metadata is missing from the ML data frame.")
  }
  cols
}

fit_imputer <- function(data, rows, feature_names = ml_feature_columns(data)) {
  medians <- vapply(feature_names, function(name) {
    x <- as.numeric(data[[name]][rows])
    x <- x[is.finite(x)]
    if (!length(x)) stop("No finite training values for feature: ", name)
    median(x)
  }, 0.0)
  list(feature_names = feature_names, medians = medians)
}

apply_imputer <- function(data, rows, imputer) {
  out <- as.matrix(data[rows, imputer$feature_names, drop = FALSE])
  storage.mode(out) <- "double"
  for (j in seq_along(imputer$feature_names)) {
    bad <- !is.finite(out[, j])
    if (any(bad)) out[bad, j] <- imputer$medians[j]
  }
  colnames(out) <- imputer$feature_names
  out
}

fit_preprocess <- function(data, rows, scale = TRUE) {
  feature_names <- ml_feature_columns(data)
  imputer <- fit_imputer(data, rows, feature_names)
  x <- apply_imputer(data, rows, imputer)
  center <- colMeans(x)
  spread <- apply(x, 2L, sd)
  spread[!is.finite(spread) | spread < 1e-8] <- 1
  list(imputer = imputer, center = center, spread = spread, scale = scale)
}

apply_preprocess <- function(data, rows, recipe) {
  x <- apply_imputer(data, rows, recipe$imputer)
  if (isTRUE(recipe$scale)) {
    x <- sweep(x, 2L, recipe$center, "-")
    x <- sweep(x, 2L, recipe$spread, "/")
  }
  x
}

target_vector <- function(data, rows, target = "target_positive_5d") {
  y <- data[[target]][rows]
  if (is.logical(y)) return(as.numeric(y))
  as.numeric(y)
}

finite_target_rows <- function(data, rows, target) {
  rows[!is.na(data[[target]][rows])]
}

prepare_event_rows <- function(data, rows, min_gap = 5L) {
  candidates <- rows[
    data$tchr_setup[rows] &
      !is.na(data$target_net_5d[rows]) &
      data$warmup_complete[rows]
  ]
  if (length(candidates) <= 1L) return(candidates)
  keep <- logical(length(candidates))
  last <- -Inf
  for (i in seq_along(candidates)) {
    if (candidates[i] - last >= min_gap) {
      keep[i] <- TRUE
      last <- candidates[i]
    }
  }
  candidates[keep]
}

