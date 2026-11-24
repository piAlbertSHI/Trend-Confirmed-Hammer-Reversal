logloss_binary <- function(y, p, eps = 1e-7) {
  p <- pmin(pmax(as.numeric(p), eps), 1 - eps)
  -mean(as.numeric(y) * log(p) + (1 - as.numeric(y)) * log1p(-p))
}

auc_rank <- function(y, p) {
  y <- as.numeric(y)
  p <- as.numeric(p)
  ok <- is.finite(y) & is.finite(p)
  y <- y[ok]
  p <- p[ok]
  n_pos <- sum(y == 1)
  n_neg <- sum(y == 0)
  if (n_pos == 0L || n_neg == 0L) return(NA_real_)
  ranks <- rank(p, ties.method = "average")
  (sum(ranks[y == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

pr_auc <- function(y, p) {
  y <- as.numeric(y)
  p <- as.numeric(p)
  ok <- is.finite(y) & is.finite(p)
  y <- y[ok]
  p <- p[ok]
  positives <- sum(y == 1)
  if (!positives) return(NA_real_)
  order_idx <- order(p, decreasing = TRUE, method = "radix")
  y <- y[order_idx]
  tp <- cumsum(y == 1)
  fp <- cumsum(y == 0)
  recall <- tp / positives
  precision <- tp / pmax(tp + fp, 1)
  recall <- c(0, recall)
  precision <- c(1, precision)
  sum(diff(recall) * (head(precision, -1L) + tail(precision, -1L)) / 2)
}

classification_metrics <- function(y, p, threshold = 0.5) {
  y <- as.numeric(y)
  p <- as.numeric(p)
  ok <- !is.na(y) & is.finite(p)
  y <- y[ok]
  p <- p[ok]
  pred <- as.numeric(p >= threshold)
  tp <- sum(pred == 1 & y == 1)
  tn <- sum(pred == 0 & y == 0)
  fp <- sum(pred == 1 & y == 0)
  fn <- sum(pred == 0 & y == 1)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  data.frame(
    n = length(y),
    positive_rate = mean(y),
    threshold = threshold,
    auc = auc_rank(y, p),
    pr_auc = pr_auc(y, p),
    logloss = logloss_binary(y, p),
    brier = mean((p - y)^2),
    accuracy = mean(pred == y),
    precision = precision,
    recall = recall,
    f1 = if (is.finite(precision) && is.finite(recall) && precision + recall > 0) {
      2 * precision * recall / (precision + recall)
    } else NA_real_,
    predicted_positive_rate = mean(pred)
  )
}

event_metrics <- function(data, rows, p = NULL, threshold = 0.5, model = "model") {
  if (!length(rows)) {
    return(data.frame(model = model, threshold = threshold, events = 0L))
  }
  target <- data$target_net_5d[rows]
  excess <- data$target_excess_5d[rows]
  if (!is.null(p)) {
    p_rows <- attr(p, "rows")
    if (length(p_rows)) {
      p <- as.numeric(p)[match(rows, p_rows)]
    } else if (length(p) != length(rows)) {
      stop("Prediction vector must have one value per event row.")
    }
  }
  selected <- if (is.null(p)) rep(TRUE, length(rows)) else p >= threshold
  selected <- selected & is.finite(target)
  n_selected <- sum(selected)
  net <- target[selected]
  excess_selected <- excess[selected]
  benchmark <- log(data$Close[pmin(rows + 5L, nrow(data))] / data$Close[rows])
  benchmark <- benchmark[selected]
  data.frame(
    model = model,
    threshold = threshold,
    events = length(rows),
    selected = n_selected,
    selection_rate = n_selected / length(rows),
    mean_net_bp = if (n_selected) mean(expm1(net)) * 10000 else NA_real_,
    median_net_bp = if (n_selected) median(expm1(net)) * 10000 else NA_real_,
    win_rate = if (n_selected) mean(net > 0) else NA_real_,
    mean_excess_bp = if (n_selected) mean(expm1(excess_selected)) * 10000 else NA_real_,
    benchmark_mean_bp = if (n_selected) mean(expm1(benchmark)) * 10000 else NA_real_,
    compounded_net = if (n_selected) expm1(sum(net)) else NA_real_,
    benchmark_compounded = if (n_selected) expm1(sum(benchmark)) else NA_real_
  )
}

