ml_split_definitions <- function() {
  data.frame(
    split = c("train", "validation", "holdout"),
    start = as.Date(c("2001-03-01", "2014-01-01", "2019-01-01")),
    end = as.Date(c("2013-12-31", "2018-12-31", "2025-05-30")),
    role = c("model fitting and time-series cross-validation",
             "early stopping and model selection",
             "final out-of-time evaluation"),
    stringsAsFactors = FALSE
  )
}

assign_ml_split <- function(data, definitions = ml_split_definitions()) {
  stopifnot(inherits(data$Date, "Date"))
  split <- rep(NA_character_, nrow(data))
  for (i in seq_len(nrow(definitions))) {
    hit <- data$Date >= definitions$start[i] & data$Date <= definitions$end[i]
    split[hit] <- definitions$split[i]
  }
  if (anyNA(split)) stop("Some rows are outside the declared time splits.")
  data$split <- split
  attr(data, "ml_feature_names") <- attr(data, "ml_feature_names")
  data
}

split_rows <- function(data, split, target = "target_positive_5d") {
  which(data$split == split & data$warmup_complete & !is.na(data[[target]]))
}

apply_embargo <- function(train_rows, validation_rows, embargo = 10L) {
  if (!length(train_rows) || !length(validation_rows)) return(integer())
  cutoff <- min(validation_rows) - as.integer(embargo)
  train_rows[train_rows < cutoff]
}

time_cv_folds <- function(data,
                          split = "train",
                          blocks = 4L,
                          embargo = 10L,
                          target = "target_positive_5d") {
  rows <- split_rows(data, split, target)
  if (length(rows) < blocks * 20L) stop("Not enough rows for chronological cross-validation.")
  blocks <- min(as.integer(blocks), 6L)
  boundaries <- unique(floor(seq(0, length(rows), length.out = blocks + 1L)))
  boundaries[1L] <- 0L
  boundaries[length(boundaries)] <- length(rows)
  folds <- list()
  for (i in 2:length(boundaries)) {
    valid <- rows[(boundaries[i - 1L] + 1L):boundaries[i]]
    if (!length(valid)) next
    base_train <- rows[seq_len(boundaries[i - 1L])]
    train <- apply_embargo(base_train, valid, embargo)
    if (length(train) < 50L) next
    folds[[length(folds) + 1L]] <- list(
      id = paste0("fold_", length(folds) + 1L),
      train = train,
      validation = valid,
      train_start = min(data$Date[train]),
      train_end = max(data$Date[train]),
      validation_start = min(data$Date[valid]),
      validation_end = max(data$Date[valid])
    )
  }
  if (length(folds) < 2L) stop("Could not construct at least two chronological CV folds.")
  folds
}

split_summary <- function(data, definitions = ml_split_definitions()) {
  out <- merge(
    definitions,
    aggregate(
      list(rows = seq_len(nrow(data))),
      by = list(split = data$split),
      FUN = length
    ),
    by = "split",
    all.x = TRUE,
    sort = FALSE
  )
  labels <- aggregate(
    list(valid_5d_labels = !is.na(data$target_positive_5d)),
    by = list(split = data$split),
    FUN = sum
  )
  positives <- aggregate(
    list(positive_5d = as.numeric(data$target_positive_5d)),
    by = list(split = data$split),
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  out <- merge(out, labels, by = "split", all.x = TRUE, sort = FALSE)
  out <- merge(out, positives, by = "split", all.x = TRUE, sort = FALSE)
  out <- out[match(definitions$split, out$split), ]
  rownames(out) <- NULL
  out
}


