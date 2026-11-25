market_files <- c(
  archive = "data/raw/HSI_25years.csv",
  extension = "data/raw/HSI_extension.json"
)
market_hashes <- c(
  archive = "e8e453bd22e7a2efca47039b7b5f1b278146b27f76b03ebe1867d7eee680cd09",
  extension = "d5a31d37668fcb57dc14f21f46509f7a2638cb7660522e269efee89173c8fb9f"
)
ohlc_fields <- c("Open", "High", "Low", "Close")

validate_bars <- function(bars) {
  stopifnot(inherits(bars$Date, "Date"), nrow(bars) > 1L,
            !anyNA(bars$Date), all(diff(bars$Date) > 0),
            all(ohlc_fields %in% names(bars)))
  prices <- as.matrix(bars[ohlc_fields])
  stopifnot(all(is.finite(prices)), all(prices > 0),
            all(bars$Low <= pmin(bars$Open, bars$Close)),
            all(bars$High >= pmax(bars$Open, bars$Close)))
  if ("Volume" %in% names(bars)) {
    stopifnot(all(is.finite(bars$Volume)), all(bars$Volume >= 0))
  }
  invisible(bars)
}

read_archive <- function(path) {
  header <- strsplit(readLines(path, n = 3L, warn = FALSE), ",", fixed = TRUE)
  stopifnot(identical(header[[1L]][1L], "Price"),
            all(header[[2L]][-1L] == "^HSI"),
            identical(header[[3L]][1L], "Date"))
  fields <- c("Date", header[[1L]][-1L])
  stopifnot(!anyDuplicated(fields), all(ohlc_fields %in% fields))
  raw <- read.csv(path, skip = 3L, header = FALSE, col.names = fields,
                  colClasses = c("character", rep("numeric", length(fields) - 1L)))
  raw$Date <- as.Date(raw$Date, format = "%Y-%m-%d")
  raw <- raw[c("Date", ohlc_fields, "Volume")]
  validate_bars(raw)
  raw
}

read_extension <- function(path) {
  chart <- jsonlite::fromJSON(path, simplifyVector = FALSE)$chart
  stopifnot(is.null(chart$error), length(chart$result) == 1L)
  series <- chart$result[[1L]]
  zone <- series$meta$exchangeTimezoneName
  stopifnot(identical(series$meta$symbol, "^HSI"), identical(zone, "Asia/Hong_Kong"))
  numeric_series <- function(values) {
    vapply(values, function(x) if (is.null(x)) NA_real_ else as.double(x), 0.0)
  }
  stamp <- as.POSIXct(as.numeric(unlist(series$timestamp)), origin = "1970-01-01", tz = "UTC")
  quote <- series$indicators$quote[[1L]]
  bars <- data.frame(
    Date = as.Date(stamp, tz = zone),
    Open = numeric_series(quote$open), High = numeric_series(quote$high),
    Low = numeric_series(quote$low), Close = numeric_series(quote$close),
    Volume = numeric_series(quote$volume)
  )
  validate_bars(bars)
  bars
}

load_market <- function() {
  hashes <- vapply(market_files, function(path) {
    digest::digest(file = path, algo = "sha256")
  }, "", USE.NAMES = FALSE)
  stopifnot(identical(hashes, unname(market_hashes)))
  archive <- read_archive(market_files[["archive"]])
  extension <- read_extension(market_files[["extension"]])
  match_idx <- match(extension$Date, archive$Date)
  overlap <- !is.na(match_idx)
  archive_overlap <- archive[match_idx[overlap], ]
  new_overlap <- extension[overlap, ]
  stopifnot(sum(overlap) >= 20L, max(archive_overlap$Date) == max(archive$Date),
            setequal(new_overlap$Date, archive$Date[archive$Date >= min(extension$Date)]))
  error <- vapply(ohlc_fields, function(field) {
    max(abs(archive_overlap[[field]] - new_overlap[[field]]))
  }, 0.0)
  stopifnot(all(error < 0.01))
  forward <- extension[extension$Date > max(archive$Date), ]
  bars <- rbind(archive, forward)
  rownames(bars) <- NULL
  validate_bars(bars)
  coverage <- do.call(rbind, lapply(list(archive = archive, overlap = new_overlap, extension = forward),
                                  function(x) data.frame(first = min(x$Date), last = max(x$Date),
                                                         sessions = nrow(x), zero_volume = sum(x$Volume == 0))))
  coverage$segment <- rownames(coverage)
  rownames(coverage) <- NULL
  list(archive = archive, full = bars, coverage = coverage,
       overlap = data.frame(field = names(error), max_abs_points = as.numeric(error),
                            sessions = sum(overlap)))
}
