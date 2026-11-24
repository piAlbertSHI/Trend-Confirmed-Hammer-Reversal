fund_file <- "strategy/data/TraHK_chart_2026-11-20.json"
fund_sha256 <- "d0ff45e05d8318de6b2fef334b97c87c08549a38aa6fea7f0fdf93a05501e5de"

read_fund <- function() {
  stopifnot(digest::digest(file = fund_file, algo = "sha256") == fund_sha256)
  raw <- jsonlite::fromJSON(fund_file, simplifyVector = FALSE)$chart
  stopifnot(is.null(raw$error), length(raw$result) == 1L)
  d <- raw$result[[1L]]
  stopifnot(d$meta$symbol == "2800.HK", d$meta$exchangeTimezoneName == "Asia/Hong_Kong",
            d$meta$currency == "HKD")
  date <- as.Date(as.POSIXct(unlist(d$timestamp), origin = "1970-01-01", tz = "UTC"),
                  tz = d$meta$exchangeTimezoneName)
  numeric <- function(q) vapply(q, function(value) if (is.null(value)) NA_real_ else as.numeric(value), 0.0)
  quote <- d$indicators$quote[[1L]]
  open <- numeric(quote$open)
  close <- numeric(quote$close)
  adjusted <- numeric(d$indicators$adjclose[[1L]]$adjclose)
  stopifnot(!anyNA(date), all(diff(date) > 0), length(date) == length(open),
            length(date) == length(close), length(date) == length(adjusted))
  good <- is.finite(open) & open > 0 & is.finite(close) & close > 0 & is.finite(adjusted) & adjusted > 0
  bars <- data.frame(Date = date, Open = open, Close = close, AdjClose = adjusted, valid = good)
  list(bars = bars, distributions = length(d$events$dividends), splits = length(d$events$splits))
}

align_fund <- function(index, feed) {
  d <- feed$bars
  loc <- match(d$Date, index$Date)
  rows <- which(d$valid & !is.na(loc))
  joined <- d[rows, , drop = FALSE]
  index_rows <- loc[rows]
  stopifnot(length(rows) > 2000L, all(diff(index_rows) > 0))
  # Ratio cancels in open-to-close returns and carries the vendor distribution adjustment across dates.
  adjusted_open <- joined$Open * (joined$AdjClose / joined$Close)
  prices <- data.frame(Date = joined$Date, Open = adjusted_open, Close = joined$AdjClose)
  overlap_scope <- which(index$Date >= min(d$Date) & index$Date <= max(d$Date))
  missing <- setdiff(overlap_scope, index_rows)
  audit <- data.frame(symbol = "2800.HK", response_rows = nrow(d), matched_sessions = nrow(prices),
                      first = min(prices$Date), last = max(prices$Date),
                      unusable_quote_rows = sum(!d$valid), fund_only_sessions = sum(d$valid & is.na(loc)),
                      index_without_valid_fund_quote = length(missing),
                      distribution_events = feed$distributions, split_events = feed$splits)
  list(prices = prices, index_rows = index_rows, audit = audit,
       missing = data.frame(date = index$Date[missing]))
}
