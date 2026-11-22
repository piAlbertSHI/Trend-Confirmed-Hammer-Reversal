pullback_grid <- function() {
  z <- expand.grid(trigger = c("RSI 10", "RSI 20", "Band reentry"),
                   slow = c(0L, 100L, 200L), hold = c(5L, 10L), KEEP.OUT.ATTRS = FALSE)
  cbind(id = sprintf("P%02d", seq_len(nrow(z))), family = "Pullback", z)
}

pullback_spec <- function(bars) {
  close <- bars$Close
  mid <- as.numeric(TTR::SMA(close, 5))
  band <- features(bars)$lower
  rsi <- as.numeric(TTR::RSI(close, n = 2L))
  atr <- as.numeric(TTR::ATR(bars[c("High", "Low", "Close")], n = 20L)[, "atr"])
  gap <- close - band
  list(close = close, mean_five = mid, atr = atr,
       windows = list("0" = rep(0, length(close)),
                      "100" = as.numeric(TTR::SMA(close, 100L)),
                      "200" = as.numeric(TTR::SMA(close, 200L))),
       trigger = list("RSI 10" = rsi < 10, "RSI 20" = rsi < 20,
                      "Band reentry" = c(FALSE, head(gap, -1L) < 0 & tail(gap, -1L) >= 0)))
}

pullback_target <- function(spec, rule) {
  close <- spec$close
  avg <- spec$mean_five
  slow <- spec$windows[[as.character(rule$slow)]]
  eligible <- spec$trigger[[rule$trigger]] & close < avg & close >= .96 * slow & is.finite(spec$atr)
  eligible <- !is.na(eligible) & eligible
  target <- logical(length(close))
  last <- 0L
  stop <- 0
  for (i in seq_along(close)) {
    # An exit decision at the close keeps the current session's fill and exits on the next open.
    if (i <= last) {
      if (close[i] >= avg[i] || close[i] <= stop) {
        last <- 0L
      }
    } else if (i > last + 1L && eligible[i]) {
      stop <- close[i] - 2 * spec$atr[i]
      last <- i + rule$hold - 1L
    }
    target[i] <- i <= last
  }
  target
}

pullback_matrix <- function(bars, rules) {
  spec <- pullback_spec(bars)
  out <- vapply(seq_len(nrow(rules)), function(j) {
    next_open_positions(pullback_target(spec, rules[j, ]))
  }, logical(nrow(bars)))
  colnames(out) <- rules$id
  out
}

study_pullbacks <- function(x) {
  bars <- x$market$full
  funding <- x$fund
  spans <- x$spans
  rules <- pullback_grid()
  r <- return_components(bars$Open, bars$Close)
  w <- pullback_matrix(bars, rules)
  dev <- period_rows(bars, spans[spans$period == "Development", ])
  folds <- training_blocks(dev, as.Date(c("2005-12-31", "2010-12-31")), bars$Date)
  fit <- training_table(r, w, rules, dev, folds)
  ordered <- fit[order(fit$rank), ]
  key <- ordered[1L, ]
  # Keep all rules visible. A candidate that fails exposure is identified in the ranking.
  candidates <- setNames(lapply(rules$id, function(id) w[, id]), rules$id)
  etf <- lapply(candidates, "[", funding$index_rows)
  a <- score_universe(bars, candidates, spans, "^HSI")
  b <- score_universe(funding$prices, etf, spans, "2800.HK")
  scores <- rbind(a, b)
  ret_fund <- return_components(funding$prices$Open, funding$prices$Close)
  inference <- do.call(rbind, lapply(seq_len(nrow(spans)), function(i) {
    p <- spans[i, ]
    if (!p$period %in% c("Out of sample", "Post-archive")) return(NULL)
    idx <- period_rows(bars, p)
    q <- portfolio_log(r, w[, key$id], idx, trial_fee)
    bh <- portfolio_log(r, rep(TRUE, nrow(bars)), idx, trial_fee)
    ix_fund <- period_rows(funding$prices, p)
    et <- portfolio_log(ret_fund, etf[[key$id]], ix_fund, trial_fee)
    bm <- portfolio_log(ret_fund, rep(TRUE, nrow(funding$prices)), ix_fund, trial_fee)
    rbind(
      cbind(instrument = "^HSI", period = p$period,
            block_inference(cbind("Net return" = q$daily, "vs buy and hold" = q$daily - bh$daily))),
      cbind(instrument = "2800.HK", period = p$period,
            block_inference(cbind("Net return" = et$daily, "vs buy and hold" = et$daily - bm$daily)))
    )
  }))
  market_daily <- comparison_daily(bars, list("Pullback" = w[, key$id], "Buy and hold" = rep(TRUE, nrow(bars))),
                                   spans[spans$period == "Out of sample", ])
  fund_daily <- comparison_daily(funding$prices,
                                 list("Pullback" = etf[[key$id]], "Buy and hold" = rep(TRUE, nrow(funding$prices))),
                                 spans[spans$period == "Out of sample", ])
  list(ranking = ordered, top = key, scores = scores, inference = inference,
       family = family_inference(r, w, dev), market_daily = market_daily, fund_daily = fund_daily)
}

pullback_signature <- function(x) {
  files <- c("strategy/PULLBACK_DESIGN.md", "strategy/pullback.R")
  c(x$signatures, setNames(vapply(files, function(file) digest::digest(file = file, algo = "sha256"), ""), files))
}

save_pullbacks <- function(x) {
  w <- study_pullbacks(x)
  w$signatures <- pullback_signature(x)
  output_tables(list(pullback_ranking = w$ranking, pullback_performance = w$scores,
                     pullback_intervals = w$inference, pullback_selection_adjustment = w$family,
                     pullback_hsi_daily = w$market_daily, pullback_trahk_daily = w$fund_daily))
  saveRDS(w, "strategy/results/pullback.rds", version = 3L)
  w
}

read_pullbacks <- function(x) {
  w <- readRDS("strategy/results/pullback.rds")
  stopifnot(identical(w$signatures, pullback_signature(x)))
  w
}

pullback_brief <- function(w) {
  row <- w$top
  list(rule = row[c("id", "trigger", "slow", "hold", "eligible")],
       positions = subset(w$scores, rule == row$id & fee_bps == trial_fee & 
                            period %in% c("Complete", "Out of sample", "Post-archive"))[
         c("instrument", "period", "trades", "net", "benchmark_net", "passes_both")])
}
