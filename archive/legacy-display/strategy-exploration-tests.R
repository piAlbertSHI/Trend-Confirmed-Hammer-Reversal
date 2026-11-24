strategy_checks <- function() {
  # Entry and exit prices are on the next open; an exiting long earns that day's gap.
  open <- c(101, 100, 107, 130, 115, 103, 100, 109)
  close <- c(99, 106, 124, 118, 103, 102, 105, 108)
  ret <- return_components(open, close)
  logical_signal <- c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
  held <- next_open_positions(logical_signal)
  stopifnot(identical(which(held), c(2L, 3L, 6L, 7L, 8L)))
  d <- portfolio_log(ret, held, 1:8, 10)
  net <- (130 / 100) * (108 / 103) * ((1 - .001) / (1 + .001))^2
  stopifnot(abs(exp(sum(d$daily)) - net) < 1e-13,
            abs(d$gross_gap[4L] - log(130 / 124)) < 1e-14, d$gross_intraday[4L] == 0,
            sum(d$buys) == 2L, sum(d$sells) == 1L, d$exit_close)
  sub <- portfolio_log(ret, held, 3:7, 10)
  expected <- (130 / 107) * (105 / 103) * ((1 - .001) / (1 + .001))^2
  stopifnot(abs(exp(sum(sub$daily)) - expected) < 1e-13, sub$gross_gap[1L] == 0)
  diary <- position_trades(d, as.Date("2020-01-01") + 0:7, open, close, 10)
  stopifnot(abs(prod(1 + diary$net) - net) < 1e-13,
            identical(diary$exit_at, c("open", "close")))

  near <- rep(95, 11)
  toy <- list(close = c(95, 96, 96, 99, 100, 98, 97, 80, 99, 100, 101),
              high = c(96, 98, 97, 100, 101, 99, 98, 85, 100, 101, 102),
              low = c(94, 92, 94, 98, 96, 95, 96, 79, 98, 99, 100),
              pullback = seq_len(11) %in% c(2L, 7L),
              shape = seq_len(11) %in% c(2L, 7L))
  s <- confirmed_holding(toy, near, 3L)
  stopifnot(identical(which(s$confirmed), c(4L, 9L)),
            identical(which(s$active), c(4L, 5L, 6L, 9L, 10L, 11L)),
            identical(head(which(next_open_positions(s$active)), 3L), 5:7))
  altered <- toy
  altered$close[5:11] <- 110
  again <- confirmed_holding(altered, near, 3L)
  stopifnot(identical(head(s$active, 4), head(again$active, 4)))
  stopifnot(identical(trend_target(c(100, 102, 100.5, 98, 100.5, 102), rep(100, 6), .01),
                      c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)))

  rules <- configuration_grid()
  market <- load_market()$full
  stopifnot(nrow(rules) == 33L, !anyDuplicated(rules$id))
  pre <- market[1:2600, ]
  prefix <- position_matrix(pre, rules)
  full <- position_matrix(market[1:3000, ], rules)
  stopifnot(identical(prefix, full[1:2600, , drop = FALSE]))
  fund <- align_fund(market, read_fund())
  stopifnot(all(market$Date[fund$index_rows] == fund$prices$Date),
            all(fund$prices$Open > 0), all(fund$prices$Close > 0))
  dry <- list(close = c(100, 96, 95, 90, 89, 100), mean_five = rep(101, 6),
              atr = rep(2, 6), windows = list("0" = rep(0, 6)),
              trigger = list(test = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)))
  rule <- data.frame(trigger = "test", slow = 0L, hold = 5L)
  stopifnot(identical(which(pullback_target(dry, rule)), c(2L, 3L)),
            identical(which(next_open_positions(pullback_target(dry, rule))), c(3L, 4L)))
  all_pullbacks <- pullback_grid()
  stopifnot(nrow(all_pullbacks) == 18L, !anyDuplicated(all_pullbacks$id),
            identical(pullback_matrix(pre, all_pullbacks),
                      pullback_matrix(market[1:3000, ], all_pullbacks)[1:2600, , drop = FALSE]))
  histories <- early_market(load_market()$archive)
  stopifnot(histories$audit$count_overlaps == 56L,
            max(histories$prices$Date) < min(market$Date))
  invisible(TRUE)
}
