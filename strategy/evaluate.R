trial_fee <- 5
train_fee <- 10
strategy_costs <- c(0, 2, 5, 10, 20, 30)
strategy_blocks <- c(10L, 20L, 40L)
strategy_boot <- 1999L
simulations_per_process <- 500L
warmup <- 220L

# The price bar that chooses an open on the first test day is already available.
study_spans <- function(bars, original_last) {
  first <- bars$Date[warmup + 1L]
  last <- max(bars$Date)
  data.frame(
    period = c("Complete", "Development", "Validation", "Historical holdout", "Post-archive", "Out of sample"),
    start = c(rep(first, 2L), as.Date(c("2017-01-01", "2020-01-01")), original_last + 1L, as.Date("2020-01-01")),
    end = c(last, as.Date(c("2016-12-31", "2019-12-31")), original_last, last, last)
  )
}

baseline_diagnosis <- function(market) {
  bars <- market$full
  r <- return_components(bars$Open, bars$Close)
  signal <- next_open_positions(seq_len(nrow(bars)) %in% signal_index(bars, features(bars)))
  spans <- periods_for(bars, max(market$archive$Date))
  do.call(rbind, lapply(seq_len(nrow(spans)), function(i) {
    rows <- period_rows(bars, spans[i, ])
    old <- intraday_reference(r, signal, rows, trial_fee)
    market_run <- portfolio_log(r, rep(TRUE, nrow(bars)), rows, 0)
    data.frame(period = spans$period[i], sessions = length(rows), trades = sum(old$buys),
               share = mean(old$weight), intraday_sum_log = sum(market_run$gross_intraday),
               overnight_sum_log = sum(market_run$gross_gap),
               market_gross = expm1(sum(market_run$daily)),
               gross_bp_per_signal = sum(old$gross_intraday) / sum(old$buys) * 10000,
               cost_bp_per_signal = sum(old$cost_log) / sum(old$buys) * 10000,
               next_open_gap_bp_per_signal = mean(r$gap[rows][old$weight]) * 10000,
               old_net = expm1(sum(old$daily)))
  }))
}

position_matrix <- function(bars, rules) {
  spec <- position_spec(bars, rules)
  out <- vapply(seq_len(nrow(rules)), function(i) {
    next_open_positions(close_targets(spec, rules[i, ]))
  }, logical(nrow(bars)))
  colnames(out) <- rules$id
  out
}

training_blocks <- function(rows, end_dates = NULL, date = NULL) {
  if (!is.null(end_dates)) {
    group <- 1L + rowSums(outer(date[rows], end_dates, ">"))
  } else {
    group <- pmin(3L, 1L + ((seq_along(rows) - 1L) * 3L) %/% length(rows))
  }
  split(rows, group)
}

training_table <- function(ret, pos, rules, rows, folds) {
  market <- rep(TRUE, nrow(pos))
  benchmark <- function(rows) sum(portfolio_log(ret, market, rows, train_fee)$daily)
  targets <- vapply(folds, benchmark, 0.0)
  bench <- benchmark(rows)
  each <- lapply(seq_len(ncol(pos)), function(j) {
    state <- pos[, j]
    run <- portfolio_log(ret, state, rows, train_fee)
    full <- sum(run$daily)
    stages <- vapply(folds, function(k) sum(portfolio_log(ret, state, k, train_fee)$daily), 0.0)
    median_log <- median(252 * (stages - targets) / lengths(folds))
    water <- risk_metrics(run$daily)[["max_drawdown"]]
    row <- rules[j, , drop = FALSE]
    trade_count <- sum(run$buys)
    exposure <- mean(run$weight)
    threshold <- if (row$family == "Confirmed hammer") .02 else .10
    cbind(row, data.frame(trades = trade_count, exposure = exposure,
          eligible = trade_count >= 12L & exposure >= threshold & exposure <= .90,
          passes_development = pass_objectives(full, bench),
          development_net = expm1(full), buy_hold_net = expm1(bench),
          dev_active_log = full - bench, successful_blocks = sum(pass_objectives(stages, targets)),
          median_block_active_annual_log = median_log, drawdown = water))
  })
  items <- do.call(rbind, each)
  priority <- order(!items$eligible, !items$passes_development, -items$successful_blocks,
                    -items$median_block_active_annual_log, -items$dev_active_log,
                    -items$drawdown, items$trades, items$id)
  items$rank <- match(seq_len(nrow(items)), priority)
  items
}

frozen_choices <- function(fit) {
  ranks <- fit[order(fit$rank), , drop = FALSE]
  stopifnot(ranks$eligible[1L])
  families <- ranks[!duplicated(ranks$family), , drop = FALSE]
  head <- cbind(role = "Selected", ranks[1L, ])
  rbind(head, cbind(role = paste0(families$family, " best"), families))
}

evaluate_window <- function(run, benchmark, span, label, instrument, fee, exposure_reference = TRUE) {
  r <- risk_metrics(run$daily)
  b <- risk_metrics(benchmark$daily)
  reference <- if (exposure_reference) {
    sum(log1p(mean(run$weight) * expm1(benchmark$daily + benchmark$cost_log)))
  } else NA_real_
  data.frame(instrument, period = span, rule = label, fee_bps = fee,
             sessions = length(run$daily), trades = sum(run$buys), exposure = mean(run$weight),
             holding_days = if (sum(run$buys)) sum(run$weight) / sum(run$buys) else NA_real_,
             net = r[["compound"]], benchmark_net = b[["compound"]],
             excess_pp = 100 * (r[["compound"]] - b[["compound"]]),
             active_log = sum(run$daily - benchmark$daily),
             matched_active_log = sum(run$daily) - reference,
             annualized = r[["annualized"]], annual_vol = r[["annual_vol"]],
             sharpe = r[["sharpe"]], drawdown = r[["max_drawdown"]],
             benchmark_drawdown = b[["max_drawdown"]],
             cost_log = sum(run$cost_log), gross_gap_log = sum(run$gross_gap),
             gross_intraday_log = sum(run$gross_intraday),
             passes_both = pass_objectives(sum(run$daily), sum(benchmark$daily)))
}

score_universe <- function(bars, positions, spans, name, fees = strategy_costs) {
  ret <- return_components(bars$Open, bars$Close)
  market <- rep(TRUE, nrow(bars))
  do.call(rbind, lapply(seq_len(nrow(spans)), function(j) {
    span <- spans[j, ]
    rows <- period_rows(bars, span)
    if (length(rows) < 20L) return(NULL)
    do.call(rbind, lapply(fees, function(fee) {
      buy_hold <- portfolio_log(ret, market, rows, fee)
      do.call(rbind, lapply(names(positions), function(label) {
        state <- positions[[label]]
        run <- if (label == "Original intraday") intraday_reference(ret, state, rows, fee) else
          portfolio_log(ret, state, rows, fee)
        evaluate_window(run, buy_hold, span$period, label, name, fee)
      }))
    }))
  }))
}

annual_selection <- function(bars, rules, pos, spans) {
  years <- as.integer(format(bars$Date, "%Y"))
  ret <- return_components(bars$Open, bars$Close)
  ids <- sort(unique(years[years >= 2010L]))
  chosen <- logical(nrow(bars))
  meta <- do.call(rbind, lapply(ids, function(year) {
    train <- which(years < year & seq_along(years) > warmup)
    test <- which(years == year)
    fit <- training_table(ret, pos, rules, train, training_blocks(train))
    top <- frozen_choices(fit)[1L, ]
    chosen[test] <<- pos[test, top$id]
    data.frame(year, rule = top$id, family = top$family, train_end = max(bars$Date[train]),
               first = min(bars$Date[test]), sessions = length(test),
               pass_training = top$passes_development)
  }))
  selected <- which(years >= 2010L)
  run <- portfolio_log(ret, chosen, selected, trial_fee)
  reference <- portfolio_log(ret, rep(TRUE, nrow(bars)), selected, trial_fee)
  d <- data.frame(date = bars$Date[selected], year = years[selected],
                  strategy_log = run$daily, benchmark_log = reference$daily,
                  exposure = run$weight, buys = run$buys, sells = run$sells)
  meta$net <- vapply(meta$year, function(y) expm1(sum(d$strategy_log[d$year == y])), 0.0)
  meta$market <- vapply(meta$year, function(y) expm1(sum(d$benchmark_log[d$year == y])), 0.0)
  list(annual = meta, daily = d,
       summary = evaluate_window(run, reference, "2010 onward", "Annual reselection", "^HSI", trial_fee),
       chosen = chosen)
}

block_inference <- function(statistics) {
  do.call(rbind, lapply(strategy_blocks, function(k) {
    values <- as.matrix(statistics)
    observed <- colMeans(values)
    samples <- circular_totals(values, k, strategy_boot, 32026L + k) / nrow(values)
    q <- t(apply(samples, 2L, quantile, probs = c(.025, .975), names = FALSE))
    p <- (1 + colSums(sweep(samples, 2L, observed, "-") >=
                      matrix(observed, nrow(samples), ncol(samples), byrow = TRUE))) / (nrow(samples) + 1L)
    data.frame(metric = colnames(values), block = k, reps = nrow(samples),
               observed_annual_log = 252 * observed, low_annual_log = 252 * q[, 1L],
               high_annual_log = 252 * q[, 2L], p_one_sided = p, row.names = NULL)
  }))
}

family_inference <- function(ret, positions, rows, fee = train_fee) {
  reference <- portfolio_log(ret, rep(TRUE, nrow(positions)), rows, fee)$daily
  differential <- vapply(seq_len(ncol(positions)), function(i) {
    portfolio_log(ret, positions[, i], rows, fee)$daily - reference
  }, numeric(length(rows)))
  do.call(rbind, lapply(strategy_blocks, function(k) {
    obs <- colMeans(differential)
    sim <- circular_totals(differential, k, strategy_boot, 41206L + k) / nrow(differential)
    se <- apply(sim, 2L, sd)
    stopifnot(all(is.finite(se)), all(se > 0))
    t_draw <- sweep(sweep(sim, 2L, obs, "-"), 2L, se, "/")
    stat <- obs / se
    i <- which.max(stat)
    data.frame(block = k, variants = ncol(positions), statistic = stat[i], best_by_t = colnames(positions)[i],
               best_annual_active_log = 252 * obs[i],
               unadjusted_p = (1 + sum(t_draw[, i] >= stat[i])) / (nrow(sim) + 1L),
               adjusted_p = (1 + sum(apply(t_draw, 1L, max) >= stat[i])) / (nrow(sim) + 1L))
  }))
}

comparison_daily <- function(bars, states, span) {
  ret <- return_components(bars$Open, bars$Close)
  rows <- period_rows(bars, span)
  d <- data.frame(date = bars$Date[rows], open = bars$Open[rows], close = bars$Close[rows])
  for (label in names(states)) {
    held <- states[[label]]
    run <- if (label == "Original intraday") intraday_reference(ret, held, rows, trial_fee) else
      portfolio_log(ret, held, rows, trial_fee)
    d[[paste0(label, " log")]] <- run$daily
    d[[paste0(label, " held")]] <- run$weight
  }
  d
}
