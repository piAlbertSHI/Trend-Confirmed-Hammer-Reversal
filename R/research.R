parameters <- expand.grid(a = seq(1, 22) / 10, b = seq(1, 22) / 10)
research_fee <- 5
min_train_trades <- 50L
block_lengths <- c(5L, 10L, 20L)
bootstrap_reps <- 1999L
null_reps <- 500L

periods_for <- function(full, last_archive) {
  data.frame(
    period = c("Archived sample", "Development", "Validation", "Historical holdout", "Post-archive"),
    start = c(min(full$Date), min(full$Date), as.Date(c("2017-01-01", "2020-01-01")),
              last_archive + 1L),
    end = c(last_archive, as.Date(c("2016-12-31", "2019-12-31")), last_archive, max(full$Date))
  )
}

period_rows <- function(bars, period) {
  which(bars$Date >= period$start & bars$Date <= period$end)
}

# Columns are independently executed rules. Rows are trading sessions.
grid_entries <- function(bars, f, grid = parameters) {
  n <- nrow(bars)
  vapply(seq_len(nrow(grid)), function(i) {
    hits <- signal_index(bars, f, grid$a[i], grid$b[i]) + 1L
    seq_len(n) %in% hits
  }, logical(n))
}

grid_performance <- function(entry_matrix, intraday_log, rows, fee = research_fee) {
  take <- entry_matrix[rows, , drop = FALSE]
  log_r <- intraday_log[rows]
  count <- colSums(take)
  gross <- as.numeric(crossprod(take, log_r))
  c <- fee / 10000
  net <- gross + count * (log1p(-c) - log1p(c))
  data.frame(trades = count, gross_log = gross, net_log = net,
             gross_compound = expm1(gross), net_compound = expm1(net),
             annualized_net = expm1(net / length(rows) * 252),
             mean_gross_bp = gross * 10000 / ifelse(count > 0, count, NA_real_))
}

select_rule <- function(score, grid = parameters, min_trades = min_train_trades) {
  eligible <- which(score$trades >= min_trades)
  stopifnot(length(eligible) > 0L)
  order_eligible <- order(-score$net_log[eligible], grid$a[eligible], grid$b[eligible])
  eligible[order_eligible[1L]]
}

cat_rule <- function(a, b) sprintf("(%.1f, %.1f)", a, b)

period_summary <- function(bars, f, periods, rules, costs) {
  tables <- lapply(seq_len(nrow(periods)), function(i) {
    rows <- period_rows(bars, periods[i, ])
    by_rule <- lapply(seq_len(nrow(rules)), function(j) {
      signal <- signal_index(bars, f, rules$a[j], rules$b[j])
      do.call(rbind, lapply(costs, function(cost) {
        run <- trade_log(bars, signal, fee_bps = cost, first = min(rows), last = max(rows))
        cbind(period = periods$period[i], rule = rules$rule[j], a = rules$a[j], b = rules$b[j],
              fee_bps = cost, trade_metrics(run, rows))
      }))
    })
    do.call(rbind, by_rule)
  })
  do.call(rbind, tables)
}

walk_forward <- function(bars, f, entries, initial_year = 2010L) {
  years <- as.integer(format(bars$Date, "%Y"))
  log_r <- log(bars$Close / bars$Open)
  primary <- signal_index(bars, f)
  daily <- data.frame(date = bars$Date, tuned = NA_real_, fixed = NA_real_)
  results <- lapply(seq.int(initial_year, max(years)), function(year) {
    train <- which(years < year)
    test <- which(years == year)
    scores <- grid_performance(entries, log_r, train)
    chosen <- select_rule(scores)
    rule <- parameters[chosen, ]
    start <- min(test)
    end <- max(test)
    tuned <- trade_log(bars, signal_index(bars, f, rule$a, rule$b),
                       fee_bps = research_fee, first = start, last = end)
    fixed <- trade_log(bars, primary, fee_bps = research_fee, first = start, last = end)
    list(rows = test, tuned = tuned$net_daily[test], fixed = fixed$net_daily[test],
         summary = data.frame(year, train_through = bars$Date[max(train)], a = rule$a, b = rule$b,
                    train_trades = scores$trades[chosen], sessions = length(test),
                    tuned_trades = nrow(tuned$trades), fixed_trades = nrow(fixed$trades),
                    tuned_net = expm1(sum(tuned$net_daily[test])),
                    fixed_net = expm1(sum(fixed$net_daily[test]))))
  })
  for (result in results) {
    daily$tuned[result$rows] <- result$tuned
    daily$fixed[result$rows] <- result$fixed
  }
  list(folds = do.call(rbind, lapply(results, "[[", "summary")), daily = daily[!is.na(daily$tuned), ])
}

# Each column is resampled with the same circular blocks, retaining cross-rule dependence.
circular_totals <- function(values, block, reps, seed) {
  values <- as.matrix(values)
  n <- nrow(values)
  stopifnot(n >= block, block >= 1L, reps >= 1L, all(is.finite(values)))
  set.seed(seed)
  full <- n %/% block
  rest <- n %% block
  block_sums <- function(k) {
    x <- rbind(values, values[seq_len(k - 1L), , drop = FALSE])
    cs <- rbind(numeric(ncol(x)), matrix(apply(x, 2L, cumsum), ncol = ncol(x)))
    cs[seq_len(n) + k, , drop = FALSE] - cs[seq_len(n), , drop = FALSE]
  }
  counts <- Matrix::sparseMatrix(
    i = sample.int(n, reps * full, replace = TRUE), j = rep(seq_len(reps), each = full),
    x = 1, dims = c(n, reps)
  )
  out <- as.matrix(Matrix::crossprod(counts, block_sums(block)))
  if (rest > 0L) out <- out + block_sums(rest)[sample.int(n, reps, replace = TRUE), , drop = FALSE]
  colnames(out) <- colnames(values)
  out
}

edge_stats <- function(log_r, hit, block, reps, seed) {
  count <- sum(hit)
  vals <- cbind(selected = hit * log_r, total = log_r, count = as.numeric(hit))
  gap <- function(selected, total, cases, n) {
    10000 * (selected / cases - (total - selected) / (n - cases))
  }
  n <- length(hit)
  observed <- gap(sum(vals[, 1L]), sum(log_r), count, n)
  stopifnot(count > 0L, count < n)
  boot <- circular_totals(vals, block, reps, seed)
  keep <- boot[, "count"] > 0 & boot[, "count"] < n
  sim <- gap(boot[keep, "selected"], boot[keep, "total"], boot[keep, "count"], n)
  ci <- quantile(sim, c(.025, .975), names = FALSE)
  naive <- stats::t.test(log_r[hit], log_r[!hit], alternative = "greater")
  data.frame(block, sessions = n, trades = count, edge_bp = observed,
             signal_bp = 10000 * mean(log_r[hit]), control_bp = 10000 * mean(log_r[!hit]),
             ci_low_bp = ci[1L], ci_high_bp = ci[2L], usable = sum(keep),
             p_one_sided = (1 + sum(sim - observed >= observed)) / (length(sim) + 1L),
             p_welch_iid = naive$p.value)
}

# Global null: no eligible rule outperforms its fixed-exposure intraday reference.
# All rules are recentered at zero for a least-favorable, max-t bootstrap.
grid_max_test <- function(entries, log_r, rows, block, reps, seed) {
  x <- entries[rows, , drop = FALSE]
  eligible <- colSums(x) >= min_train_trades
  x <- x[, eligible, drop = FALSE]
  matched <- sweep(x, 2L, colMeans(x), "-")
  excess <- sweep(matched, 1L, log_r[rows], "*")
  observed <- colMeans(excess)
  sim <- circular_totals(excess, block, reps, seed) / nrow(excess)
  sigma <- apply(sim, 2L, sd)
  stopifnot(all(is.finite(sigma)), all(sigma > 0))
  centered <- sweep(sim, 2L, observed, "-")
  t_boot <- sweep(centered, 2L, sigma, "/")
  max_sim <- apply(t_boot, 1L, max)
  t_obs <- observed / sigma
  best <- which.max(t_obs)
  i <- which(eligible)[best]
  data.frame(block, reps, eligible = sum(eligible), a = parameters$a[i], b = parameters$b[i],
             max_t = t_obs[best], edge_annual_bp = observed[best] * 252 * 10000,
             naive_p = (1 + sum(t_boot[, best] >= t_obs[best])) / (reps + 1L),
             adjusted_p = (1 + sum(max_sim >= t_obs[best])) / (reps + 1L))
}

latency_profile <- function(bars, f, max_lag = 5L, block = 10L, reps = bootstrap_reps) {
  dates <- seq.int(20L, nrow(bars) - max_lag)
  log_r <- log(bars$Close / bars$Open)
  hit <- dates %in% signal_index(bars, f)
  future <- sapply(seq_len(max_lag), function(k) log_r[dates + k])
  x <- cbind(future * hit, future, as.numeric(hit))
  n <- length(hit)
  effect <- function(s) {
    count <- s[, 2L * max_lag + 1L]
    selected <- s[, seq_len(max_lag), drop = FALSE]
    all <- s[, max_lag + seq_len(max_lag), drop = FALSE]
    10000 * (selected / count - (all - selected) / (n - count))
  }
  obs <- as.numeric(effect(matrix(colSums(x), nrow = 1L)))
  sim <- effect(circular_totals(x, block, reps, 8105L))
  sd_sim <- apply(sim, 2L, sd)
  centered_t <- sweep(sweep(sim, 2L, obs, "-"), 2L, sd_sim, "/")
  critical <- as.numeric(quantile(apply(abs(centered_t), 1L, max), .95))
  data.frame(lag = seq_len(max_lag), sessions = n, signals = sum(hit), block, reps,
             edge_bp = obs, low_bp = obs - critical * sd_sim, high_bp = obs + critical * sd_sim)
}

timing_comparison <- function(bars, f, source) {
  variants <- data.frame(
    rule = c("Mid k=1", "Mid k=2", "Mid k=3", "Mid hold 2", "Lower k=1", "Lower k=2",
             "Hammer only", "Mid only"),
    band = c(rep("mid", 4L), "lower", "lower", "none", "mid"),
    lag = c(1L, 2L, 3L, 1L, 1L, 2L, 1L, 1L),
    hold = c(1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L),
    shape = c(rep(TRUE, 7L), FALSE)
  )
  do.call(rbind, lapply(seq_len(nrow(variants)), function(i) {
    v <- variants[i, ]
    signals <- signal_index(bars, f, band = v$band, shape = v$shape)
    fit <- trade_log(bars, signals, v$lag, v$hold, fee_bps = research_fee)
    cbind(source, v, trade_metrics(fit, seq_len(nrow(bars))))
  }))
}

benchmarks <- function(bars, primary, periods) {
  do.call(rbind, lapply(seq_len(nrow(periods)), function(i) {
    rows <- period_rows(bars, periods[i, ])
    trade <- rows %in% primary$trades$entry
    r <- log(bars$Close[rows] / bars$Open[rows])
    buy_hold <- c(r[1L], diff(log(bars$Close[rows])))
    reference <- list(Cash = 0 * r, "Buy and hold" = buy_hold, "Daily intraday" = r,
                      "Exposure-matched reference" = mean(trade) * r,
                      "Original rule, gross" = primary$gross_daily[rows])
    result <- t(vapply(reference, risk_metrics, numeric(5L)))
    data.frame(period = periods$period[i], series = names(reference),
               exposure = c(0, 1, 1, mean(trade), mean(trade)), result, row.names = NULL)
  }))
}

volatility_regimes <- function(bars, primary, development) {
  log_r <- log(bars$Close / bars$Open)
  volatility <- c(NA_real_, as.numeric(TTR::runSD(diff(log(bars$Close)), 20L)))
  threshold <- median(volatility[development], na.rm = TRUE)
  previous_vol <- c(NA_real_, head(volatility, -1L))
  in_trade <- seq_len(nrow(bars)) %in% primary$trades$entry
  regime <- ifelse(previous_vol > threshold, "High", "Low")
  out <- lapply(c("Low", "High"), function(label) {
    rows <- which(regime == label & seq_len(nrow(bars)) > max(development))
    traded <- log_r[rows][in_trade[rows]]
    untraded <- log_r[rows][!in_trade[rows]]
    data.frame(regime = label, threshold_annual_vol = threshold * sqrt(252), sessions = length(rows),
               trades = length(traded), mean_trade_bp = mean(traded) * 10000,
               edge_bp = (mean(traded) - mean(untraded)) * 10000,
               gross_compound = expm1(sum(traded)), net_compound = expm1(sum(primary$net_daily[rows])))
  })
  do.call(rbind, out)
}
