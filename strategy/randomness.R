# Absolute and active log returns on the same synthetic market.
path_metrics <- function(bars, spans, main, combined) {
  spec <- position_spec(bars, rbind(main, combined))
  selected <- next_open_positions(close_targets(spec, main))
  combined_state <- if (main$id == combined$id) selected else
    next_open_positions(close_targets(spec, combined))
  core <- next_open_positions(close_targets(spec, combined, "core"))
  pullback <- next_open_positions(pullback_target(pullback_spec(bars), followup_rule()))
  ret <- return_components(bars$Open, bars$Close)
  market <- rep(TRUE, nrow(bars))
  value <- function(w, r) sum(portfolio_log(ret, w, r, trial_fee)$daily)
  do.call(rbind, lapply(seq_len(nrow(spans)), function(i) {
    p <- spans[i, ]
    rows <- period_rows(bars, p)
    plan <- portfolio_log(ret, selected, rows, trial_fee)
    benchmark <- portfolio_log(ret, market, rows, trial_fee)
    net <- sum(plan$daily)
    bm <- sum(benchmark$daily)
    matched <- sum(log1p(mean(plan$weight) * expm1(benchmark$daily + benchmark$cost_log)))
    reversion <- portfolio_log(ret, pullback, rows, trial_fee)
    rev_log <- sum(reversion$daily)
    market_simple <- expm1(benchmark$daily + benchmark$cost_log)
    rev_matched <- sum(log1p(mean(reversion$weight) * market_simple))
    data.frame(period = p$period, net_log = net, index_log = bm, active_log = net - bm,
               p06_net_log = rev_log, p06_active_log = rev_log - bm,
               p06_exposure_log = rev_log - rev_matched, p06_exposure = mean(reversion$weight),
               p06_passes_both = pass_objectives(rev_log, bm),
               exposure_active_log = net - matched,
               hammer_log = value(combined_state, rows) - value(core, rows),
               exposure = mean(plan$weight), trades = sum(plan$buys),
               passes_both = pass_objectives(net, bm))
  }))
}

continuation_candles <- function(known, dates, path, donor, reference) {
  last <- tail(known$Close, 1L)
  close <- last * exp(cumsum(path))
  open <- c(last, head(close, -1L)) * exp(reference$gap[donor])
  high <- pmax(open, close) * exp(reference$upper[donor])
  low <- pmin(open, close) * exp(-reference$lower[donor])
  run <- rbind(known, data.frame(Date = dates, Open = open, High = high, Low = low, Close = close))
  validate_bars(run)
  run
}

simulate_processes <- function(bars, spans, main, combined, reps = simulations_per_process) {
  cutoff <- as.Date("2019-12-31")
  fields <- c("Date", ohlc_fields)
  old <- bars[bars$Date <= cutoff, fields]
  future <- bars[bars$Date > cutoff, fields]
  warm <- tail(old, 500L)
  dates <- future$Date
  horizon <- length(dates)
  observed <- path_metrics(bars[fields], spans, main, combined)
  initialized <- path_metrics(rbind(warm, future), spans, main, combined)
  stopifnot(isTRUE(all.equal(observed, initialized, tolerance = 1e-12)))
  reference <- candle_reference(old)
  r <- reference$returns
  ar <- ar_parameters(r)
  methods <- c("Random walk", "AR(1)", "GARCH-M", "EGARCH")

  pieces <- lapply(seq_along(methods), function(j) {
    model <- methods[j]
    set.seed(241201L + j)
    if (j == 1L) {
      coeff <- data.frame(model, term = "daily_drift_pct", estimate = 100 * mean(r), se = NA_real_)
      diag <- calibration_diagnostics(model, r - mean(r), notes = "Independent empirical OHLC tuples")
    } else if (j == 2L) {
      coeff <- ar$coefficients
      diag <- calibration_diagnostics(model, ar$innovation, ar = ar$phi, solver = "ML",
                                      notes = "Empirical innovations; start at last calibrated return")
    } else {
      fit <- fit_volatility(r, model)
      params <- fit$fit@fit$matcoef
      coeff <- data.frame(model, term = rownames(params), estimate = params[, 1L], se = params[, 2L],
                          row.names = NULL)
      diag <- calibration_diagnostics(model, fit$fit@fit$z, solver = fit$solver,
                                      persistence = rugarch::persistence(fit$fit), notes = fit$messages)
      package_seed <- sample.int(100000000L, reps)
      sims <- rugarch::ugarchsim(fit$fit, n.sim = horizon, n.start = 0L, m.sim = reps,
                                 startMethod = "sample", rseed = package_seed)
      paths <- sims@simulation$seriesSim / 100
      stopifnot(nrow(paths) == horizon, ncol(paths) == reps)
      set.seed(482032L + j)
    }
    one <- lapply(seq_len(reps), function(i) {
      if (j == 1L) {
        donor <- sample.int(length(r), horizon, replace = TRUE)
        path <- r[donor]
      } else {
        path <- if (j == 2L) {
          innovation <- sample(ar$innovation, horizon, replace = TRUE)
          ar$mean + as.numeric(stats::filter(innovation, ar$phi, method = "recursive",
                                             init = tail(r, 1L) - ar$mean))
        } else paths[, i]
        donor <- match_shapes(path, reference)
      }
      sim <- continuation_candles(warm, dates, path, donor, reference)
      cbind(model, replication = i, path_metrics(sim, spans, main, combined))
    })
    list(draws = do.call(rbind, one), coefficients = coeff, diagnostics = diag)
  })
  metadata <- data.frame(calibration_first = min(old$Date), calibration_last = max(old$Date),
                         simulation_first = min(future$Date), simulation_last = max(future$Date),
                         trading_sessions = horizon, warmup_sessions = nrow(warm), paths_per_model = reps)
  bind <- function(field) do.call(rbind, lapply(pieces, "[[", field))
  draws <- bind("draws")
  stopifnot(nrow(draws) == reps * length(methods) * nrow(spans),
            all(is.finite(draws$net_log)), all(is.finite(draws$active_log)))
  list(draws = draws, coefficients = bind("coefficients"), diagnostics = bind("diagnostics"),
       observed = observed, metadata = metadata)
}

strategy_null_summary <- function(sim) {
  g <- split(sim$draws, list(sim$draws$period, sim$draws$model), drop = TRUE)
  columns <- c("net_log", "active_log", "exposure_active_log", "hammer_log",
               "p06_net_log", "p06_active_log", "p06_exposure_log")
  labels <- c("Selected net", "Selected vs index", "Selected vs equal exposure", "Hammer overlay vs trend",
              "P06 net", "P06 vs index", "P06 vs equal exposure")
  do.call(rbind, unname(lapply(g, function(d) {
    p <- d$period[1L]
    obs <- sim$observed[sim$observed$period == p, ]
    do.call(rbind, lapply(seq_along(columns), function(j) {
      stat <- d[[columns[j]]]
      val <- obs[[columns[j]]]
      count <- sum(stat >= val)
      interval <- stats::binom.test(count, nrow(d))$conf.int
      dist <- quantile(stat, c(.025, .5, .975), names = FALSE)
      continuation <- startsWith(columns[j], "p06_")
      data.frame(period = p, model = d$model[1L], metric = labels[j],
                 observed_log = val, median_log = dist[2L], low_log = dist[1L], high_log = dist[3L],
                 draws = nrow(d), at_least_observed = count,
                 p_one_sided = (count + 1L) / (nrow(d) + 1L),
                 mc_low = interval[1L], mc_high = interval[2L],
                 fraction_profitable_and_ahead = mean(if (continuation) d$p06_passes_both else d$passes_both),
                 median_exposure = median(if (continuation) d$p06_exposure else d$exposure))
    }))
  })))
}

# Conditional reference on realized prices, preserving the number of setups by year and core state.
timing_permutation <- function(bars, periods, rule, reps = 999L) {
  spec <- position_spec(bars, rule)
  slow <- spec$avg[[as.character(rule$slow)]]
  core <- trend_target(spec$close, slow, rule$buffer)
  actual <- confirmed_holding(spec, slow, rule$hold)
  rows <- period_rows(bars, periods[periods$period == "Out of sample", ])
  can <- which(seq_len(nrow(bars)) >= min(rows) & !is.na(slow) & spec$close >= .96 * slow & spec$pullback)
  strata <- split(can, list(format(bars$Date[can], "%Y"), core[can]), drop = TRUE)
  counts <- vapply(strata, function(ii) sum(actual$setup[ii]), 0L)
  stopifnot(sum(counts) > 0L)
  r <- return_components(bars$Open, bars$Close)
  costed <- function(target) sum(portfolio_log(r, next_open_positions(target), rows, trial_fee)$daily)
  without <- costed(core)
  observed <- costed(core | actual$active) - without
  set.seed(731051L)
  stat <- vapply(seq_len(reps), function(i) {
    sample_spec <- spec
    sample_spec$shape[can] <- FALSE
    for (j in seq_along(strata)) {
      pool <- strata[[j]]
      flagged <- pool[sample.int(length(pool), counts[j])]
      sample_spec$shape[flagged] <- TRUE
    }
    draw <- confirmed_holding(sample_spec, slow, rule$hold)$active
    costed(core | draw) - without
  }, 0.0)
  list(summary = data.frame(observed_log = observed, eligible_days = length(can),
                            chosen_setups = sum(counts), strata = length(strata),
                            reps = reps, median_log = median(stat),
                            p_one_sided = (1 + sum(stat >= observed)) / (reps + 1L)),
       draws = data.frame(replication = seq_along(stat), incremental_log = stat))
}

randomness_signature <- function() {
  more <- c("strategy/randomness.R", "strategy/pullback.R", "strategy/followup.R", "R/legacy.R", "R/null_models.R")
  setNames(vapply(more, function(file) digest::digest(file = file, algo = "sha256"), ""), more)
}

randomness_study <- function(x) {
  spans <- x$spans[x$spans$period %in% c("Out of sample", "Post-archive"), ]
  sim <- simulate_processes(x$market$full, spans, x$primary, x$combined)
  sim$summary <- strategy_null_summary(sim)
  sim$random_entry <- timing_permutation(x$market$full, spans, x$combined)
  sim$signatures <- c(x$signatures, randomness_signature())
  sim
}

save_randomness <- function(x) {
  output_tables(list(model_paths = x$draws, model_tail_tests = x$summary,
                     model_calibration = x$metadata, model_diagnostics = x$diagnostics,
                     model_coefficients = x$coefficients, observed_stats = x$observed,
                     hammer_permutation = x$random_entry$summary,
                     hammer_permutation_draws = x$random_entry$draws))
  saveRDS(x, "strategy/results/randomness.rds", version = 3L)
  invisible(x)
}

read_randomness <- function(backtest) {
  x <- readRDS("strategy/results/randomness.rds")
  stopifnot(identical(x$signatures, c(backtest$signatures, randomness_signature())))
  x
}
