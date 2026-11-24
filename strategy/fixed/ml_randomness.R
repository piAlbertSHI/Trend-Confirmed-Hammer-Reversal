ml_overlay_path_statistics <- function(bars, rows, primary, overlay_context) {
  if (!"Volume" %in% names(bars)) {
    bars$Volume <- overlay_context$volume[seq_len(nrow(bars))]
  }
  spec <- position_spec(bars, primary)
  base_state <- next_open_positions(close_targets(spec, primary))
  daily_build <- build_ml_table(bars)
  daily_build$data <- assign_ml_split(daily_build$data)
  attr(daily_build$data, "ml_feature_names") <- daily_build$feature_names
  daily <- build_daily_economic_table(daily_build$data)
  events <- tchr_trade_events(bars, daily, base_state, rows, trial_fee)
  predictions <- transfer_tchr_ml_scores_frozen(
    events$events, daily, overlay_context$saved,
    overlay_context$thresholds, overlay_context$run_id
  )
  decision <- rep(NA, nrow(events$events))
  decision[predictions$entry_id] <- predictions$selected
  overlay_state <- apply_tchr_ml_entry_filter(
    base_state, events$entry_bar, decision
  )
  ret <- return_components(bars$Open, bars$Close)
  base <- portfolio_log(ret, base_state, rows, trial_fee)
  overlay <- portfolio_log(ret, overlay_state, rows, trial_fee)
  core_state <- next_open_positions(close_targets(spec, primary, "core"))
  core <- portfolio_log(ret, core_state, rows, trial_fee)
  benchmark <- portfolio_log(ret, rep(TRUE, nrow(bars)), rows, trial_fee)
  matched <- sum(log1p(mean(overlay$weight) *
                        expm1(benchmark$daily + benchmark$cost_log)))
  overlay_risk <- risk_metrics(overlay$daily)
  base_net <- sum(base$daily)
  overlay_net <- sum(overlay$daily)
  benchmark_net <- sum(benchmark$daily)
  c(
    net_log = base_net,
    index_log = benchmark_net,
    active_log = base_net - benchmark_net,
    matched_log = base_net - sum(log1p(mean(base$weight) *
                                      expm1(benchmark$daily + benchmark$cost_log))),
    overlay_log = base_net - sum(core$daily),
    ml_net_log = overlay_net,
    ml_active_log = overlay_net - benchmark_net,
    ml_matched_log = overlay_net - matched,
    ml_overlay_log = overlay_net - sum(core$daily),
    ml_drawdown = unname(overlay_risk[["max_drawdown"]]),
    ml_trades = sum(overlay$buys),
    ml_exposure = mean(overlay$weight),
    ml_both = as.numeric(pass_objectives(overlay_net, benchmark_net))
  )
}

ml_market_null <- function(result, overlay_context, n_paths = simulations_per_process) {
  setup <- result$setup
  rows <- setup$rows
  fields <- c("Date", ohlc_fields)
  warm <- setup$bars[seq_len(min(rows) - 1L), fields]
  actual <- setup$bars[rows, fields]
  dates <- actual$Date
  horizon <- length(rows)
  observed <- ml_overlay_path_statistics(
    setup$bars[fields], rows, setup$primary, overlay_context
  )
  reference <- candle_reference(actual)
  returns <- reference$returns
  ar <- ar_parameters(returns)
  labels <- c("Random walk", "AR(1)", "GARCH-M", "EGARCH")
  out <- lapply(seq_along(labels), function(k) {
    model <- labels[k]
    set.seed(42126L + k)
    if (k == 1L) {
      coeff <- data.frame(model, term = "drift_pct", estimate = 100 * mean(returns), se = NA_real_)
      diag <- calibration_diagnostics(model, returns - mean(returns), notes = "Empirical daily tuples")
    } else if (k == 2L) {
      coeff <- ar$coefficients
      diag <- calibration_diagnostics(model, ar$innovation, solver = "ML", ar = ar$phi,
                                      notes = "Centered empirical innovations")
    } else {
      fit <- if (model == "EGARCH") fixed_egarch_fit(returns) else fit_volatility(returns, model)
      opt <- fit$optimization
      est <- fit$fit@fit$matcoef
      coeff <- data.frame(model, term = rownames(est), estimate = est[, 1L], se = est[, 2L], row.names = NULL)
      diag <- calibration_diagnostics(model, fit$fit@fit$z, solver = fit$solver,
                                      persistence = rugarch::persistence(fit$fit), notes = fit$messages)
      seeds <- sample.int(100000000L, n_paths)
      sim_paths <- rugarch::ugarchsim(fit$fit, n.sim = horizon, n.start = 500L,
                                      m.sim = n_paths, rseed = seeds, startMethod = "unconditional")
      paths <- sim_paths@simulation$seriesSim / 100
      set.seed(52260L + k)
    }
    raw <- vapply(seq_len(n_paths), function(i) {
      if (k == 1L) {
        donor <- sample.int(length(returns), horizon, replace = TRUE)
        path <- returns[donor]
      } else {
        path <- if (k == 2L) {
          innovations <- sample(ar$innovation, horizon + 250L, replace = TRUE)
          ar$mean + tail(as.numeric(stats::filter(
            innovations, ar$phi, method = "recursive"
          )), horizon)
        } else paths[, i]
        donor <- match_shapes(path, reference)
      }
      simulated <- continuation_candles(warm, dates, path, donor, reference)
      ml_overlay_path_statistics(simulated, rows, setup$primary, overlay_context)
    }, observed)
    stopifnot(all(is.finite(raw)))
    list(
      draws = cbind(model, path = seq_len(n_paths), as.data.frame(t(raw))),
      diagnostics = diag,
      coefficients = coeff,
      optimization = if (exists("opt")) opt else NULL
    )
  })
  bind <- function(name) do.call(rbind, lapply(out, "[[", name))
  draws <- bind("draws")
  stopifnot(all(table(draws$model) == n_paths))
  targets <- c("net_log", "active_log", "matched_log", "overlay_log",
               "ml_net_log", "ml_active_log", "ml_matched_log", "ml_overlay_log",
               "ml_drawdown", "ml_trades", "ml_exposure", "ml_both")
  results <- do.call(rbind, lapply(labels, function(label) {
    d <- draws[draws$model == label, , drop = FALSE]
    do.call(rbind, lapply(targets, function(metric) {
      real <- observed[[metric]]
      z <- d[[metric]]
      if (metric == "ml_both") {
        return(data.frame(
          model = label, metric, observed = real, paths = n_paths,
          exceedances = NA_integer_, p = NA_real_,
          mc_low = NA_real_, mc_high = NA_real_,
          q025 = NA_real_, median = NA_real_, q975 = NA_real_,
          positive_and_ahead = mean(z), stringsAsFactors = FALSE
        ))
      }
      if (metric %in% c("ml_trades", "ml_exposure")) {
        return(data.frame(
          model = label, metric, observed = real, paths = n_paths,
          exceedances = NA_integer_, p = NA_real_,
          mc_low = NA_real_, mc_high = NA_real_,
          q025 = quantile(z, .025, names = FALSE), median = median(z),
          q975 = quantile(z, .975, names = FALSE),
          positive_and_ahead = NA_real_, stringsAsFactors = FALSE
        ))
      }
      count <- sum(z >= real)
      band <- stats::binom.test(count, n_paths)$conf.int
      limits <- quantile(z, c(.025, .5, .975), names = FALSE)
      data.frame(model = label, metric, observed = real, paths = n_paths,
                 exceedances = count, p = (count + 1L) / (n_paths + 1L),
                 mc_low = band[1L], mc_high = band[2L],
                 q025 = limits[1L], median = limits[2L], q975 = limits[3L],
                 positive_and_ahead = mean(d$ml_both))
    }))
  }))
  fit_span <- data.frame(first = min(dates), last = max(dates), sessions = horizon,
                         calibrated_close_return_observations = length(returns),
                         paths_per_model = n_paths)
  list(draws = draws, summary = results, coefficients = bind("coefficients"),
       diagnostics = bind("diagnostics"), optimization = bind("optimization"),
       observed = observed, span = fit_span)
}
