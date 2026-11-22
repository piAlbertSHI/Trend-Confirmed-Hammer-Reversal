fixed_egarch_fit <- function(returns) {
  spec <- volatility_spec("EGARCH")
  methods <- c("solnp", "nlminb")
  fits <- lapply(methods, function(solver) {
    tryCatch(suppressWarnings(rugarch::ugarchfit(spec, 100 * returns, solver = solver)), error = identity)
  })
  log <- do.call(rbind, lapply(seq_along(fits), function(i) {
    v <- fits[[i]]
    if (inherits(v, "error")) {
      return(data.frame(solver = methods[i], converged = FALSE, log_likelihood = NA_real_,
                        shape = NA_real_, residual_sd = NA_real_, note = conditionMessage(v)))
    }
    data.frame(solver = methods[i], converged = isTRUE(v@fit$convergence == 0L),
               log_likelihood = v@fit$LLH, shape = v@fit$coef[["shape"]],
               residual_sd = sd(v@fit$z), note = "")
  }))
  eligible <- which(log$converged & is.finite(log$log_likelihood))
  stopifnot(length(eligible) > 0L)
  best <- eligible[which.max(log$log_likelihood[eligible])]
  log$selected <- seq_len(nrow(log)) == best
  list(fit = fits[[best]], solver = methods[best], messages = "Higher likelihood among converged solvers",
       optimization = log)
}

fixed_path_statistics <- function(bars, rows, primary) {
  spec <- position_spec(bars, primary)
  make <- function(variant) next_open_positions(close_targets(spec, primary, variant))
  r <- return_components(bars$Open, bars$Close)
  main <- portfolio_log(r, make("full"), rows, trial_fee)
  core <- portfolio_log(r, make("core"), rows, trial_fee)
  no_shape <- portfolio_log(r, make("no_shape"), rows, trial_fee)
  base <- portfolio_log(r, rep(TRUE, nrow(bars)), rows, trial_fee)
  matched <- sum(log1p(mean(main$weight) * expm1(base$daily + base$cost_log)))
  net <- sum(main$daily)
  benchmark <- sum(base$daily)
  c(net_log = net, index_log = benchmark, active_log = net - benchmark,
    matched_log = net - matched, overlay_log = net - sum(core$daily),
    shape_log = net - sum(no_shape$daily), trades = sum(main$buys),
    exposure = mean(main$weight), both = as.numeric(pass_objectives(net, benchmark)))
}

fixed_market_null <- function(result, n_paths = simulations_per_process) {
  s <- result$setup
  r <- s$rows
  fields <- c("Date", ohlc_fields)
  warm <- s$bars[seq_len(min(r) - 1L), fields]
  actual <- s$bars[r, fields]
  dates <- actual$Date
  horizon <- length(r)
  observed <- fixed_path_statistics(s$bars[fields], r, s$primary)
  reference <- candle_reference(actual)
  returns <- reference$returns
  ar <- ar_parameters(returns)
  labels <- c("Random walk", "AR(1)", "GARCH-M", "EGARCH")
  out <- lapply(seq_along(labels), function(k) {
    model <- labels[k]
    set.seed(42126L + k)
    opt <- NULL
    if (k == 1L) {
      coeff <- data.frame(model, term = "drift_pct", estimate = 100 * mean(returns), se = NA_real_)
      diag <- calibration_diagnostics(model, returns - mean(returns), notes = "Empirical daily tuples")
    } else if (k == 2L) {
      coeff <- ar$coefficients
      diag <- calibration_diagnostics(model, ar$innovation, solver = "ML", ar = ar$phi,
                                      notes = "Centered empirical innovations")
    } else {
      v <- if (model == "EGARCH") fixed_egarch_fit(returns) else fit_volatility(returns, model)
      opt <- v$optimization
      est <- v$fit@fit$matcoef
      coeff <- data.frame(model, term = rownames(est), estimate = est[, 1L], se = est[, 2L], row.names = NULL)
      diag <- calibration_diagnostics(model, v$fit@fit$z, solver = v$solver,
                                      persistence = rugarch::persistence(v$fit), notes = v$messages)
      seeds <- sample.int(100000000L, n_paths)
      sim <- rugarch::ugarchsim(v$fit, n.sim = horizon, n.start = 500L,
                                m.sim = n_paths, rseed = seeds, startMethod = "unconditional")
      paths <- sim@simulation$seriesSim / 100
      stopifnot(ncol(paths) == n_paths, nrow(paths) == horizon)
      set.seed(52260L + k)
    }
    raw <- vapply(seq_len(n_paths), function(i) {
      if (k == 1L) {
        donor <- sample.int(length(returns), horizon, replace = TRUE)
        path <- returns[donor]
      } else {
        path <- if (k == 2L) {
          e <- sample(ar$innovation, horizon + 250L, replace = TRUE)
          ar$mean + tail(as.numeric(stats::filter(e, ar$phi, method = "recursive")), horizon)
        } else paths[, i]
        donor <- match_shapes(path, reference)
      }
      m <- continuation_candles(warm, dates, path, donor, reference)
      fixed_path_statistics(m, r, s$primary)
    }, observed)
    stopifnot(all(is.finite(raw)))
    list(draws = cbind(model, path = seq_len(n_paths), as.data.frame(t(raw))),
         diagnostics = diag, coefficients = coeff, optimization = opt)
  })
  bind <- function(name) do.call(rbind, lapply(out, "[[", name))
  draws <- bind("draws")
  stopifnot(all(table(draws$model) == n_paths))
  targets <- c("net_log", "active_log", "matched_log", "overlay_log", "shape_log")
  results <- do.call(rbind, lapply(labels, function(label) {
    d <- draws[draws$model == label, ]
    do.call(rbind, lapply(targets, function(metric) {
      real <- observed[[metric]]
      z <- d[[metric]]
      count <- sum(z >= real)
      band <- stats::binom.test(count, n_paths)$conf.int
      limits <- quantile(z, c(.025, .5, .975), names = FALSE)
      data.frame(model = label, metric, observed = real, paths = n_paths, exceedances = count,
                 p = (count + 1L) / (n_paths + 1L), mc_low = band[1L], mc_high = band[2L],
                 q025 = limits[1L], median = limits[2L], q975 = limits[3L],
                 positive_and_ahead = mean(d$both))
    }))
  }))
  fit_span <- data.frame(first = min(dates), last = max(dates), sessions = horizon,
                         calibrated_close_return_observations = length(returns), paths_per_model = n_paths)
  list(draws = draws, summary = results, coefficients = bind("coefficients"),
       diagnostics = bind("diagnostics"), optimization = bind("optimization"),
       observed = observed, span = fit_span)
}
