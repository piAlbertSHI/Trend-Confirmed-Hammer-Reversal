fit_volatility <- function(log_r, model) {
  spec <- volatility_spec(model)
  scaled <- 100 * log_r
  messages <- character()
  for (solver in c("solnp", "hybrid")) {
    fit <- tryCatch(withCallingHandlers(
      rugarch::ugarchfit(spec, scaled, solver = solver),
      warning = function(w) {
        messages <<- unique(c(messages, conditionMessage(w)))
        invokeRestart("muffleWarning")
      }), error = identity)
    if (!inherits(fit, "error") && isTRUE(fit@fit$convergence == 0L) &&
        all(is.finite(fit@fit$coef))) {
      return(list(fit = fit, solver = solver, messages = paste(messages, collapse = " | ")))
    }
    messages <- c(messages, if (inherits(fit, "error")) conditionMessage(fit) else
      paste(solver, "convergence code", fit@fit$convergence))
  }
  stop(paste(model, paste(messages, collapse = "; ")))
}

calibration_diagnostics <- function(model, innovation, ar = NA_real_, solver = NA_character_,
                                    persistence = NA_real_, notes = "") {
  z <- innovation[is.finite(innovation)]
  lb <- function(x) unname(stats::Box.test(x, lag = 20L, type = "Ljung-Box")$p.value)
  data.frame(model, residuals = length(z), ar, solver, persistence,
             ljung_box_return_p = lb(z), ljung_box_sq_p = lb(z^2), notes)
}

ar_parameters <- function(r) {
  fit <- stats::arima(100 * r, order = c(1L, 0L, 0L), include.mean = TRUE)
  stopifnot(fit$code == 0L)
  params <- coef(fit)
  residual <- as.numeric(residuals(fit))[-1L]
  data <- data.frame(model = "AR(1)", term = names(params), estimate = as.numeric(params),
                     se = sqrt(diag(vcov(fit))), row.names = NULL)
  list(phi = unname(params[["ar1"]]), mean = unname(params[["intercept"]]) / 100,
       innovation = (residual - mean(residual)) / 100, coefficients = data)
}

candle_reference <- function(bars) {
  n <- nrow(bars)
  prev <- bars$Close[-n]
  a <- bars[-1L, ]
  r <- log(a$Close / prev)
  boundaries <- quantile(r, seq(.1, .9, .1), names = FALSE)
  bins <- split(seq_along(r), findInterval(r, boundaries) + 1L)
  list(returns = r, boundaries = boundaries, bins = bins,
       gap = log(a$Open / prev),
       upper = log(a$High / pmax(a$Open, a$Close)),
       lower = log(pmin(a$Open, a$Close) / a$Low))
}

match_shapes <- function(path, reference) {
  bin_number <- findInterval(path, reference$boundaries) + 1L
  uniform <- runif(length(path))
  idx <- integer(length(path))
  for (j in unique(bin_number)) {
    slots <- which(bin_number == j)
    candidates <- reference$bins[[as.character(j)]]
    idx[slots] <- candidates[1L + floor(uniform[slots] * length(candidates))]
  }
  idx
}

coherent_scores <- function(path, donor, reference, first, date) {
  if (any(!is.finite(path))) stop("Nonfinite simulated returns")
  close <- first$Close * exp(c(0, cumsum(path)))
  open <- c(first$Open, head(close, -1L) * exp(reference$gap[donor]))
  high <- c(first$High, pmax(open[-1L], close[-1L]) * exp(reference$upper[donor]))
  low <- c(first$Low, pmin(open[-1L], close[-1L]) * exp(-reference$lower[donor]))
  bars <- data.frame(Date = date, Open = open, High = high, Low = low, Close = close)
  validate_bars(bars)
  s <- signal_index(bars, features(bars)) + 1L
  s <- s[s <= nrow(bars)]
  log_r <- log(close / open)
  eligible <- seq.int(21L, length(close))
  simulated <- sum(log_r[s])
  exposure <- length(s) / length(eligible)
  c(log_return = simulated, trades = length(s), matched_log = simulated - exposure * sum(log_r[eligible]),
    mean_trade_bp = if (length(s)) mean(log_r[s]) * 10000 else NA_real_)
}

revised_nulls <- function(bars, reps = null_reps) {
  reference <- candle_reference(bars)
  r <- reference$returns
  n <- length(r)
  ar <- ar_parameters(r)
  models <- c("Random walk", "AR(1)", "GARCH-M", "EGARCH")
  first <- bars[1L, ]
  rows <- lapply(seq_along(models), function(k) {
    model <- models[k]
    set.seed(25080L + k)
    coeff <- NULL
    if (model == "Random walk") {
      diagnostic <- calibration_diagnostics(model, r - mean(r), notes = "Empirical drift and iid tuples")
      coeff <- data.frame(model, term = "daily_drift_pct", estimate = 100 * mean(r), se = NA_real_)
    } else if (model == "AR(1)") {
      diagnostic <- calibration_diagnostics(model, ar$innovation, ar = ar$phi, solver = "ML",
                                            notes = "Centered empirical innovations")
      coeff <- ar$coefficients
    } else {
      fit <- fit_volatility(r, model)
      m <- fit$fit@fit$matcoef
      coeff <- data.frame(model, term = rownames(m), estimate = as.numeric(m[, 1L]),
                          se = as.numeric(m[, 2L]), row.names = NULL)
      diagnostic <- calibration_diagnostics(model, fit$fit@fit$z, solver = fit$solver,
                                            persistence = rugarch::persistence(fit$fit),
                                            notes = fit$messages)
      seeds <- sample.int(100000000L, reps)
      sim <- rugarch::ugarchsim(fit$fit, n.sim = n, n.start = 500L, m.sim = reps,
                                startMethod = "unconditional", rseed = seeds)
      matrix_paths <- sim@simulation$seriesSim / 100
      stopifnot(identical(dim(matrix_paths), c(n, as.integer(reps))))
      set.seed(39010L + k)
    }
    scores <- vapply(seq_len(reps), function(i) {
      if (model == "Random walk") {
        donor <- sample.int(n, n, replace = TRUE)
        path <- r[donor]
      } else {
        if (model == "AR(1)") {
          innovation <- sample(ar$innovation, n + 250L, replace = TRUE)
          path <- ar$mean + tail(as.numeric(stats::filter(innovation, ar$phi, method = "recursive")), n)
        } else {
          path <- matrix_paths[, i]
        }
        donor <- match_shapes(path, reference)
      }
      coherent_scores(path, donor, reference, first, bars$Date)
    }, c(log_return = 0, trades = 0, matched_log = 0, mean_trade_bp = 0))
    d <- as.data.frame(t(scores))
    stopifnot(all(is.finite(d$log_return)), all(is.finite(d$matched_log)), all(d$trades >= 0))
    list(draws = cbind(model, replication = seq_len(reps), d), coefficients = coeff,
         diagnostics = diagnostic)
  })
  names(rows) <- models
  bind <- function(field) do.call(rbind, unname(lapply(rows, "[[", field)))
  list(draws = bind("draws"), coefficients = bind("coefficients"), diagnostics = bind("diagnostics"))
}

null_tail_summary <- function(draws, bars, primary) {
  raw <- sum(primary$gross_daily)
  eligible <- seq.int(21L, nrow(bars))
  exposure <- nrow(primary$trades) / length(eligible)
  matched <- raw - exposure * sum(log(bars$Close[eligible] / bars$Open[eligible]))
  observed <- c(log_return = raw, matched_log = matched)
  do.call(rbind, lapply(unique(draws$model), function(model) {
    d <- draws[draws$model == model, ]
    do.call(rbind, lapply(names(observed), function(statistic) {
      x <- d[[statistic]]
      count <- sum(x >= observed[[statistic]])
      ci <- stats::binom.test(count, length(x))$conf.int
      q <- quantile(x, c(.025, .5, .975), names = FALSE)
      data.frame(model, statistic, observed_log = observed[[statistic]],
                 simulations = length(x), exceedances = count,
                 p_one_sided = (1 + count) / (length(x) + 1L),
                 mc_low = ci[1L], mc_high = ci[2L],
                 lower_log = q[1L], median_log = q[2L], upper_log = q[3L],
                 median_trades = median(d$trades))
    }))
  }))
}
