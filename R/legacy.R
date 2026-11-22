legacy_candles <- function(returns, initial_price) {
  close <- initial_price * exp(cumsum(returns))
  n <- length(close)
  open <- c(initial_price, close[-n])
  high <- close * (1 + abs(rnorm(n, 0, 0.01)))
  low <- close * (1 - abs(rnorm(n, 0, 0.01)))
  if (any(!is.finite(close)) || any(close <= 0) || any(!is.finite(high))) {
    return(c(points = NA_real_, invalid_bar_share = NA_real_))
  }
  mid <- TTR::SMA(close, 20L)
  body <- abs(close - open)
  buy <- which(close < mid & pmin(open, close) - low > 2 * body &
                 high - pmax(open, close) < body) + 1L
  buy <- buy[buy <= n]
  c(points = sum(close[buy] - open[buy]),
    invalid_bar_share = mean(high < pmax(open, close) | low > pmin(open, close)))
}

volatility_spec <- function(model) {
  if (model == "GARCH-M") {
    variance <- list(model = "sGARCH", garchOrder = c(1, 1))
    average <- list(armaOrder = c(0, 0), include.mean = TRUE, archm = TRUE, archpow = 1)
  } else {
    stopifnot(model == "EGARCH")
    variance <- list(model = "eGARCH", garchOrder = c(1, 1))
    average <- list(armaOrder = c(0, 0), include.mean = TRUE)
  }
  rugarch::ugarchspec(variance.model = variance, mean.model = average,
                     distribution.model = "std")
}

legacy_draws <- function(archive, n_walk = 500L, n_ar = 500L, n_garch = 500L, n_egarch = 50L) {
  original <- legacy_fields(archive)
  r <- diff(log(original$Close))
  initial <- original$Close[1L]
  fit_ar <- stats::arima(r[-1L], order = c(1L, 0L, 0L))
  ar_params <- data.frame(term = names(coef(fit_ar)), estimate = as.numeric(coef(fit_ar)),
                          se = sqrt(diag(vcov(fit_ar))), row.names = NULL)
  ar_params$t <- ar_params$estimate / ar_params$se
  phi <- unname(coef(fit_ar)[["ar1"]])
  sigma <- sqrt(fit_ar$sigma2)

  counts <- c("Random walk" = n_walk, "AR(1)" = n_ar, "GARCH-M" = n_garch, EGARCH = n_egarch)
  output <- lapply(names(counts), function(model) {
    count <- counts[[model]]
    set.seed(123)
    points <- invalid <- rep(NA_real_, count)
    status <- rep("ok", count)
    spec <- if (model %in% c("GARCH-M", "EGARCH")) volatility_spec(model) else NULL
    for (i in seq_len(count)) {
      if (model == "Random walk") {
        path <- sample(r, length(r), replace = TRUE)
      } else if (model == "AR(1)") {
        # The archive searched for a coefficient named const and fell back to zero.
        path <- as.numeric(filter(rnorm(length(r) - 1L, sd = sigma), phi, method = "recursive"))
      } else {
        draw <- sample(r, length(r), replace = TRUE)
        fit <- tryCatch(suppressWarnings(rugarch::ugarchfit(spec, draw, solver = "solnp")),
                        error = identity)
        if (inherits(fit, "error") || !identical(as.integer(fit@fit$convergence), 0L)) {
          status[i] <- if (inherits(fit, "error")) "fit_error_zero" else "no_convergence_zero"
          points[i] <- 0
          next
        }
        sim <- tryCatch(suppressWarnings(rugarch::ugarchsim(fit, n.sim = length(r))), error = identity)
        if (inherits(sim, "error")) {
          status[i] <- "simulation_error"
          next
        }
        path <- as.numeric(sim@simulation$seriesSim)
      }
      score <- legacy_candles(path, initial)
      points[i] <- score[["points"]]
      invalid[i] <- score[["invalid_bar_share"]]
      if (!is.finite(points[i])) status[i] <- "non_finite_path"
    }
    data.frame(model, replication = seq_len(count), points, invalid_bar_share = invalid, status)
  })
  list(draws = do.call(rbind, output), ar = ar_params)
}

legacy_summary <- function(draws, observed) {
  models <- unique(draws$model)
  reported <- c(0.390, 0.432, 0.202, 0.460)
  do.call(rbind, lapply(seq_along(models), function(i) {
    d <- draws[draws$model == models[i], ]
    pts <- d$points[is.finite(d$points)]
    success <- d$points[d$status == "ok"]
    data.frame(model = models[i], requested = nrow(d), finite = length(pts),
               failed_fit_as_zero = sum(endsWith(d$status, "_zero")),
               other_errors = sum(!is.finite(d$points)), median_points = median(pts),
               exceedances = sum(pts >= observed), reproduced_tail = mean(pts >= observed),
               finite_fits_tail = mean(success >= observed), reported_tail = reported[i],
               mean_invalid_bar_share = mean(d$invalid_bar_share, na.rm = TRUE))
  }))
}
