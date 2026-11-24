input_signature <- function() {
  scripts <- file.path("R", c("data.R", "strategy.R", "research.R", "legacy.R",
                             "null_models.R", "analysis.R"))
  paths <- c(scripts, unname(market_files))
  structure(vapply(paths, digest::digest, "", algo = "sha256", file = TRUE), names = paths)
}

load_or_replay <- function(archive) {
  path <- "archive/replay.rds"
  key <- digest::digest(file = "R/legacy.R", algo = "sha256")
  if (file.exists(path)) {
    saved <- readRDS(path)
    if (identical(saved$input_sha, market_hashes[["archive"]]) &&
        identical(saved$implementation_sha, key)) return(saved)
  }
  replay <- legacy_draws(archive)
  replay$input_sha <- market_hashes[["archive"]]
  replay$implementation_sha <- key
  replay$environment <- capture.output(sessionInfo())
  saveRDS(replay, path, version = 3L)
  replay
}

reconcile <- function(archive, old, f_old, f_correct, parameters) {
  idx <- seq_len(nrow(archive))
  s <- signal_index(archive, f_correct)
  old_run <- trade_log(old, signal_index(old, f_old))
  fixed <- trade_log(archive, s)
  a <- trade_metrics(old_run, idx)
  b <- trade_metrics(fixed, idx)
  axes <- seq(0.1, 2.2, by = 0.1)
  old_grid <- archived_grid(old, expand.grid(a = axes, b = axes), f_old)
  corrected_same_day <- archived_grid(archive, parameters, f_correct)
  j <- which(abs(old_grid$a - .1) < 1e-9 & abs(old_grid$b - 1.3) < 1e-9)
  old_tuned <- old_grid[j, ]
  safe_tuned <- trade_log(archive, signal_index(archive, f_correct, .1, 1.3))
  c <- trade_metrics(safe_tuned, idx)
  audit <- data.frame(
    specification = c("Archived fixed rule", "Named columns, next open",
                      "Archived tuned rule", "Named columns, signal-day open",
                      "Named columns, next open at published tuning"),
    a = c(2, 2, .1, .1, .1), b = c(1, 1, 1.3, 1.3, 1.3),
    columns = c("swapped", "named", "swapped", "named", "named"),
    signal_to_entry = c(1L, 1L, 0L, 0L, 1L),
    trades = c(a$trades, b$trades, old_tuned$trades, corrected_same_day$trades[j], c$trades),
    sum_points = c(a$rounded_points, b$rounded_points, old_tuned$points,
                   corrected_same_day$points[j], c$points),
    win = c(a$gross_win, b$gross_win, old_tuned$win, corrected_same_day$win[j], c$gross_win),
    points_per_trade = c(a$rounded_points / a$trades, b$rounded_points / b$trades,
                         old_tuned$points_per_trade, corrected_same_day$points_per_trade[j], c$points / c$trades),
    compound = c(a$gross_compound, b$gross_compound, NA_real_, NA_real_, c$gross_compound),
    report_points = c(718.48, NA_real_, 43411.08, NA_real_, NA_real_),
    report_win = c(NA_real_, NA_real_, .5688, NA_real_, NA_real_),
    report_points_per_trade = c(NA_real_, NA_real_, 25.89, NA_real_, NA_real_)
  )
  stopifnot(a$trades == 127L, b$trades == 125L,
            abs(audit$sum_points[1L] - 718.48) < 1e-7,
            abs(audit$sum_points[2L] + 648.57) < 1e-7)
  list(audit = audit, original = old_run, corrected = fixed,
       landscape = rbind(cbind(scenario = "Swapped OHLC, same-day", old_grid),
                         cbind(scenario = "Named OHLC, same-day", corrected_same_day)))
}

simulate_project <- function() {
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  market <- load_market()
  archive <- market$archive
  full <- market$full
  original_bars <- legacy_fields(archive)
  f <- features(full)
  f_archive <- features(archive)
  legacy_features <- features(original_bars)
  previous <- reconcile(archive, original_bars, legacy_features, f_archive, parameters)
  replay <- load_or_replay(archive)
  replay_summary <- legacy_summary(replay$draws, previous$audit$sum_points[1L])
  stopifnot(nrow(replay$draws) == 1550L, all(replay_summary$finite > 0))

  periods <- periods_for(full, max(archive$Date))
  intraday <- log(full$Close / full$Open)
  entries <- grid_entries(full, f)
  results <- lapply(seq_len(nrow(periods)), function(j) {
    idx <- period_rows(full, periods[j, ])
    cbind(period = periods$period[j], parameters, grid_performance(entries, intraday, idx))
  })
  grid <- do.call(rbind, results)
  development <- period_rows(full, periods[2L, ])
  picked <- select_rule(results[[2L]])
  rules <- rbind(data.frame(rule = c("Original rule", "Published tuning"), a = c(2, .1), b = c(1, 1.3)),
                 cbind(rule = "Development selection", parameters[picked, ]))
  rownames(rules) <- NULL
  metrics <- period_summary(full, f, periods, rules, costs = c(0, 1, 2, 5, 10, 20))
  base_metrics <- metrics[metrics$fee_bps == 0, ]
  breakeven <- base_metrics[c("period", "rule", "trades")]
  breakeven$breakeven_one_way_bp <- 10000 * tanh(log1p(base_metrics$gross_compound) / (2 * base_metrics$trades))

  walk <- walk_forward(archive, f_archive, entries[seq_len(nrow(archive)), , drop = FALSE])
  primary <- trade_log(full, signal_index(full, f), fee_bps = research_fee)
  cumulative <- data.frame(date = full$Date, open = full$Open, close = full$Close,
                           mid = f$mid, lower = f$lower,
                           signal = seq_len(nrow(full)) %in% signal_index(full, f))
  transactions <- lapply(seq_len(nrow(rules)), function(i) {
    s <- signal_index(full, f, rules$a[i], rules$b[i])
    run <- trade_log(full, s, fee_bps = research_fee)
    stopifnot(abs(sum(run$trades$net_log) - sum(run$net_daily)) < 1e-10)
    cumulative[[paste0("rule_", i, "_gross_log")]] <<- run$gross_daily
    cumulative[[paste0("rule_", i, "_net_log")]] <<- run$net_daily
    cbind(rule = rules$rule[i], run$trades)
  })
  trades <- do.call(rbind, transactions)

  holdouts <- lapply(c(1L, 4L, 5L), function(j) {
    rows <- intersect(period_rows(full, periods[j, ]), seq.int(21L, nrow(full)))
    hit <- rows %in% primary$trades$entry
    by_block <- lapply(block_lengths, function(block) {
      cbind(period = periods$period[j], edge_stats(intraday[rows], hit, block, bootstrap_reps,
                                                  6210L + 10L * j + block))
    })
    do.call(rbind, by_block)
  })
  max_adjustment <- do.call(rbind, lapply(block_lengths, function(block) {
    grid_max_test(entries, intraday, development[development >= 21L], block,
                  bootstrap_reps, 7200L + block)
  }))

  null <- revised_nulls(archive)
  tail <- null_tail_summary(null$draws, archive, previous$corrected)
  timing <- rbind(timing_comparison(original_bars, legacy_features, "Swapped columns"),
                  timing_comparison(archive, f_archive, "Named columns"))

  list(
    signature = input_signature(), analysis_date = max(full$Date),
    market = market, periods = periods, rules = rules, reconciliation = previous$audit,
    legacy = list(draws = replay$draws, summary = replay_summary, ar = replay$ar,
                  environment = replay$environment, sensitivity = previous$landscape),
    metrics = metrics, grid = grid, break_even = breakeven, timing = timing,
    daily = cumulative, trades = trades, original_trades = previous$original$trades,
    baselines = benchmarks(full, primary, periods), walk = walk,
    edge = do.call(rbind, holdouts), max_adjustment = max_adjustment,
    latency = latency_profile(archive, f_archive),
    regimes = volatility_regimes(full, primary, development),
    null = c(null, list(tail = tail)), environment = capture.output(sessionInfo())
  )
}

write_results <- function(result) {
  dir.create("results", showWarnings = FALSE)
  tables <- list(
    data_coverage = result$market$coverage, source_overlap = result$market$overlap,
    sample_periods = result$periods, rule_definitions = result$rules,
    reconciliation = result$reconciliation,
    legacy_draws = result$legacy$draws, legacy_summary = result$legacy$summary,
    legacy_ar_coefficients = result$legacy$ar, legacy_grid = result$legacy$sensitivity,
    legacy_trades = result$original_trades, daily_returns = result$daily, trades = result$trades,
    performance = result$metrics, grid = result$grid, break_even = result$break_even,
    execution_variants = result$timing, baselines = result$baselines,
    walk_forward_folds = result$walk$folds, walk_forward_daily = result$walk$daily,
    conditional_edge = result$edge, grid_max_test = result$max_adjustment,
    latency = result$latency, volatility_regimes = result$regimes,
    null_draws = result$null$draws, null_tail_tests = result$null$tail,
    null_coefficients = result$null$coefficients, null_diagnostics = result$null$diagnostics
  )
  for (table in names(tables)) {
    write.csv(tables[[table]], file.path("results", paste0(table, ".csv")),
              row.names = FALSE, na = "", fileEncoding = "UTF-8")
  }
  writeLines(result$environment, "results/session-info.txt", useBytes = TRUE)
  saveRDS(result, "results/analysis.rds", version = 3L)
  invisible(result)
}

analysis_summary <- function(x) {
  fixed <- subset(x$metrics, rule == "Original rule" & fee_bps == research_fee)
  list(archive_sessions = nrow(x$market$archive),
       extension_sessions = nrow(x$market$full) - nrow(x$market$archive),
       reconstruction = x$reconciliation[1:2, c("specification", "trades", "sum_points")],
       fixed = fixed[c("period", "trades", "gross_compound", "net_compound")],
       grid_p = x$max_adjustment$adjusted_p,
       legacy_simulations = nrow(x$legacy$draws), revised_simulations = nrow(x$null$draws),
       walk_forward_folds = nrow(x$walk$folds))
}
