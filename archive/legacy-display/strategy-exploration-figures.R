strategy_ink <- c("Buy and hold" = "#687884", "Frozen trend + hammer" = blue,
                  "Bollinger reentry candidate" = teal, "Original intraday" = "#9F662D")
strategy_dash <- c("Buy and hold" = "42", "Frozen trend + hammer" = "solid",
                   "Bollinger reentry candidate" = "solid", "Original intraday" = "13")

market_decomposition_plot <- function(x) {
  d <- x$market$archive
  ret <- return_components(d$Open, d$Close)
  val <- data.frame(date = d$Date, "Overnight" = cumsum(ret$gap), "Open to close" = cumsum(ret$intraday),
                    "Buy and hold" = cumsum(ret$intraday + ret$gap), check.names = FALSE)
  series <- setdiff(names(val), "date")
  color <- setNames(c(teal, rust, "#687884"), series)
  ggplot(curves_long(val, series), aes(date, value, color = rule, linetype = rule)) +
    geom_hline(yintercept = 0, color = "#A5B4BC", linewidth = .45) +
    geom_line(linewidth = .76) +
    scale_color_manual(values = color, breaks = series) +
    scale_linetype_manual(values = setNames(c("solid", "solid", "42"), series), breaks = series) +
    scale_y_continuous(labels = percent_axis(1)) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    labs(title = "Overnight gains offset intraday losses",
         subtitle = "The original intraday rule was flat between the close and the next open.",
         y = "Additive log return", x = NULL) +
    chart_theme()
}

curves_long <- function(d, cols) {
  do.call(rbind, lapply(cols, function(name) data.frame(date = d$date, rule = name, value = d[[name]])))
}

performance_curves <- function(d, extra = NULL) {
  val <- data.frame(date = d$date, "Buy and hold" = expm1(cumsum(d[["Buy and hold log"]])),
                    "Frozen trend + hammer" = expm1(cumsum(d[["Selected log"]])),
                    "Original intraday" = expm1(cumsum(d[["Original intraday log"]])), check.names = FALSE)
  if (!is.null(extra)) {
    stopifnot(identical(extra$date, d$date))
    val[["Bollinger reentry candidate"]] <- expm1(cumsum(extra[["P06 log"]]))
  }
  curves_long(val, setdiff(names(val), "date"))
}

equity_full_plot <- function(x) {
  d <- comparison_daily(x$market$full, x$oos_positions[c("Selected", "Original intraday", "Buy and hold")],
                        x$spans[x$spans$period == "Complete", ])
  p <- performance_curves(d)
  cutoff <- as.Date("2020-01-01")
  ggplot(p, aes(date, value, color = rule, linetype = rule)) +
    annotate("rect", xmin = cutoff, xmax = max(d$date), ymin = -Inf, ymax = Inf, fill = "#E9F2F3") +
    geom_hline(yintercept = 0, color = "#A5B4BC", linewidth = .47) +
    geom_line(linewidth = .73) +
    scale_color_manual(values = strategy_ink, breaks = names(strategy_ink)) +
    scale_linetype_manual(values = strategy_dash, breaks = names(strategy_dash)) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    scale_y_continuous(labels = percent_axis()) +
    labs(title = "Longer exposure improved the full history",
         subtitle = "Pale shading marks 2020 onward. Results include 5 bp per side.",
         y = "Cumulative net return", x = NULL) + chart_theme()
}

equity_oos_plot <- function(x, followup) {
  p <- rbind(cbind(instrument = "Hang Seng price index", performance_curves(x$data_oos, followup$oos_daily)),
             cbind(instrument = "Tracker Fund of Hong Kong", performance_curves(x$fund_oos, followup$fund_daily)))
  ggplot(p, aes(date, value, color = rule, linetype = rule)) +
    geom_hline(yintercept = 0, color = "#A5B4BC", linewidth = .5) +
    geom_line(linewidth = .76) +
    facet_wrap(~instrument, ncol = 2L) +
    scale_color_manual(values = strategy_ink, breaks = names(strategy_ink)) +
    scale_linetype_manual(values = strategy_dash, breaks = names(strategy_dash)) +
    scale_x_date(breaks = as.Date(paste0(seq(2020, 2026, by = 2), "-01-01")), date_labels = "%Y") +
    scale_y_continuous(labels = percent_axis()) +
    labs(title = "The frozen strategy lagged both benchmarks after 2019",
         subtitle = "All strategies restart at 0%. Bollinger reentry was identified in follow-up exploration.",
         y = "Cumulative net return", x = NULL) + chart_theme()
}

illustrate_cost <- function(x, pullback) {
  a <- subset(x$scores, rule == "Selected" & period %in% c("Complete", "Out of sample") & instrument == "^HSI")
  both <- subset(pullback$scores, rule == "P06" & period %in% c("Complete", "Out of sample") & instrument == "^HSI")
  cols <- c("period", "fee_bps", "net")
  rows <- rbind(cbind(type = "Frozen trend + hammer", a[cols]),
                cbind(type = "Bollinger reentry candidate", both[cols]),
                data.frame(type = "Buy and hold", period = a$period, fee_bps = a$fee_bps, net = a$benchmark_net))
  rows$period <- factor(rows$period, c("Complete", "Out of sample"), c("Full history", "2020-26 evaluation"))
  ggplot(rows, aes(fee_bps, net, color = type, linetype = type)) +
    geom_hline(yintercept = 0, color = "#A5B4BC", linewidth = .5) +
    geom_vline(xintercept = trial_fee, linewidth = .45, color = "#91A7AF", linetype = "dotted") +
    geom_line(linewidth = .89) + geom_point(size = 1.9) +
    facet_wrap(~period, scales = "free_y") +
    scale_color_manual(values = strategy_ink, breaks = names(strategy_ink)) +
    scale_linetype_manual(values = strategy_dash, breaks = names(strategy_dash)) +
    scale_y_continuous(labels = percent_axis()) +
    scale_x_continuous(breaks = c(0, 5, 10, 20, 30)) +
    labs(title = "Fees widen the evaluation-period gap",
         subtitle = "The frozen rule lagged in the evaluation window even at zero modeled costs.",
         x = "One-way cost (basis points)", y = "Cumulative net return") + chart_theme()
}

effect_intervals_plot <- function(x) {
  labels <- c("Selected vs index or fund" = "Frozen rule vs buy and hold",
              "Selected vs equal exposure" = "Frozen rule vs matched exposure",
              "Hammer overlay vs trend" = "Confirmed overlay vs same trend",
              "Hammer shape vs no shape" = "Shape filter vs no shape filter")
  d <- subset(x$statistics, block == 20L & period == "Out of sample" & metric %in% names(labels))
  d$label <- factor(unname(labels[d$metric]), levels = rev(unname(labels)))
  d$instrument <- factor(d$instrument, c("^HSI", "2800.HK"), c("Hang Seng Index", "Tracker Fund"))
  ggplot(d, aes(observed_annual_log, label)) +
    geom_vline(xintercept = 0, color = "#91A7AF", linewidth = .48, linetype = "dashed") +
    geom_segment(aes(x = low_annual_log, xend = high_annual_log, yend = label),
                 linewidth = 1, color = teal) +
    geom_point(color = teal, size = 2.4) +
    facet_wrap(~instrument, ncol = 2L) +
    scale_x_continuous(labels = percent_axis(.1)) +
    labs(title = "Intervals span zero for timing and hammer effects",
         subtitle = "95% circular block intervals, 20-session blocks, 2020-26.",
         x = "Annualized active log return", y = NULL) + chart_theme() +
    theme(panel.grid.major.y = element_blank())
}

earlier_comparison_plot <- function(followup) {
  d <- followup$early_daily
  val <- data.frame(date = d$date, "Buy and hold" = expm1(cumsum(d[["Buy and hold log"]])),
                    "Bollinger reentry candidate" = expm1(cumsum(d[["P06 log"]])), check.names = FALSE)
  p <- curves_long(val, setdiff(names(val), "date"))
  ggplot(p, aes(date, value, color = rule, linetype = rule)) +
    geom_hline(yintercept = 0, color = "#A5B4BC", linewidth = .5) +
    geom_line(linewidth = .79) +
    scale_color_manual(values = strategy_ink, breaks = names(strategy_ink)) +
    scale_linetype_manual(values = strategy_dash, breaks = names(strategy_dash)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_y_continuous(labels = percent_axis()) +
    labs(title = "The later winner faces an earlier Hong Kong market",
         subtitle = "A separate pre-2000 price history. Rule and costs match the follow-up experiment.",
         y = "Cumulative net return", x = NULL) + chart_theme()
}

simulation_plot <- function(null) {
  names <- c("Random walk", "AR(1)", "GARCH-M", "EGARCH")
  d <- subset(null$draws, period == "Out of sample")
  stats <- subset(null$summary, period == "Out of sample" & metric == "Selected vs index")
  stats <- stats[match(names, stats$model), ]
  panel <- sprintf("%s   |   p = %.3f", stats$model, stats$p_one_sided)
  d$panel <- factor(panel[match(d$model, names)], levels = panel)
  stats$panel <- factor(panel, levels = panel)
  ggplot(d, aes(active_log)) +
    geom_histogram(bins = 29L, fill = "#BDD6E1", color = "white", linewidth = .32) +
    geom_vline(xintercept = 0, color = "#6D838F", linewidth = .45, linetype = "dashed") +
    geom_vline(data = stats, aes(xintercept = observed_log), color = rust, linewidth = .85) +
    facet_wrap(~panel, ncol = 2L) +
    scale_x_continuous(labels = percent_axis()) +
    labs(title = "Observed relative performance sits inside the null distributions",
         subtitle = "Each panel has 500 pre-2020 calibrated simulations. Rust line marks the observed spread.",
         x = "Cumulative active log return vs buy and hold", y = "Simulated paths") +
    chart_theme()
}

render_strategy_figures <- function(x, null, pullback, followup) {
  dir.create("strategy/figures", showWarnings = FALSE)
  charts <- list(market_decomposition_plot(x), equity_full_plot(x), equity_oos_plot(x, followup),
                 illustrate_cost(x, pullback), effect_intervals_plot(x), earlier_comparison_plot(followup),
                 simulation_plot(null))
  stems <- c("01-return-source", "02-full-wealth", "03-evaluation", "04-fees",
             "05-effect-intervals", "06-earlier-history", "07-randomness")
  height <- c(4.8, 4.85, 4.86, 4.7, 4.05, 4.65, 5.4)
  for (i in seq_along(stems)) {
    path <- file.path("strategy", "figures", stems[i])
    ggsave(paste0(path, ".png"), charts[[i]], device = ragg::agg_png,
           width = 10.6, height = height[i], dpi = 155, bg = "white")
    ggsave(paste0(path, ".svg"), charts[[i]], device = svglite::svglite,
           width = 10.6, height = height[i], bg = "white")
  }
  invisible(NULL)
}
