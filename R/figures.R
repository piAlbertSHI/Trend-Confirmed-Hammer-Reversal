suppressPackageStartupMessages(library(ggplot2))

ink <- "#122837"
muted <- "#4C6371"
blue <- "#275E8C"
teal <- "#0B716B"
rust <- "#B2472C"
grid_line <- "#E1E8EB"
strategy_colors <- c("Original rule" = blue, "Published tuning" = rust, "Development selection" = teal)
rule_short <- c("Original rule" = "Original rule", "Published tuning" = "Reported tuning",
                "Development selection" = "Development selection")

percent_axis <- function(step = 1) scales::label_percent(accuracy = step)

chart_theme <- function() {
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(12, 17, 12, 14),
      plot.title = element_text(color = ink, face = "bold", size = 17, margin = margin(b = 4)),
      plot.subtitle = element_text(color = muted, size = 10.7, margin = margin(b = 14), lineheight = 1.14),
      axis.text = element_text(color = muted, size = 10),
      axis.title = element_text(color = ink, size = 10.5),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8)),
      panel.grid.major = element_line(color = grid_line, linewidth = .35),
      panel.grid.minor = element_blank(),
      panel.spacing = grid::unit(1.05, "lines"),
      strip.text = element_text(color = ink, face = "bold", size = 10.5, hjust = 0,
                                margin = margin(b = 6, t = 3)),
      legend.position = "bottom", legend.title = element_blank(),
      legend.text = element_text(color = ink, size = 9.5),
      legend.key.width = grid::unit(1.2, "lines")
    )
}

plot_signal <- function(result) {
  bars <- result$market$full
  f <- features(bars)
  archive_end <- max(result$market$archive$Date)
  candidates <- result$trades$signal[result$trades$rule == "Original rule" &
                                       result$trades$signal_date > archive_end]
  candidates <- candidates[candidates > 18 & candidates <= nrow(bars) - 8]
  s <- candidates[ceiling(length(candidates) / 2)]
  idx <- seq.int(s - 17L, s + 8L)
  d <- cbind(bars[idx, ], f[idx, ], session = idx)
  d$direction <- ifelse(d$Close >= d$Open, "Close >= open", "Close < open")
  markers <- data.frame(session = c(s, s + 1L - .19, s + 1L + .19),
                        price = c(bars$Close[s], bars$Open[s + 1L], bars$Close[s + 1L]),
                        mark = factor(c("Signal close", "Next open", "Next close"),
                                      c("Signal close", "Next open", "Next close")))
  ticks <- idx[seq.int(1, length(idx), by = 4L)]
  ggplot(d, aes(session)) +
    annotate("rect", xmin = s - .5, xmax = s + .5, ymin = -Inf, ymax = Inf, fill = "#E8F1F7") +
    annotate("rect", xmin = s + .5, xmax = s + 1.5, ymin = -Inf, ymax = Inf, fill = "#F8EAE3") +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#9EC9C4", alpha = .23) +
    geom_line(aes(y = mid), linewidth = .7, linetype = "longdash", color = teal) +
    geom_segment(aes(xend = session, y = Low, yend = High, color = direction), linewidth = .55) +
    geom_rect(aes(xmin = session - .24, xmax = session + .24, ymin = pmin(Open, Close),
                  ymax = pmax(Open, Close), fill = direction, color = direction), linewidth = .42) +
    geom_point(data = markers, aes(x = session, y = price, shape = mark), inherit.aes = FALSE,
               fill = "#FFFDF8", color = ink, size = 3, stroke = .95) +
    scale_shape_manual(values = c("Signal close" = 21, "Next open" = 24, "Next close" = 22)) +
    scale_color_manual(values = c("Close >= open" = teal, "Close < open" = rust), guide = "none") +
    scale_fill_manual(values = c("Close >= open" = "#E0EFEC", "Close < open" = rust), guide = "none") +
    scale_x_continuous(breaks = ticks, labels = format(bars$Date[ticks], "%m/%d")) +
    scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
    labs(title = "A signal at the close, a trade in the next session",
         subtitle = sprintf("%s signal / %s execution. Dashed line: 20-session average. Shading: +/- 2 SD.",
                            bars$Date[s], bars$Date[s + 1L]),
         x = "Hong Kong trading sessions", y = "Hang Seng Index (points)") +
    chart_theme()
}

plot_equity <- function(result) {
  daily <- result$daily
  archive_end <- max(result$market$archive$Date)
  over <- function(type, log_r) {
    nav <- exp(cumsum(log_r))
    data.frame(date = daily$date, type, net = nav - 1, drawdown = nav / cummax(c(1, nav))[-1L] - 1)
  }
  a <- over("Original rule, gross", daily$rule_1_gross_log)
  b <- over("Original rule, 5 bp", daily$rule_1_net_log)
  d <- over("Development selection, 5 bp", daily$rule_3_net_log)
  values <- rbind(a, b, d)
  metrics <- rbind(cbind(values[c("date", "type")], value = values$net, measure = "Cumulative return"),
                   cbind(values[c("date", "type")], value = values$drawdown, measure = "Drawdown"))
  metrics$measure <- factor(metrics$measure, levels = c("Cumulative return", "Drawdown"))
  names <- c("Original rule, gross", "Original rule, 5 bp", "Development selection, 5 bp")
  ggplot(metrics, aes(date, value, color = type, linetype = type)) +
    annotate("rect", xmin = archive_end + 1L, xmax = max(daily$date), ymin = -Inf, ymax = Inf,
             fill = "#EDF5F6") +
    geom_hline(yintercept = 0, linewidth = .42, color = "#879BA4") +
    geom_vline(xintercept = as.Date("2017-01-01"), color = "#A1B1B5", linewidth = .4, linetype = "dotted") +
    geom_line(linewidth = .68) +
    facet_wrap(~measure, ncol = 1L, scales = "free_y") +
    scale_color_manual(values = setNames(c(blue, rust, teal), names), breaks = names) +
    scale_linetype_manual(values = setNames(c("solid", "22", "solid"), names), breaks = names) +
    scale_x_date(breaks = as.Date(paste0(seq(2000L, 2025L, by = 5L), "-01-01")), date_labels = "%Y") +
    scale_y_continuous(labels = percent_axis()) +
    labs(title = "Compounded capital and drawdowns",
         subtitle = "Dotted line: end of development. Shaded area: sessions after the original archive.",
         x = NULL, y = NULL) +
    chart_theme()
}

plot_surfaces <- function(result) {
  keep <- c("Development", "Historical holdout", "Post-archive")
  g <- subset(result$grid, period %in% keep)
  g$period <- factor(g$period, keep, c("Development / 2000-16", "Historical holdout / 2020-25",
                                     "Post-archive / 2025-26"))
  anchors <- merge(result$rules[c(1L, 3L), ], data.frame(period = levels(g$period)))
  limit <- max(abs(g$annualized_net))
  ggplot(g, aes(a, b)) +
    geom_tile(aes(fill = annualized_net), width = .102, height = .102) +
    geom_point(data = anchors, aes(shape = rule), size = 3.2, color = ink, fill = "white", stroke = 1) +
    facet_wrap(~period, nrow = 1L) +
    scale_fill_gradient2(low = "#B74232", mid = "#F8F6EE", high = "#116E70",
                         limits = c(-limit, limit), labels = percent_axis(.1),
                         name = "Annualized net", guide = guide_colorbar(barwidth = 8, barheight = .65)) +
    scale_shape_manual(values = c("Original rule" = 21, "Development selection" = 24),
                       breaks = c("Original rule", "Development selection")) +
    scale_x_continuous(breaks = c(.1, 1, 2.2), expand = expansion(0)) +
    scale_y_continuous(breaks = c(.1, 1, 2.2), expand = expansion(0)) +
    coord_fixed() +
    labs(title = "The same 484 rules across three time periods",
         subtitle = "Annualized net return on a common scale. Next-open entry, 5 bp per side.",
         x = "Minimum lower shadow / body (a)", y = "Maximum upper shadow / body (b)") +
    chart_theme() + theme(panel.grid = element_blank())
}

plot_costs <- function(result) {
  keep <- c("Archived sample", "Historical holdout", "Post-archive")
  d <- subset(result$metrics, period %in% keep)
  d$period <- factor(d$period, keep, c("Full archive", "Historical holdout", "Post-archive"))
  ggplot(d, aes(fee_bps, net_compound, color = rule, shape = rule)) +
    geom_hline(yintercept = 0, color = "#879BA4", linewidth = .45) +
    geom_vline(xintercept = research_fee, color = "#A1B1B5", linewidth = .45, linetype = "dashed") +
    geom_line(linewidth = .8) + geom_point(size = 2, stroke = .8) +
    facet_wrap(~period, nrow = 1L, scales = "free_y") +
    scale_x_continuous(breaks = c(0, 2, 5, 10, 20)) +
    scale_y_continuous(labels = percent_axis(1)) +
    scale_color_manual(values = strategy_colors, breaks = names(strategy_colors), labels = rule_short) +
    scale_shape_manual(values = c(16, 15, 17), breaks = names(strategy_colors), labels = rule_short) +
    labs(title = "Turnover changes the cost of a signal",
         subtitle = "Each panel starts with fresh capital. Vertical axes differ. The dashed line marks 5 bp.",
         x = "Cost per side (basis points)", y = "Cumulative net return") +
    chart_theme()
}

plot_nulls <- function(result) {
  draws <- result$null$draws
  summary <- subset(result$null$tail, statistic == "matched_log")
  summary$panel <- sprintf("%s  |  p = %.3f", summary$model, summary$p_one_sided)
  draws$panel <- factor(summary$panel[match(draws$model, summary$model)], levels = summary$panel)
  summary$panel <- factor(summary$panel, levels = summary$panel)
  ggplot(draws, aes(100 * matched_log)) +
    geom_histogram(bins = 29L, fill = "#C4DCE6", color = "#FFFFFF", linewidth = .22) +
    geom_vline(aes(xintercept = 100 * observed_log), data = summary,
               color = rust, linewidth = .87) +
    facet_wrap(~panel, ncol = 2L) +
    labs(title = "Null distributions for the exposure-matched differential",
         subtitle = "500 simulations per model. Vertical line: observed archive value.",
         x = "Cumulative log-return differential (percentage points)", y = "Simulated paths") +
    chart_theme()
}

plot_edges <- function(result) {
  d <- subset(result$edge, block == 10L)
  get_names <- c("Archived sample", "Historical holdout", "Post-archive")
  d$period <- factor(d$period, rev(get_names))
  ggplot(d, aes(edge_bp, period)) +
    geom_vline(xintercept = 0, color = "#829AA2", linewidth = .6, linetype = "dashed") +
    geom_segment(aes(x = ci_low_bp, xend = ci_high_bp, yend = period), color = teal, linewidth = 1) +
    geom_point(shape = 23, fill = teal, color = teal, size = 3.1) +
    labs(title = "Were signal-session returns higher?",
         subtitle = "Mean log return on entry sessions minus all eligible non-entry sessions. 95% block intervals.",
         x = "Conditional difference (basis points per session)", y = NULL) +
    chart_theme() + theme(panel.grid.major.y = element_blank())
}

plot_latency <- function(result) {
  q <- result$latency
  ggplot(q, aes(lag, edge_bp)) +
    geom_hline(yintercept = 0, color = "#829AA2", linewidth = .5, linetype = "dashed") +
    geom_errorbar(aes(ymin = low_bp, ymax = high_bp), width = .13, color = teal, linewidth = .75) +
    geom_line(color = "#8CB6B3", linewidth = .5) +
    geom_point(shape = 21, fill = teal, color = "white", stroke = .65, size = 3.8) +
    scale_x_continuous(breaks = seq_len(nrow(q))) +
    labs(title = "Does the difference emerge at a later entry?",
         subtitle = "Same set of signal dates at all lags. 95% simultaneous bands cover all five estimates.",
         x = "Trading sessions after the signal", y = "Conditional difference (bp)") +
    chart_theme()
}

plot_walk <- function(result) {
  d <- result$walk$daily
  fixed <- data.frame(date = d$date, series = "Original rule", value = expm1(cumsum(d$fixed)))
  tuned <- data.frame(date = d$date, series = "Annual reselection", value = expm1(cumsum(d$tuned)))
  labels <- c("Original rule", "Annual reselection")
  ggplot(rbind(fixed, tuned), aes(date, value, color = series, linetype = series)) +
    geom_hline(yintercept = 0, color = "#829AA2", linewidth = .42) +
    geom_line(linewidth = .82) +
    scale_color_manual(values = setNames(c(blue, teal), labels), breaks = labels) +
    scale_linetype_manual(values = setNames(c("22", "solid"), labels), breaks = labels) +
    scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
    scale_y_continuous(labels = percent_axis()) +
    labs(title = "Annual reselection with an expanding training window",
         subtitle = "Each year's parameters use prior years only. Both rules pay 5 bp per side.",
         x = NULL, y = "Cumulative net return") +
    chart_theme()
}

render_figures <- function(result) {
  dir.create("figures", showWarnings = FALSE)
  makers <- setNames(list(plot_signal, plot_equity, plot_surfaces, plot_walk, plot_costs,
                         plot_edges, plot_latency, plot_nulls),
                     c("01-signal", "02-equity", "03-sensitivity", "04-walk-forward",
                       "05-cost", "06-edge", "07-delay", "08-null-models"))
  heights <- c(5.05, 6.2, 4.75, 4.42, 4.55, 3.4, 4.25, 5.32)
  for (i in seq_along(makers)) {
    fig <- makers[[i]](result)
    stem <- file.path("figures", names(makers)[i])
    ggsave(paste0(stem, ".png"), fig, device = ragg::agg_png, width = 10,
           height = heights[i], dpi = 160, bg = "white")
    ggsave(paste0(stem, ".svg"), fig, device = svglite::svglite, width = 10,
           height = heights[i], bg = "white")
  }
  invisible(NULL)
}
