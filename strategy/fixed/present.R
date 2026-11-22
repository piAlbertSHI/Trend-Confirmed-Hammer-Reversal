fixed_colors <- c("TCHR" = blue, "Buy and hold" = "#687884",
                  "Original intraday" = "#9B672B", "Overnight" = teal,
                  "Open to close" = rust, "Matched exposure" = teal)
model_names <- c("Random walk", "AR(1)", "GARCH-M", "EGARCH")
measure_names <- c("Net" = "TCHR net", "Active" = "Relative to buy and hold",
                   "Exposure-adjusted" = "Relative to equal exposure",
                   "Confirmation increment" = "Confirmation vs same trend",
                   "Shape increment" = "Hammer vs same trigger")

long_series <- function(d, columns) {
  do.call(rbind, lapply(names(columns), function(name) {
    data.frame(date = d$date, id = name, value = columns[[name]])
  }))
}

date_ticks <- function() {
  scale_x_date(breaks = c(fixed_start, as.Date(paste0(seq(2005L, 2025L, 5L), "-01-01"))), date_labels = "%Y")
}

fixed_diagnose_chart <- function(history) {
  d <- history$daily
  series <- long_series(d, list("Overnight" = cumsum(d$overnight_log), "Open to close" = cumsum(d$intraday_log),
                               "Buy and hold" = cumsum(d$overnight_log + d$intraday_log)))
  ggplot(series, aes(date, value, color = id)) +
    geom_hline(yintercept = 0, color = "#91A7AF", linewidth = .5) +
    geom_line(linewidth = .76) +
    scale_color_manual(values = fixed_colors, breaks = c("Buy and hold", "Overnight", "Open to close")) +
    scale_y_continuous(labels = percent_axis()) + date_ticks() +
    labs(title = "How the Hang Seng return accumulated",
         subtitle = "Overnight and open-to-close components sum to the gross buy-and-hold log return.",
         x = NULL, y = "Cumulative log return") + chart_theme()
}

fixed_equity_chart <- function(history) {
  d <- history$daily
  nav <- function(key) exp(cumsum(d[[key]]))
  columns <- list("TCHR" = nav("TCHR log"),
                  "Buy and hold" = nav("Buy and hold log"), "Original intraday" = nav("Original intraday log"))
  curves <- lapply(columns, function(x) x - 1)
  dd <- lapply(columns, function(x) x / cummax(c(1, x))[-1L] - 1)
  series <- rbind(cbind(panel = "Cumulative net return", long_series(d, curves)),
                  cbind(panel = "Drawdown", long_series(d, dd)))
  ggplot(series, aes(date, value, color = id, linetype = id)) +
    geom_hline(yintercept = 0, color = "#91A7AF", linewidth = .44) +
    geom_line(linewidth = .72) +
    facet_wrap(~panel, ncol = 1L, scales = "free_y") +
    scale_color_manual(values = fixed_colors, breaks = names(columns)) +
    scale_linetype_manual(values = setNames(c("solid", "42", "13"), names(columns)), breaks = names(columns)) +
    scale_y_continuous(labels = percent_axis()) + date_ticks() +
    labs(title = "TCHR performance and drawdown",
         subtitle = "Net values include 5 basis points per side. All portfolios start with the same capital.",
         x = NULL, y = NULL) + chart_theme()
}

fixed_fee_chart <- function(history) {
  keys <- c("TCHR", "Buy and hold", "Original intraday")
  d <- subset(history$performance, rule %in% keys)
  ggplot(d, aes(fee_bps, net, color = rule, shape = rule)) +
    geom_hline(yintercept = 0, color = "#91A7AF", linewidth = .44) +
    geom_vline(xintercept = trial_fee, color = "#91A7AF", linewidth = .44, linetype = "dashed") +
    geom_line(linewidth = .94) + geom_point(size = 2.45) +
    scale_color_manual(values = fixed_colors, breaks = keys) +
    scale_shape_manual(values = setNames(c(16, 15, 17), keys), breaks = keys) +
    scale_x_continuous(breaks = c(0, 5, 10, 20, 30)) +
    scale_y_continuous(labels = percent_axis()) +
    labs(title = "TCHR net compounding across trading-cost assumptions",
         subtitle = "Single historical window. The dashed line marks the primary cost scenario.",
         x = "One-way cost (basis points)", y = "Cumulative net return") + chart_theme()
}

fixed_forest_chart <- function(history) {
  d <- subset(history$intervals, block == 20L)
  d$label <- factor(unname(measure_names[d$metric]), levels = rev(unname(measure_names)))
  ggplot(d, aes(observed_annual_log, label)) +
    geom_vline(xintercept = 0, color = "#91A7AF", linewidth = .5, linetype = "dashed") +
    geom_segment(aes(x = low_annual_log, xend = high_annual_log, yend = label),
                 linewidth = .95, color = teal) +
    geom_point(color = teal, size = 2.55) +
    scale_x_continuous(labels = percent_axis(.1)) +
    labs(title = "Uncertainty in returns and incremental effects",
         subtitle = "95% circular block intervals. 1,999 resamples with 20-session blocks.",
         x = "Annualized log return or differential", y = NULL) + chart_theme() +
    theme(panel.grid.major.y = element_blank())
}

fixed_sim_chart <- function(models) {
  stat <- subset(models$summary, metric == "active_log")
  stat <- stat[match(model_names, stat$model), ]
  level <- sprintf("%s   |   p = %.3f", stat$model, stat$p)
  stat$panel <- factor(level, levels = level)
  d <- models$draws
  d$panel <- factor(level[match(d$model, model_names)], levels = level)
  ggplot(d, aes(active_log)) +
    geom_histogram(bins = 33L, fill = "#BDD6E1", color = "white", linewidth = .28) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "#859BA5", linewidth = .43) +
    geom_vline(data = stat, aes(xintercept = observed), color = rust, linewidth = .88) +
    facet_wrap(~panel, ncol = 2L) +
    scale_x_continuous(labels = percent_axis()) +
    labs(title = "Where the TCHR active return falls in each process",
         subtitle = "500 equal-length price paths per model. Rust line: measured historical active return.",
         x = "Cumulative active log return vs buy and hold", y = "Simulated paths") + chart_theme()
}

figure_ids <- c("01-return-source", "02-value-drawdown", "03-costs", "04-effects", "05-randomness")

make_fixed_figures <- function(history, models) {
  dir.create("strategy/fixed/figures", showWarnings = FALSE)
  sizes <- c(4.55, 6.2, 4.65, 3.85, 5.55)
  charts <- list(fixed_diagnose_chart(history), fixed_equity_chart(history), fixed_fee_chart(history),
                 fixed_forest_chart(history), fixed_sim_chart(models))
  for (i in seq_along(charts)) {
    path <- file.path("strategy/fixed/figures", figure_ids[i])
    ggsave(paste0(path, ".png"), charts[[i]], device = ragg::agg_png,
           width = 10, height = sizes[i], dpi = 162, bg = "white")
    ggsave(paste0(path, ".svg"), charts[[i]], device = svglite::svglite,
           width = 10, height = sizes[i], bg = "white")
  }
}

fixed_figure <- function(i, alt, caption) {
  uri <- base64enc::dataURI(file = file.path("strategy/fixed/figures", paste0(figure_ids[i], ".png")),
                           mime = "image/png")
  paste0('<figure tabindex="0"><img src="', uri, '" alt="', esc(alt), '" loading="lazy" decoding="async">',
         "<figcaption>", esc(caption), ' <a href="fixed/figures/', figure_ids[i],
         '.svg">SVG 图</a></figcaption></figure>')
}

make_fixed_report <- function(history, models) {
  sel <- function(name, fee = trial_fee) {
    p <- history$performance
    d <- p[p$rule == name & p$fee_bps == fee, ]
    stopifnot(nrow(d) == 1L)
    d
  }
  q <- sel("TCHR")
  b <- sel("Buy and hold")
  d <- history$diagnostic
  names <- c("TCHR", "同参数趋势", "同确认时序，移除形态", "布林下轨回归", "原日内规则", "恒指买入持有")
  group <- subset(history$performance, fee_bps == trial_fee)
  trading <- tab("单边 5 bp 历史回测", data.frame(
    "策略" = names, "交易数" = group$trades, "敞口" = pct(group$exposure),
    "净复利" = pct(group$net, sign = TRUE), "年化净收益" = pct(group$annualized, sign = TRUE),
    "最大回撤" = pct(group$drawdown), check.names = FALSE))
  row <- function(stat) {
    a <- subset(models$summary, metric == stat)
    a[match(model_names, a$model), ]
  }
  t <- row("active_log")
  sim <- tab("同一历史窗口的模拟单侧概率", data.frame(
    "模型" = c("随机游走", model_names[-1L]),
    "净收益 p" = formatted(row("net_log")$p, 3L),
    "相对恒指超额 p" = formatted(t$p, 3L),
    "等敞口超额 p" = formatted(row("matched_log")$p, 3L),
    "模拟同时净盈利和跑赢频率" = pct(t$positive_and_ahead, digits = 1L),
    check.names = FALSE))
  z <- subset(history$intervals, block == 20L & metric == "Active")
  mt <- subset(history$max_t, block == 20L)
  words <- lapply(list(
    FIRST = format(d$first), LAST = format(d$last), DAYS = formatted(d$sessions, 0L, grouping = TRUE),
    NET = pct(q$net, sign = TRUE), INDEX = pct(b$net, sign = TRUE),
    EXCESS = formatted(q$excess_pp, sign = TRUE), ANNUAL = pct(q$annualized),
    DD = pct(q$drawdown), INDEX_DD = pct(b$drawdown),
    OLD_N = d$old_trades, OLD_EXP = pct(d$old_exposure), OLD_BP = formatted(d$old_bp_gross, sign = TRUE),
    OLD_NET = pct(d$old_net), GAP = pct(d$buyhold_overnight_log, sign = TRUE),
    DAY = pct(d$buyhold_intraday_log), NO_SHAPE = pct(sel("Same confirmation without shape")$net, sign = TRUE),
    OVERLAY = formatted(100 * (q$net - sel("Same trend only")$net), sign = TRUE),
    TRADES = q$trades, DURATION = formatted(q$holding_days, 1L), EXPOSURE = pct(q$exposure), MATCHED = pct(d$matched_compound),
    NO_FEE = pct(sel("TCHR", 0)$net, sign = TRUE), HIGH_FEE = pct(sel("TCHR", 30)$net, sign = TRUE),
    ACTIVE = pct(z$observed_annual_log, sign = TRUE),
    ACTIVE_CI = paste0("[", pct(z$low_annual_log), ", ", pct(z$high_annual_log), "]"),
    RAW_P = formatted(mt$primary_p, 3L), ADJ_P = formatted(mt$primary_adjusted_p, 3L)
  ), esc)
  charts <- list(
    DIAG = fixed_figure(1L, "同一时期的隔夜与开收盘对数收益", "图为对数口径的市场收益分解。"),
    EQUITY = fixed_figure(2L, "TCHR、原日内规则和恒指的净复利与回撤", "三条曲线使用相同的日期、费用口径与起始资金。"),
    FEES = fixed_figure(3L, "三种组合的单边费用敏感性", "虚线标示主费用情景。"),
    INTERVALS = fixed_figure(4L, "区块自助法估计的超额收益区间", "收益与收益差按每年 252 日换算。"),
    NULL = fixed_figure(5L, "四种随机过程的累计超额分布", "红线表示原始价格序列的观察值。")
  )
  css <- paste(utf8_text("report/style.css"), utf8_text("strategy/style.css"),
               "@media(max-width:620px){.key-results .excess-unit{display:block;line-height:1.25;margin-top:.2em}}", sep = "\n")
  values <- c(list(CSS = css, PERFORMANCE_TABLE = trading, NULL_TABLE = sim), words, charts)
  page <- utf8_text("strategy/fixed/report.html.in")
  markers <- gregexpr("@@[A-Z_0-9]+@@", page)
  keys <- gsub("@@", "", regmatches(page, markers)[[1L]], fixed = TRUE)
  stopifnot(all(keys %in% names(values)), !anyDuplicated(names(values)))
  regmatches(page, markers) <- list(unlist(values[keys], use.names = FALSE))
  stopifnot(!grepl("@@[A-Z_0-9]+@@", page))
  writeLines(enc2utf8(page), "strategy/report.html", useBytes = TRUE)
}
