utf8_text <- function(path) {
  paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

esc <- function(x) htmltools::htmlEscape(enc2utf8(as.character(x)))

formatted <- function(x, digits = 2L, sign = FALSE, grouping = FALSE) {
  pattern <- paste0("%", if (sign) "+" else "", ".", digits, "f")
  out <- sprintf(pattern, x)
  if (grouping) {
    magnitude <- formatC(abs(x), format = "f", digits = digits, big.mark = ",")
    prefix <- ifelse(x < 0, "-", if (sign) "+" else "")
    out <- paste0(prefix, magnitude)
  }
  replace(out, is.na(x), "—")
}

pct <- function(x, sign = FALSE, digits = 2L) {
  replace(paste0(formatted(100 * x, digits, sign), "%"), is.na(x), "—")
}

tab <- function(label, display) {
  head <- paste0('<th scope="col">', esc(names(display)), "</th>", collapse = "")
  body <- vapply(seq_len(nrow(display)), function(i) {
    cells <- esc(unlist(display[i, ], use.names = FALSE))
    paste0('<tr><th scope="row">', cells[1L], "</th>",
           paste0("<td>", cells[-1L], "</td>", collapse = ""), "</tr>")
  }, "")
  paste0('<div class="table-scroll" tabindex="0" role="region" aria-label="', esc(label), '">',
         "<table><caption>", esc(label), "</caption><thead><tr>", head,
         "</tr></thead><tbody>", paste0(body, collapse = "\n"), "</tbody></table></div>")
}

figure <- function(id, alt, description) {
  file <- file.path("figures", paste0(id, ".png"))
  form <- base64enc::dataURI(file = file, mime = "image/png")
  paste0('<figure tabindex="0" aria-label="', esc(alt), '"><img src="', form, '" alt="',
         esc(alt), '" loading="lazy" decoding="async"><figcaption>', esc(description),
         ' <a href="figures/', id, '.svg">SVG 矢量图</a></figcaption></figure>')
}

three_rules <- function(result, period, fee) {
  d <- result$metrics[result$metrics$period == period & result$metrics$fee_bps == fee, , drop = FALSE]
  d[match(result$rules$rule, d$rule), ]
}

report_tables <- function(x) {
  labels <- c("原档全样本", "开发期", "验证期", "历史留出", "报告后区间")
  audit <- x$reconciliation
  audit_table <- tab("归档规则与校正时序。均为零成本", data.frame(
    "规则" = c("原版固定规则", "按列名读取，次日买入", "原版调参脚本",
               "按列名读取，信号当日买入", "按列名读取，次日买入"),
    "阈值 (a, b)" = cat_rule(audit$a, audit$b),
    "入场" = ifelse(audit$signal_to_entry > 0, "次日开盘", "当日开盘"),
    "交易数" = audit$trades,
    "累计点数" = formatted(audit$sum_points, sign = TRUE, grouping = TRUE),
    "胜率" = pct(audit$win), check.names = FALSE
  ))

  period_specs <- x$periods$period
  settings <- lapply(period_specs, function(s) three_rules(x, s, research_fee))
  cell <- function(d, j) paste0(pct(d$net_compound[j], sign = TRUE), "（", d$trades[j], " 笔）")
  perf_table <- tab("各区间净复利与交易数。单边 5 bp", data.frame(
    "区间" = labels, "原规则" = vapply(settings, cell, "", j = 1L),
    "报告调参" = vapply(settings, cell, "", j = 2L),
    "开发期选参" = vapply(settings, cell, "", j = 3L), check.names = FALSE
  ))

  e <- subset(x$edge, block == 10L)
  int <- function(low, high, digits = 2L) {
    paste0("[", formatted(low, digits), ", ", formatted(high, digits), "]")
  }
  edges <- tab("原规则的条件收益差。10 日区块", data.frame(
    "区间" = labels[match(e$period, period_specs)], "交易数" = e$trades,
    "差值 (bp)" = formatted(e$edge_bp, sign = TRUE),
    "95% 区间 (bp)" = int(e$ci_low_bp, e$ci_high_bp),
    "单侧 p" = formatted(e$p_one_sided, 3L), check.names = FALSE
  ))

  t_rules <- c("Mid k=1", "Mid k=2", "Mid k=3", "Mid hold 2", "Lower k=1", "Lower k=2")
  old <- subset(x$timing, source == "Swapped columns")
  named <- subset(x$timing, source == "Named columns")
  old <- old[match(t_rules, old$rule), ]
  named <- named[match(t_rules, named$rule), ]
  timing <- tab("执行迟滞与持有期。原档全样本", data.frame(
    "规则" = c("中轨，延迟 1 日", "中轨，延迟 2 日", "中轨，延迟 3 日",
               "中轨，持有 2 日", "下轨，延迟 1 日", "下轨，延迟 2 日"),
    "换列毛点数" = formatted(old$rounded_points, sign = TRUE, grouping = TRUE),
    "校正毛点数" = formatted(named$rounded_points, sign = TRUE, grouping = TRUE),
    "校正交易数" = named$trades,
    "校正净复利" = pct(named$net_compound, sign = TRUE), check.names = FALSE
  ))

  m <- x$max_adjustment
  max_t <- tab("开发期规则族的多重检验", data.frame(
    "区块长度" = paste(m$block, "日"), "合格规则" = m$eligible,
    "最高 t 对应阈值" = cat_rule(m$a, m$b), "最大 t" = formatted(m$max_t),
    "单规则 p" = formatted(m$naive_p, 3L), "全族校正 p" = formatted(m$adjusted_p, 3L),
    check.names = FALSE
  ))

  n <- subset(x$null$tail, statistic == "matched_log")
  g <- subset(x$null$tail, statistic == "log_return")
  null_table <- tab("4 个模型的单侧尾部概率。各 500 条路径", data.frame(
    "过程" = c("随机游走", "AR(1)", "GARCH-M", "EGARCH"),
    "毛收益 p" = formatted(g$p_one_sided, 3L),
    "敞口匹配 p" = formatted(n$p_one_sided, 3L),
    "敞口匹配 p 的模拟区间" = int(n$mc_low, n$mc_high, 3L),
    "模拟交易数中位数" = n$median_trades, check.names = FALSE
  ))

  v <- x$market$coverage
  coverage <- tab("行情档案与扩展数据", data.frame(
    "数据段" = c("原始研究档案", "来源校验重叠段", "报告后扩展"),
    "首个交易日" = format(v$first), "最后交易日" = format(v$last),
    "交易日数" = formatted(v$sessions, 0L, grouping = TRUE), check.names = FALSE
  ))
  list(AUDIT_TABLE = audit_table, PERIOD_TABLE = perf_table, EDGE_TABLE = edges,
       TIMING_TABLE = timing, MAX_TABLE = max_t, NULL_TABLE = null_table, COVERAGE_TABLE = coverage)
}

render_report <- function(x) {
  audit <- x$reconciliation
  orig <- three_rules(x, "Archived sample", research_fee)[1L, ]
  fwd <- three_rules(x, "Post-archive", research_fee)[1L, ]
  ref <- subset(x$baselines, period == "Archived sample")
  ref <- setNames(ref$compound, ref$series)
  legacy <- x$legacy$summary
  legacy_g <- legacy[legacy$model == "GARCH-M", ]
  invalid <- 100 * range(legacy$mean_invalid_bar_share)
  costs <- subset(x$break_even, period == "Post-archive" & rule == "Original rule")
  calibr <- x$null$diagnostics
  entry <- x$rules[3L, ]
  words <- list(
    FIRST = format(min(x$market$full$Date)), LAST = format(max(x$market$full$Date)),
    SESSIONS = formatted(nrow(x$market$full), 0L, grouping = TRUE),
    LEGACY_POINTS = formatted(audit$sum_points[1L], sign = TRUE, grouping = TRUE),
    CORRECT_POINTS = formatted(audit$sum_points[2L], sign = TRUE, grouping = TRUE),
    N_ORIGINAL = orig$trades, ORIGINAL_GROSS = pct(orig$gross_compound),
    ORIGINAL_NET = pct(orig$net_compound), N_FORWARD = fwd$trades,
    FORWARD_SESSIONS = nrow(x$market$full) - nrow(x$market$archive),
    FORWARD_GROSS = pct(fwd$gross_compound, sign = TRUE), FORWARD_NET = pct(fwd$net_compound),
    TUNED_POINTS = formatted(audit$sum_points[3L], grouping = TRUE),
    TUNED_GAP = formatted(audit$sum_points[3L] - audit$report_points[3L], grouping = TRUE),
    TUNED_SAFE = pct(audit$compound[5L]), TRAIN_RULE = cat_rule(entry$a, entry$b),
    EXPOSURE = pct(orig$exposure), DRAWDOWN = pct(orig$net_drawdown),
    BUY_HOLD = pct(ref[["Buy and hold"]], sign = TRUE),
    DAILY_INTRADAY = pct(ref[["Daily intraday"]], sign = TRUE),
    MATCHED_REF = pct(ref[["Exposure-matched reference"]], sign = TRUE),
    FOLDS = nrow(x$walk$folds), WALK_TRADE = sum(x$walk$folds$tuned_trades),
    WALK_TUNED = pct(expm1(sum(x$walk$daily$tuned)), sign = TRUE),
    WALK_FIXED = pct(expm1(sum(x$walk$daily$fixed)), sign = TRUE),
    FORWARD_BREAK_EVEN = formatted(costs$breakeven_one_way_bp),
    GARCH_FAIL = legacy_g$failed_fit_as_zero, GARCH_VALID = formatted(legacy_g$finite_fits_tail, 3L),
    INVALID_SHARE = paste0(formatted(invalid[1L], 1), "%–", formatted(invalid[2L], 1), "%"),
    EGARCH_REPRO = formatted(legacy$reproduced_tail[legacy$model == "EGARCH"], 3L),
    GARCH_LB_SQ = formatted(calibr$ljung_box_sq_p[calibr$model == "GARCH-M"], 3L),
    EGARCH_LB_SQ = formatC(calibr$ljung_box_sq_p[calibr$model == "EGARCH"], format = "e", digits = 2L),
    OVERLAP = x$market$coverage$sessions[x$market$coverage$segment == "overlap"]
  )
  figs <- list(
    SIGNAL_FIG = figure("01-signal", "实际恒生指数 K 线与信号和执行日",
                        "按报告后合格信号的中位序号取示例。圆点为信号收盘，三角为下一日开盘，方形为退出收盘。"),
    EQUITY_FIG = figure("02-equity", "原规则和开发期选参的累计复利与回撤",
                        "连续净值使用整个合并样本。灰蓝区间标示原始档案截止后的交易日。"),
    SURFACES_FIG = figure("03-sensitivity", "三段时期的 484 组影线阈值表现",
                           "圆形标示固定原规则，三角形标示开发期选定阈值。所有颜色使用同一尺度。"),
    WALK_FIG = figure("04-walk-forward", "逐年重新选参与固定规则的时间外复利",
                       "当前测试年份不会进入该年的选参。最后一折截至 2025 年 4 月 1 日。"),
    COST_FIG = figure("05-cost", "三种规则在不同单边费率下的复利",
                       "报告调参的换手率较高，成本曲线下降更快。各图纵轴单独取值。"),
    EDGE_FIG = figure("06-edge", "三个区间的条件收益差和区块自助置信区间",
                       "区间跨过零。报告后样本的交易少，估计精度明显较低。"),
    LATENCY_FIG = figure("07-delay", "延迟一至五日的开收盘收益差及联合区间",
                          "纵轴以基点计。区间同时覆盖五个估计，体现比较多个入场日产生的不确定性。"),
    NULL_FIG = figure("08-null-models", "四种随机过程的敞口匹配收益差经验分布",
                       "红线为真实样本，落在各模型的模拟主体之内。")
  )
  tokens <- c(list(CSS = utf8_text("report/style.css")), lapply(words, esc), report_tables(x), figs)
  page <- utf8_text("report/template.html")
  markers <- gregexpr("@@[A-Z_0-9]+@@", page)
  keys <- gsub("@@", "", regmatches(page, markers)[[1L]], fixed = TRUE)
  stopifnot(all(keys %in% names(tokens)), !anyDuplicated(names(tokens)))
  regmatches(page, markers) <- list(unlist(tokens[keys], use.names = FALSE))
  stopifnot(!grepl("@@[A-Z_0-9]+@@", page))
  writeLines(enc2utf8(page), "report.html", useBytes = TRUE)
  invisible("report.html")
}
