strategy_figure <- function(id, alt, text) {
  file <- file.path("strategy", "figures", paste0(id, ".png"))
  source <- base64enc::dataURI(file = file, mime = "image/png")
  paste0('<figure tabindex="0"><img src="', source, '" alt="', esc(alt),
         '" loading="lazy" decoding="async"><figcaption>', esc(text),
         ' <a href="figures/', id, '.svg">SVG 矢量图</a></figcaption></figure>')
}

performance_row <- function(scores, period, instrument = "^HSI", label = "Selected", fee = trial_fee) {
  d <- scores[scores$period == period & scores$instrument == instrument & scores$rule == label &
                scores$fee_bps == fee, , drop = FALSE]
  stopifnot(nrow(d) == 1L)
  d
}

p_value <- function(d, metric, model, target = "Out of sample") {
  p <- d$p_one_sided[d$metric == metric & d$model == model & d$period == target]
  stopifnot(length(p) == 1L)
  p
}

strategy_tables <- function(x, null, pullback, followup) {
  map <- c("Complete" = "完整历史", "Development" = "开发期", "Validation" = "2017–2019",
           "Historical holdout" = "2020 年至原档末", "Post-archive" = "报告后扩展",
           "Out of sample" = "2020 年起合计")
  h <- lapply(names(map), function(s) performance_row(x$scores, s, "^HSI"))
  f <- lapply(names(map), function(s) performance_row(x$scores, s, "2800.HK"))
  ret <- function(rows, col) pct(vapply(rows, function(d) d[[col]], 0.0), sign = TRUE)
  perf <- tab("冻结的均线＋锤头线规则。净复利与同期买入持有", data.frame(
    "区间" = unname(map), "恒指策略" = ret(h, "net"), "恒指基准" = ret(h, "benchmark_net"),
    "盈富策略" = ret(f, "net"), "盈富基准" = ret(f, "benchmark_net"), check.names = FALSE))
  pr <- list(performance_row(pullback$scores, "Out of sample", "^HSI", "P06"),
             performance_row(pullback$scores, "Out of sample", "2800.HK", "P06"),
             performance_row(pullback$scores, "Post-archive", "^HSI", "P06"),
             performance_row(pullback$scores, "Post-archive", "2800.HK", "P06"),
             performance_row(followup$early, "Earlier historical sample", "^HSI", "P06"))
  retest <- tab("P06 固定规则。单边 5 bp", data.frame(
    "样本" = c("2020 年起 · 恒指", "2020 年起 · 盈富", "报告后扩展 · 恒指",
              "报告后扩展 · 盈富", "1990–2000 年 · 恒指"),
    "交易数" = vapply(pr, function(d) d$trades, 0L),
    "持仓日期占比" = ret(pr, "exposure"),
    "策略净复利" = ret(pr, "net"), "买入持有" = ret(pr, "benchmark_net"), check.names = FALSE))
  models <- c("Random walk", "AR(1)", "GARCH-M", "EGARCH")
  v <- function(metric) formatted(vapply(models, function(m) p_value(null$summary, metric, m), 0.0), 3L)
  stochastic <- tab("2020 年后的模拟尾部概率。每模型 500 条", data.frame(
    "模型" = c("随机游走", models[-1L]), "冻结规则净收益 p" = v("Selected net"),
    "冻结规则超额 p" = v("Selected vs index"), "P06 净收益 p" = v("P06 net"),
    "P06 超额 p" = v("P06 vs index"), check.names = FALSE))
  list(PERF_TABLE = perf, FOLLOWUP_TABLE = retest, NULL_TABLE = stochastic)
}

render_strategy_report <- function(x, null, pullback, followup) {
  whole <- performance_row(x$scores, "Complete")
  main <- performance_row(x$scores, "Out of sample")
  zero <- performance_row(x$scores, "Out of sample", fee = 0)
  near <- performance_row(pullback$scores, "Out of sample", label = "P06")
  near_fund <- performance_row(pullback$scores, "Out of sample", "2800.HK", "P06")
  earlier <- performance_row(followup$early, "Earlier historical sample", label = "P06")
  pullback_first <- performance_row(pullback$scores, "Out of sample", label = pullback$top$id)
  dx <- subset(x$diagnostics, period == "Archived sample")
  value_for <- function(label) sum(x$data_oos[[paste0(label, " log")]])
  shape <- value_for("Trend + hammer best")
  p06 <- pullback$ranking[pullback$ranking$id == "P06", ]
  words <- lapply(list(
    HISTORY_FIRST = as.character(min(x$spans$start)), HISTORY_LAST = as.character(max(x$spans$end)),
    MAIN_FULL = pct(whole$net, sign = TRUE), BASE_FULL = pct(whole$benchmark_net, sign = TRUE),
    MAIN_OOS = pct(main$net), BASE_OOS = pct(main$benchmark_net),
    P06_OOS = pct(near$net, sign = TRUE), P06_ETF_OOS = pct(near_fund$net, sign = TRUE),
    OLD_TRADES = dx$trades, OLD_EXPOSURE = pct(dx$share), OLD_TRADE_BP = formatted(dx$gross_bp_per_signal),
    OLD_GAP_LOG = pct(dx$overnight_sum_log, sign = TRUE), OLD_DAY_LOG = pct(dx$intraday_sum_log),
    EXPOSURE = pct(whole$exposure), N_FULL = whole$trades,
    MAIN_DD = pct(whole$drawdown), BASE_DD = pct(whole$benchmark_drawdown),
    FREE_MAIN_OOS = pct(zero$net), FREE_BASE_OOS = pct(zero$benchmark_net),
    ANNUAL_WALK = pct(x$walk$summary$net, sign = TRUE), ANNUAL_BASE = pct(x$walk$summary$benchmark_net, sign = TRUE),
    OVERLAY_DELTA = pct(shape - value_for("Same trend without overlay"), sign = TRUE),
    SHAPE_DELTA = pct(shape - value_for("Same overlay without hammer"), sign = TRUE),
    PULLBACK_TRAIN_PICK_OOS = pct(pullback_first$net),
    P06_TRADES = near$trades, P06_DEV_EXPO = pct(p06$exposure),
    P06_EARLY_TRADES = earlier$trades, P06_EARLY = pct(earlier$net, sign = TRUE), P06_EARLY_BASE = pct(earlier$benchmark_net, sign = TRUE),
    MAIN_MAX_P = formatted(x$family$adjusted_p[x$family$block == 20L], 3L),
    P06_MAX_P = formatted(followup$adjusted$adjusted_p[followup$adjusted$block == 20L], 3L),
    HAMMER_PERM = formatted(null$random_entry$summary$p_one_sided, 3L),
    FUND_DAYS = formatted(x$fund$audit$matched_sessions, 0L, grouping = TRUE)
  ), esc)
  charts <- list(
    DIAG_FIG = strategy_figure("01-return-source", "原档中开收盘、隔夜及买入持有累计收益",
                                "图中对数收益以相加方式分解，单独隔夜曲线用于诊断市场收益时段。"),
    FULL_FIG = strategy_figure("02-full-wealth", "原规则、冻结新规则及大盘的全历史复利",
                                "原始行情统一预热 220 个交易日。阴影为 2020 年起的主评估区间。"),
    OOS_FIG = strategy_figure("03-evaluation", "2020 年起在恒指和盈富基金代理上的净值",
                               "两组在各自首个可执行日重置为零。下轨回归属于后续探索。"),
    COST_FIG = strategy_figure("04-fees", "全历史和评估期的单边费用敏感性",
                                "包括 0、2、5、10、20、30 bp。两幅面板使用各自的纵轴范围。"),
    INTERVAL_FIG = strategy_figure("05-effect-intervals", "市场比较与锤头线消融的时间区块区间",
                                    "逐日主动对数收益按每年 252 日换算。静态等敞口对照使用冻结规则在各区间的平均持仓比例。"),
    EARLIER_FIG = strategy_figure("06-earlier-history", "P06 对比买入持有的档案前独立历史路径",
                                   "此区间行情单独抓取，参数保留后期选定设置。"),
    NULL_FIG = strategy_figure("07-randomness", "四种零模型中的冻结策略累计超额分布",
                                "每条路径使用同路径指数作为对照，红线标示真实价格序列的观察值。")
  )
  tokens <- c(list(CSS = paste(utf8_text("report/style.css"), utf8_text("strategy/style.css"), sep = "\n")),
              words, strategy_tables(x, null, pullback, followup), charts)
  contents <- utf8_text("strategy/template.html")
  matches <- gregexpr("@@[A-Z_0-9]+@@", contents)
  keys <- gsub("@@", "", regmatches(contents, matches)[[1L]], fixed = TRUE)
  stopifnot(all(keys %in% names(tokens)), !anyDuplicated(names(tokens)))
  regmatches(contents, matches) <- list(unlist(tokens[keys], use.names = FALSE))
  stopifnot(!grepl("@@[A-Z_0-9]+@@", contents))
  writeLines(enc2utf8(contents), "strategy/research-detail.html", useBytes = TRUE)
  invisible("strategy/research-detail.html")
}
