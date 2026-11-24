library(zoo)
library(xts)
library(TTR)
library(ggplot2)
library(quantmod)
library(dplyr)

# ===== 用户设置参数 =====
n_sim <- 50  # ✅ 模拟次数，可调整
set.seed(42) # 保持可复现

# ===== 初始化 & 数据读取 =====
column_names <- read.csv("HSI_25years.csv", skip = 2, nrows = 1, header = FALSE)
col_names <- as.character(unlist(column_names))
data <- read.csv("HSI_25years.csv", skip = 3, header = FALSE, col.names = col_names)
colnames(data) <- c("Datetime", "Open", "High", "Low", "Close", "Volume")
data$Datetime <- as.POSIXct(data$Datetime, format = "%Y-%m-%d")
cols <- c("Open", "High", "Low", "Close", "Volume")
data[cols] <- lapply(data[cols], function(x) as.numeric(as.character(x)))
ohlc <- xts(data[, cols], order.by = data$Datetime)

# ===== 获取对数收益率 =====
Closes <- as.numeric(Cl(ohlc))
log_returns <- diff(log(Closes))[-1]
initial_price <- as.numeric(Cl(ohlc)[1])

# ===== 定义锤头线识别函数 =====
isHammer <- function(open, high, low, close, lower_shadow_ratio = 1.5, upper_shadow_ratio = 0.5) {
  body <- abs(close - open)
  lower_shadow <- pmin(open, close) - low
  upper_shadow <- high - pmax(open, close)
  return(lower_shadow > lower_shadow_ratio * body & upper_shadow < upper_shadow_ratio * body)
}

# ===== 用于存储所有模拟结果的 DataFrame =====
all_sim_df <- data.frame()

# ===== 主循环：进行 n_sim 次模拟 =====
for (i in 1:n_sim) {
  # 随机游走生成模拟价格
  sim_log_returns <- sample(log_returns, size = length(log_returns), replace = TRUE)
  cumulative_returns <- cumsum(sim_log_returns)
  sim_prices <- initial_price * exp(cumulative_returns)

  sim_ohlc <- xts(
    cbind(
      Open = c(initial_price, sim_prices[-length(sim_prices)]),
      High = sim_prices * (1 + abs(rnorm(length(sim_prices), 0, 0.01))),
      Low = sim_prices * (1 - abs(rnorm(length(sim_prices), 0, 0.01))),
      Close = sim_prices,
      Volume = 0
    ),
    order.by = index(ohlc)[1:length(sim_log_returns)]
  )

  # 布林带
  bb <- BBands(Cl(sim_ohlc), n = 20)
  sim_ohlc$mavg <- bb$mavg

  # 锤头线识别 + 信号生成
  sim_ohlc$Hammer <- mapply(
    isHammer,
    open = as.numeric(sim_ohlc$Open),
    high = as.numeric(sim_ohlc$High),
    low = as.numeric(sim_ohlc$Low),
    close = as.numeric(sim_ohlc$Close)
  )

  sim_ohlc$Signal <- NA
  sim_ohlc$Signal[which(sim_ohlc$Hammer == 1 & sim_ohlc$Close < sim_ohlc$mavg)] <- "Buy"

  df_plot <- data.frame(
    Date = index(sim_ohlc),
    Open = as.numeric(sim_ohlc$Open),
    Close = as.numeric(sim_ohlc$Close),
    Signal = sim_ohlc$Signal
  )

  df_plot <- na.omit(df_plot)
  buy_points <- df_plot[df_plot$Signal == "Buy", ]

  # 收益计算
  if (nrow(buy_points) > 0) {
    buy_points$Profit <- round(buy_points$Close - buy_points$Open, 2)
    buy_points$Cumulative_Profit <- cumsum(buy_points$Profit)
    buy_points$Sim_ID <- paste0("Sim_", i)
    all_sim_df <- rbind(all_sim_df, buy_points[, c("Date", "Sim_ID", "Cumulative_Profit")])
  }
}

# ===== 绘图：50次模拟策略的累计收益线图 =====
if (nrow(all_sim_df) > 0) {
  ggplot(all_sim_df, aes(x = Date, y = Cumulative_Profit, group = Sim_ID, color = Sim_ID)) +
    geom_line(alpha = 0.6) +
    labs(title = paste0(n_sim, " 次锤头线策略模拟：累计收益曲线"),
         x = "日期", y = "累计收益") +
    theme_minimal() +
    theme(legend.position = "none")
} else {
  cat("\n⚠️ 所有模拟中均未触发交易信号。\n")
}


# ===== 收益总结统计 =====
if (nrow(all_sim_df) > 0) {
  # 提取每次模拟的最终累计收益
  final_profit_df <- all_sim_df %>%
    group_by(Sim_ID) %>%
    summarise(Final_Profit = last(Cumulative_Profit))
  
  cat("\n==== 多次模拟策略收益总结 ====\n")
  cat("模拟次数：", n_sim, "\n")
  cat("有信号的模拟次数：", nrow(final_profit_df), "\n")
  cat("平均最终收益：", round(mean(final_profit_df$Final_Profit), 2), "\n")
  cat("中位数收益：", round(median(final_profit_df$Final_Profit), 2), "\n")
  cat("最大收益：", round(max(final_profit_df$Final_Profit), 2), "\n")
  cat("最小收益：", round(min(final_profit_df$Final_Profit), 2), "\n")
  cat("胜率（最终收益 > 0 的比例）：", round(mean(final_profit_df$Final_Profit > 0) * 100, 2), "%\n")
  
}
