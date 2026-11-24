library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(ggplot2)
library(dplyr)
library(boot)
library(quantmod)



rm(list = ls())
# ========== 📥 读取 CSV ==========
column_names <- read.csv("HSI_25years.csv", skip = 2, nrows = 1, header = FALSE)
col_names <- as.character(unlist(column_names))

data <- read.csv("HSI_25years.csv", skip = 3, header = FALSE, col.names = col_names)
colnames(data) <- c("Datetime", "Open", "High", "Low", "Close", "Volume")

# ========== 🧹 数据清洗 ==========
data$Datetime <- as.POSIXct(data$Datetime, format = "%Y-%m-%d")
cols <- c("Open", "High", "Low", "Close", "Volume")
data[cols] <- lapply(data[cols], function(x) as.numeric(as.character(x)))

# ========== 🔁 构建 xts ==========
ohlc <- xts(data[, cols], order.by = data$Datetime)

# ========== 📊 计算布林带 ==========
bb <- BBands(Cl(ohlc), n = 20)
ohlc$mavg <- bb$mavg

# ========== 🔍 改进版锤头线识别函数 ==========
isHammer <- function(open, high, low, close, lower_shadow_ratio = 2, upper_shadow_ratio = 1) {
  body <- abs(close - open)
  lower_shadow <- pmin(open, close) - low
  upper_shadow <- high - pmax(open, close)
  
  return(lower_shadow > lower_shadow_ratio * body & upper_shadow < upper_shadow_ratio * body)
}

# ========== 🟢 识别锤头线 ==========
lower_shadow_param <- 1.5   # 可调参数
upper_shadow_param <- 0.5   # 可调参数

ohlc$Hammer <- mapply(
  isHammer,
  open = as.numeric(ohlc$Open),
  high = as.numeric(ohlc$High),
  low = as.numeric(ohlc$Low),
  close = as.numeric(ohlc$Close),
  MoreArgs = list(lower_shadow_ratio = lower_shadow_param, upper_shadow_ratio = upper_shadow_param)
)


# ========== 💡 生成交易信号（修正版） ==========
# 步骤1: 将布林带和锤头线计算向前平移1日（避免未来数据）
ohlc$lag_Close <- lag(Cl(ohlc), 1)          # 前一日收盘价
ohlc$lag_mavg <- lag(ohlc$mavg, 1)          # 前一日布林带中轨
ohlc$lag_Hammer <- lag(ohlc$Hammer, 1)      # 前一日锤头线信号

# 步骤2: 基于前一日数据生成当日交易信号
ohlc$Signal <- NA
ohlc$Signal[which(ohlc$lag_Hammer == 1 & ohlc$lag_Close < ohlc$lag_mavg)] <- "Buy"

# 步骤3: 以当日开盘价买入，当日收盘价卖出
trade_dates <- index(ohlc)[which(ohlc$Signal == "Buy")]
buy_price <- as.numeric(Op(ohlc)[trade_dates])    # 当日开盘价
sell_price <- as.numeric(Cl(ohlc)[trade_dates])   # 当日收盘价
profit_points <- sell_price - buy_price


strategy_df <- data.frame(
  Date = trade_dates,
  Buy_Price = round(buy_price, 2),
  Sell_Price = round(sell_price, 2),
  Profit = round(profit_points, 2)
)

daily_df <- data.frame(
  Date = index(ohlc),
  Buy_Price = round(as.numeric(Op(ohlc)), 2),
  Sell_Price = round(as.numeric(Cl(ohlc)), 2)
)
daily_df$Profit <- round(daily_df$Sell_Price - daily_df$Buy_Price, 2)



# ========== 📊 收益总结 ==========
cat("===== 策略一：锤头线策略 =====\n")
cat("交易次数：", nrow(strategy_df), "\n")
cat("累计点数收益：", sum(strategy_df$Profit), "\n")
cat("平均单笔收益：", round(mean(strategy_df$Profit), 2), "\n")
cat("胜率：", round(mean(strategy_df$Profit > 0) * 100, 2), "%\n\n")


strategy_df$Cumulative_Profit <- cumsum(strategy_df$Profit)
daily_df$Cumulative_Profit <- cumsum(daily_df$Profit)

compare_df <- data.frame(
  Date = daily_df$Date,
  Daily_Strategy = daily_df$Cumulative_Profit
)

strategy_cum <- data.frame(
  Date = strategy_df$Date,
  Hammer_Strategy = strategy_df$Cumulative_Profit
)

compare_df <- left_join(compare_df, strategy_cum, by = "Date")

ggplot(compare_df, aes(x = Date)) +
  geom_point(aes(y = Hammer_Strategy, color = "锤头线策略"), size = 1) +
  labs(title = "策略累计收益对比", x = "日期", y = "累计点数收益") +
  scale_color_manual(values = c("日内持仓策略" = "blue", "锤头线策略" = "red")) +
  theme_minimal()



C# ========== 🎲 完整Bootstrap模拟 ==========

# 提取原始数据
Closes <- as.numeric(Cl(ohlc))
log_returns <- diff(log(Closes))[-1] 
n_days <- length(Closes)
initial_price <- 16658  # 初始价格（使用第1天收盘价）

# 检查原始数据时间索引
if (any(is.na(index(ohlc)))) {
  stop("原始数据时间索引包含NA，请处理缺失日期。")
}

# 定义单次模拟函数
bootstrap_simulation <- function(data, init_price) {
  # 生成随机对数收益率序列（重采样）
  sim_log_returns <- sample(data, size = length(data), replace = TRUE)
  
  # ========== 生成价格序列 ==========
  # 初始价格：使用传入的init_price（第1天收盘价）
  cumulative_returns <- cumsum(sim_log_returns)
  sim_prices <- c(init_price, init_price * exp(cumulative_returns))  # 长度n_days
  
  # ========== 构建模拟OHLC数据 ==========
  # 开盘价逻辑：第t日开盘价 = 第t-1日收盘价
  Open_prices <- sim_prices[1:(n_days-1)]  # 前n_days-1个价格作为开盘价（第2~n_days日）
  
  # 生成波动（假设波动率为1%）
  volatility_scale <- 0.01
  random_high <- 1 + abs(rnorm(n_days-1, 0, volatility_scale))
  random_low <- 1 - abs(rnorm(n_days-1, 0, volatility_scale))
  
  # 构建OHLC（从第2天到第n_days天）
  sim_ohlc <- xts(
    cbind(
      Open = Open_prices,                     # 开盘价 = 前一日收盘价
      High = sim_prices[2:n_days] * random_high,  # 当日最高价
      Low = sim_prices[2:n_days] * random_low,    # 当日最低价
      Close = sim_prices[2:n_days],               # 当日收盘价
      Volume = 0                                 # 成交量设为0（可忽略）
    ),
    order.by = index(ohlc)[2:n_days]  # 时间索引对齐原始数据
  )
  
  # ========== 技术指标计算（避免未来数据） ==========
  # 计算布林带（滞后1日）
  bb_sim <- BBands(Cl(sim_ohlc), n = 20)
  sim_ohlc$mavg <- lag(bb_sim$mavg, 1)  # 使用前一日布林带中轨
  
  # 识别锤头线（基于前一日数据）
  sim_ohlc$Hammer <- mapply(
    isHammer,
    open = lag(Op(sim_ohlc), 1),  # 前一日开盘价
    high = lag(Hi(sim_ohlc), 1),  # 前一日最高价
    low = lag(Lo(sim_ohlc), 1),   # 前一日最低价
    close = lag(Cl(sim_ohlc), 1), # 前一日收盘价
    MoreArgs = list(
      lower_shadow_ratio = lower_shadow_param, 
      upper_shadow_ratio = upper_shadow_param
    )
  )
  
  # ========== 交易信号生成 ==========
  sim_ohlc$Signal <- NA
  buy_condition <- which(
    lag(sim_ohlc$Hammer, 1) == 1 &   # 前一日出现锤头线
      lag(Cl(sim_ohlc), 1) < lag(sim_ohlc$mavg, 1)  # 前一日收盘价低于布林带中轨
  )
  
  # ========== 收益计算（当日开盘买入，当日收盘卖出） ==========
  if (length(buy_condition) > 0) {
    buy_price_sim <- as.numeric(Op(sim_ohlc)[buy_condition])  # 当日开盘价
    sell_price_sim <- as.numeric(Cl(sim_ohlc)[buy_condition]) # 当日收盘价
    sum(sell_price_sim - buy_price_sim, na.rm = TRUE)
  } else {
    0
  }
}

# 运行模拟
set.seed(123)
n_sim <- 500
sim_results <- replicate(n_sim, bootstrap_simulation(log_returns, initial_price))

# ========== 📊 显著性检验 ==========
actual_profit <- sum(strategy_df$Profit)
p_value <- mean(sim_results >= actual_profit)

# 绘制收益分布图
library(ggplot2)
ggplot(data.frame(Profit = sim_results), aes(x = Profit)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  geom_vline(xintercept = actual_profit, color = "red", linetype = "dashed") +
  annotate("text", x = actual_profit, y = 50, 
           label = paste0("实际收益: ", round(actual_profit, 2)), 
           color = "red", hjust = -0.1) +
  labs(title = "Bootstrap模拟收益分布",
       subtitle = paste("p-value =", round(p_value, 4)),
       x = "累计收益点数", y = "频数") +
  theme_minimal()

# ========== 📝 输出结果 ==========
cat("\n===== Bootstrap分析结果 =====\n")
cat("实际策略累计收益:", actual_profit, "\n")
cat("模拟收益中位数:", median(sim_results), "\n")
cat("模拟收益超过实际的次数:", sum(sim_results >= actual_profit), "\n")
cat("统计显著性(p-value):", p_value, "\n")