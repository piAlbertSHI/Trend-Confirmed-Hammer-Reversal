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

# ========== 💡 生成交易信号 ==========
ohlc$Signal <- NA
ohlc$Signal[which(ohlc$Hammer == 1 & ohlc$Close < ohlc$mavg)] <- "Buy"

upper_shadow <- as.numeric(ohlc$High) - 
  pmax(as.numeric(ohlc$Open), as.numeric(ohlc$Close))

ohlc$Signal[which(
  ohlc$Close > ohlc$mavg &
    upper_shadow > 2 * abs(as.numeric(ohlc$Close) - as.numeric(ohlc$Open))
)] <- "Sell"

# ========== 📌 构建交易记录 ==========
trade_dates <- index(ohlc)[which(ohlc$Signal == "Buy")]
buy_price <- as.numeric(Op(ohlc)[trade_dates])
sell_price <- as.numeric(Cl(ohlc)[trade_dates])
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


# ========== 🎲 GARCH-M 模型模拟收益序列 ==========

# 加载必要库
library(rugarch)

# 提取原始对数收益率
Closes <- as.numeric(Cl(ohlc))
log_returns <- diff(log(Closes))[-1]  # 使用对数收益率
initial_price <- as.numeric(Cl(ohlc)[1])  # 初始价格

# 定义 GARCH-M 模型
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE, archm = TRUE, archpow = 2),
  distribution.model = "norm"
)

# 拟合 GARCH-M 模型
garch_fit <- ugarchfit(spec = garch_spec, data = log_returns)

# 定义基于 GARCH-M 的收益模拟函数
garchm_simulation <- function(garch_fit, initial_price, length_out) {
  # 模拟对数收益率序列
  garch_sim <- ugarchsim(garch_fit, n.sim = length_out)
  sim_log_returns <- fitted(garch_sim)  # 提取模拟对数收益率
  
  # 计算累积收益率并生成价格序列
  cumulative_returns <- cumsum(sim_log_returns)
  sim_prices <- initial_price * exp(cumulative_returns)
  
  # 构建模拟 OHLC 数据
  sim_ohlc <- xts(
    cbind(
      Open = c(initial_price, sim_prices[-length(sim_prices)]),
      High = sim_prices * (1 + abs(rnorm(length(sim_prices), 0, 0.01))),  # 模拟波动
      Low = sim_prices * (1 - abs(rnorm(length(sim_prices), 0, 0.01))),
      Close = sim_prices,
      Volume = 0
    ),
    order.by = index(ohlc)[1:length_out]
  )
  
  # 重新计算技术指标（使用模拟收盘价）
  bb_sim <- BBands(Cl(sim_ohlc), n = 20)
  sim_ohlc$mavg <- bb_sim$mavg
  
  # 识别锤头线（使用模拟价格）
  sim_ohlc$Hammer <- mapply(
    isHammer,
    open = as.numeric(sim_ohlc$Open),
    high = as.numeric(sim_ohlc$High),
    low = as.numeric(sim_ohlc$Low),
    close = as.numeric(sim_ohlc$Close),
    MoreArgs = list(lower_shadow_ratio = lower_shadow_param, 
                    upper_shadow_ratio = upper_shadow_param)
  )
  
  # 生成交易信号
  sim_ohlc$Signal <- NA
  sim_ohlc$Signal[which(sim_ohlc$Hammer == 1 & 
                          sim_ohlc$Close < sim_ohlc$mavg)] <- "Buy"
  
  upper_shadow_sim <- as.numeric(sim_ohlc$High) - 
    pmax(as.numeric(sim_ohlc$Open), as.numeric(sim_ohlc$Close))
  
  sim_ohlc$Signal[which(
    sim_ohlc$Close > sim_ohlc$mavg & 
      upper_shadow_sim > 2 * abs(as.numeric(sim_ohlc$Close) - 
                                   as.numeric(sim_ohlc$Open))
  )] <- "Sell"
  
  # 计算模拟收益
  trade_dates_sim <- index(sim_ohlc)[which(sim_ohlc$Signal == "Buy")]
  if (length(trade_dates_sim) > 0) {
    buy_price_sim <- as.numeric(Op(sim_ohlc)[trade_dates_sim])
    sell_price_sim <- as.numeric(Cl(sim_ohlc)[trade_dates_sim])
    sum(sell_price_sim - buy_price_sim, na.rm = TRUE)
  } else {
    0
  }
}

# 运行 500 次 GARCH-M 模拟
set.seed(123)
n_sim <- 500
sim_results_garchm <- replicate(n_sim, garchm_simulation(garch_fit, initial_price, length(log_returns)))

# ========== 📊 显著性检验 ==========

# 计算实际策略累计收益
actual_profit <- sum(strategy_df$Profit)

# 计算 GARCH-M 模拟的 p-value
p_value_garchm <- mean(sim_results_garchm >= actual_profit)

# 绘制收益分布图
ggplot(data.frame(Profit = sim_results_garchm), aes(x = Profit)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  geom_vline(xintercept = actual_profit, color = "red", linetype = "dashed") +
  annotate("text", x = actual_profit, y = 50, 
           label = paste0("实际收益: ", round(actual_profit, 2)), 
           color = "red", hjust = -0.1) +
  labs(title = "GARCH-M 模拟收益分布",
       subtitle = paste("p-value =", round(p_value_garchm, 4)),
       x = "累计收益点数", y = "频数") +
  theme_minimal()

# ========== 📝 输出结果 ==========

cat("\n===== GARCH-M 模拟分析结果 =====\n")
cat("实际策略累计收益:", actual_profit, "\n")
cat("GARCH-M 模拟收益中位数:", median(sim_results_garchm), "\n")
cat("GARCH-M 模拟收益超过实际的次数:", sum(sim_results_garchm >= actual_profit), "\n")
cat("统计显著性 (p-value):", p_value_garchm, "\n")
