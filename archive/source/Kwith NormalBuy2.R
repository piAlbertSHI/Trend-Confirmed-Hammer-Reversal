library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(ggplot2)
library(dplyr)

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
lower_shadow_param <- 2
upper_shadow_param <- 1

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
strategy_dates <- index(ohlc)

buy_price <- as.numeric(Op(ohlc))
sell_price <- as.numeric(Cl(ohlc))

adjusted_buy_price <- buy_price
adjusted_sell_price <- sell_price
profit_points <- numeric(length(buy_price))

for (i in 1:length(strategy_dates)) {
  if (!is.na(ohlc$Signal[i]) && ohlc$Signal[i] == "Buy") {
    # 信号日 → 正常交易
    profit_points[i] <- sell_price[i] - buy_price[i]
  } else if (!is.na(ohlc$Signal[i]) && ohlc$Signal[i] == "Sell") {
    # Sell 信号 → 空仓
    profit_points[i] <- 0
    adjusted_buy_price[i] <- NA
    adjusted_sell_price[i] <- NA
  } else {
    # 无信号 → 日内持仓
    profit_points[i] <- sell_price[i] - buy_price[i]
  }
}

strategy_df <- data.frame(
  Date = strategy_dates,
  Buy_Price = round(adjusted_buy_price, 2),
  Sell_Price = round(adjusted_sell_price, 2),
  Profit = round(profit_points, 2)
)

# ========== 🔵 日内策略 ==========
daily_df <- data.frame(
  Date = index(ohlc),
  Buy_Price = round(buy_price, 2),
  Sell_Price = round(sell_price, 2)
)
daily_df$Profit <- round(daily_df$Sell_Price - daily_df$Buy_Price, 2)

# ========== 📊 收益总结 ==========
cat("===== 策略一：锤头线+日内持仓+Sell过滤 =====\n")
cat("交易次数（含空仓日）：", nrow(strategy_df), "\n")
cat("累计点数收益：", sum(strategy_df$Profit), "\n")
cat("平均单笔收益：", round(mean(strategy_df$Profit), 2), "\n")
cat("胜率：", round(mean(strategy_df$Profit > 0) * 100, 2), "%\n\n")

cat("===== 策略二：纯日内持仓策略 =====\n")
cat("交易次数：", nrow(daily_df), "\n")
cat("累计点数收益：", sum(daily_df$Profit), "\n")
cat("平均单笔收益：", round(mean(daily_df$Profit), 2), "\n")
cat("胜率：", round(mean(daily_df$Profit > 0) * 100, 2), "%\n")

# ========== 📈 收益对比图 ==========
strategy_df$Cumulative_Profit <- cumsum(strategy_df$Profit)
daily_df$Cumulative_Profit <- cumsum(daily_df$Profit)

compare_df <- data.frame(
  Date = daily_df$Date,
  Daily_Strategy = daily_df$Cumulative_Profit,
  Mixed_Strategy = strategy_df$Cumulative_Profit
)

ggplot(compare_df, aes(x = Date)) +
  geom_line(aes(y = Daily_Strategy, color = "日内持仓策略")) +
  geom_line(aes(y = Mixed_Strategy, color = "锤头线+日内持仓+Sell过滤")) +
  labs(title = "策略累计收益对比", x = "日期", y = "累计点数收益") +
  scale_color_manual(values = c("日内持仓策略" = "blue", "锤头线+日内持仓+Sell过滤" = "red")) +
  theme_minimal()
