library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(ggplot2)

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

# ========== 🚀 定义上涨阶段 ==========
uptrend <- Cl(ohlc) > bb$mavg  # 只考虑中轨突破

# ========== 📌 生成阳线标记 ==========
is_bullish <- Cl(ohlc) > Op(ohlc)

# ========== 🎯 找到连续5根阳线信号 + 布林带上涨过滤 ==========
signal <- rep(NA, NROW(ohlc))
for (i in 5:(NROW(ohlc)-1)) {
  if (all(is_bullish[(i-4):i]) && uptrend[i]) {
    signal[i+1] <- 1  # 下一根K线开盘买入，收盘卖出
  }
}

# ========== 🟢 构建交易记录 ==========
trade_dates <- index(ohlc)[which(signal == 1)]
buy_price <- Op(ohlc)[trade_dates]
sell_price <- Cl(ohlc)[trade_dates]
profit_points <- as.numeric(sell_price - buy_price)

trade_df <- data.frame(
  Date = trade_dates,
  Buy_Price = round(buy_price, 2),
  Sell_Price = round(sell_price, 2),
  Profit = round(profit_points, 2)
)

# ========== 📈 画出信号点 ==========
signal_plot <- data.frame(
  Date = index(ohlc),
  Close = Cl(ohlc),
  Signal = ifelse(!is.na(signal), signal, 0)
)

ggplot(signal_plot, aes(x = Date, y = Close)) +
  geom_line(color = "gray") +
  geom_point(data = subset(signal_plot, Signal == 1),
             aes(x = Date, y = Close),
             color = "red", size = 1.5) +
  labs(title = "布林带中轨突破 + 连续5阳线买入信号", 
       x = "日期", y = "收盘价") +
  theme_minimal()

# ========== 📊 收益总结 ==========
total_profit <- sum(trade_df$Profit)
cat("总交易次数：", nrow(trade_df), "\n")
cat("累计点数收益：", total_profit, "\n")
cat("平均单笔收益：", round(mean(trade_df$Profit), 2), "\n")
cat("胜率：", round(mean(trade_df$Profit > 0) * 100, 2), "%\n")


