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
lower_shadow_param <- 2   # 可调参数
upper_shadow_param <- 1   # 可调参数

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



# ========== 🚀 Bootstrap生成模拟简单收益率 ==========
# 计算简单收益率（价格变化百分比）
simple_returns <- ROC(Cl(ohlc), type = "discrete")[-1]

# Bootstrap重抽样函数
bootstrap_returns <- function(returns, n_days=252, n_sim=1000) {
  sim_returns <- matrix(nrow=n_days, ncol=n_sim)
  for(i in 1:n_sim) {
    sim_returns[,i] <- sample(returns, n_days, replace=TRUE)
  }
  return(sim_returns)
}

# 参数设置
n_sim <- 500   # 模拟次数
n_days <- 252  # 模拟天数
set.seed(123)
sim_returns <- bootstrap_returns(as.vector(simple_returns), n_days, n_sim)

# ========== 📈 随机游走生成价格数据 ==========
generate_prices <- function(initial_price, sim_returns) {
  prices <- numeric(length(sim_returns) + initial_price
                    for(i in 2:length(sim_returns)) {
                      prices[i] <- prices[i-1] * (1 + sim_returns[i])
                    }
                    
                    # 生成OHLC数据（带随机波动）
                    data.frame(
                      Open = c(initial_price, prices[-length(prices)] * runif(length(prices)-1, 0.99, 1.01)),
                      High = prices * runif(length(prices), 1.005, 1.015),
                      Low = prices * runif(length(prices), 0.985, 0.995),
                      Close = prices,
                      Volume = NA
                    ) %>% 
                      mutate(High = pmax(High, Open, Close),
                             Low = pmin(Low, Open, Close))
}

# ========== 🔄 批量模拟策略表现 ==========
initial_price <- as.numeric(last(Cl(ohlc)))
sim_results <- list()

for(i in 1:n_sim) {
  # 生成模拟数据
  sim_data <- generate_prices(initial_price, sim_returns[,i])
  sim_ohlc <- xts(sim_data, order.by=seq(Sys.Date(), by="day", length.out=n_days))
  
  # 计算布林带
  bb_sim <- BBands(Cl(sim_ohlc), n=20)
  sim_ohlc$mavg <- bb_sim$mavg
  
  # 识别锤头线
  sim_ohlc$Hammer <- mapply(isHammer,
                            open=as.numeric(sim_ohlc$Open),
                            high=as.numeric(sim_ohlc$High),
                            low=as.numeric(sim_ohlc$Low),
                            close=as.numeric(sim_ohlc$Close))
  
  # 生成交易信号
  sim_ohlc$Signal <- NA
  sim_ohlc$Signal[which(sim_ohlc$Hammer & Cl(sim_ohlc) < sim_ohlc$mavg)] <- "Buy"
  sim_ohlc$Signal[which(Cl(sim_ohlc) > sim_ohlc$mavg & 
                          (Hi(sim_ohlc) - pmax(Op(sim_ohlc), Cl(sim_ohlc))) > 
                          2*abs(Cl(sim_ohlc)-Op(sim_ohlc)))] <- "Sell"
  
  # 记录交易结果
  trades <- index(sim_ohlc)[which(sim_ohlc$Signal == "Buy")]
  if(length(trades)>0) {
    valid_trades <- trades + 1 %in% index(sim_ohlc)
    buy_prices <- as.numeric(Cl(sim_ohlc)[trades[valid_trades]])
    sell_prices <- as.numeric(Op(sim_ohlc)[trades[valid_trades] + 1])
    
    sim_results[[i]] <- data.frame(
      SimID = i,
      Trades = sum(valid_trades),
      TotalProfit = sum(sell_prices - buy_prices),
      WinRate = mean((sell_prices - buy_prices) > 0)
    )
  }
}

# ========== 📊 模拟结果分析 ==========
results_df <- do.call(rbind, sim_results)

cat("===== 蒙特卡洛模拟结果（n=", n_sim, "）=====\n")
cat("平均交易次数:", round(mean(results_df$Trades), 1), "\n")
cat("平均总收益:", round(mean(results_df$TotalProfit), 1), "点\n")
cat("收益标准差:", round(sd(results_df$TotalProfit), 1), "点\n")
cat("平均胜率:", round(mean(results_df$WinRate)*100, 1), "%\n")

# 收益分布直方图
ggplot(results_df, aes(x=TotalProfit)) +
  geom_histogram(binwidth=50, fill="#69b3a2", color="white") +
  geom_vline(xintercept=0, linetype="dashed", color="red") +
  labs(title="策略收益分布", x="累计收益点数", y="频次")

# ========== 📝 增强的价格生成逻辑 ==========
generate_prices_advanced <- function(initial_price, sim_returns) {
  prices <- initial_price
  for(i in 2:length(sim_returns)) {
    prices[i] <- prices[i-1] * (1 + sim_returns[i])
  }
  
  # 生成更真实的OHLC
  data.frame(
    Open = c(initial_price, prices[-length(prices)] * runif(length(prices)-1, 0.99, 1.01)),
    High = prices * runif(length(prices), 1.01, 1.03),
    Low = prices * runif(length(prices), 0.97, 0.99),
    Close = prices
  ) %>% 
    mutate(High = pmax(High, Open, Close),
           Low = pmin(Low, Open, Close))
}

# ========== 🔒 风险控制模块 ==========
apply_stoploss <- function(sim_ohlc, stoploss_pct=0.02) {
  buys <- which(sim_ohlc$Signal == "Buy")
  for(b in buys) {
    if(b+1 > nrow(sim_ohlc)) next  # 防止越界
    
    entry_price <- as.numeric(Cl(sim_ohlc)[b])
    stoploss_price <- entry_price * (1 - stoploss_pct)
    actual_exit <- min(as.numeric(Op(sim_ohlc)[b+1]), stoploss_price)
    
    sim_ohlc$Profit[b] <- actual_exit - entry_price
  }
  return(sim_ohlc)
}