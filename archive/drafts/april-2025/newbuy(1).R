library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(ggplot2)
library(dplyr)
library(boot)
library(quantmod)

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

# ========== 🔵 日内策略：每日开盘买入收盘卖出 ==========
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

cat("===== 策略二：日内持仓策略 =====\n")
cat("交易次数：", nrow(daily_df), "\n")
cat("累计点数收益：", sum(daily_df$Profit), "\n")
cat("平均单笔收益：", round(mean(daily_df$Profit), 2), "\n")
cat("胜率：", round(mean(daily_df$Profit > 0) * 100, 2), "%\n")

# ========== 📈 收益对比图 ==========
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



time_series <- daily_df$Profit  # 替换为包含时间序列数据的列名
time_series <- na.omit(time_series)


# 定义 AR(1) 模型拟合函数
fit_ar1 <- function(data, indices) {
  # 通过样本索引生成 bootstrap 数据
  bootstrap_sample <- data[indices]
  
  # AR(1) 模型拟合
  ar_model <- arima(bootstrap_sample, order = c(1, 0, 0))
  
  # 返回 AR(1) 系数（phi）
  return(ar_model$coef[1])
}

# 设置 bootstrap 参数
set.seed(123)  # 确保结果可重复
bootstrap_results <- boot(
  data = time_series,
  statistic = fit_ar1,
  R = 1000,  # 设置 bootstrap 重采样次数
  parallel = "no",  # 禁用并行化以便调试
  stype = "i"  # 使用索引类型采样
)

# 查看 bootstrap 结果
print(bootstrap_results)

# 计算 AR(1) 系数的置信区间
ci <- boot.ci(bootstrap_results, type = "perc")
print(ci)

# 可视化 bootstrap 分布
hist(bootstrap_results$t, breaks = 30, main = "Bootstrap Distribution of AR(1) Coefficient",
     xlab = "AR(1) Coefficient", col = "skyblue")


# ========== 🔄 AR(1)模型验证模块 ==========
# 使用原始收益率数据拟合AR(1)模型
ohlc <- xts(data[, c("Open", "High", "Low", "Close", "Volume")], order.by = data$Date)
orig_returns <- ROC(Cl(ohlc), type = "continuous")  # 使用对数收益率
orig_returns <- na.omit(orig_returns)  # 去除NA值
ar_model <- arima(orig_returns, order = c(1,0,0))

# 定义模拟函数
simulate_ar1_ohlc <- function(ar_model, n_days = 252*25, start_price = 1000) {
  # 提取模型参数
  phi <- ar_model$coef[1]
  intercept <- ar_model$coef[2]
  sigma <- sqrt(ar_model$sigma2)
  
  # 生成收益率序列
  sim_returns <- numeric(n_days)
  sim_returns[1] <- orig_returns[1]  # 使用初始值
  
  for(i in 2:n_days) {
    sim_returns[i] <- intercept + phi*sim_returns[i-1] + rnorm(1, 0, sigma)
  }
  
  # 转换为价格序列（包含完整的OHLC模拟）
  sim_prices <- cumprod(c(start_price, 1 + sim_returns))
  
  # 生成完整OHLC数据（带随机波动）
  sim_ohlc <- data.frame(
    Datetime = seq.Date(from = as.Date("1997-07-01"), by = "day", length.out = n_days),
    Open = numeric(n_days),
    High = numeric(n_days),
    Low = numeric(n_days),
    Close = numeric(n_days)
  )
  
  # 首日数据
  sim_ohlc[1, c("Open","High","Low","Close")] <- sim_prices[1]
  
  # 生成每日价格波动
  for(i in 2:n_days) {
    base_price <- sim_prices[i]
    volatility <- sd(orig_returns, na.rm = T)  # 使用原始波动率
    
    # 生成当日价格波动
    intraday_range <- runif(1, 0.5, 2)*volatility  # 随机波动幅度
    o <- sim_prices[i-1]
    c <- base_price
    h <- max(o, c) + abs(rnorm(1, intraday_range/2, intraday_range/4))
    l <- min(o, c) - abs(rnorm(1, intraday_range/2, intraday_range/4))
    
    sim_ohlc[i, ] <- list(
      Datetime = sim_ohlc$Datetime[i],
      Open = o,
      High = max(o, h, c),
      Low = min(o, l, c),
      Close = c
    )
  }
  return(sim_ohlc)
}

# ========== 🔁 执行模拟验证 ==========
set.seed(123)
n_simulations <- 100  # 模拟次数
sim_results <- list()

for(sim in 1:n_simulations) {
  # 生成模拟数据
  sim_data <- simulate_ar1_ohlc(ar_model, n_days = nrow(ohlc))
  
  # 使用原有策略处理模拟数据
  sim_ohlc <- xts(sim_data[, -1], order.by = as.Date(sim_data$Datetime))
  
  # 重新计算技术指标
  bb_sim <- BBands(Cl(sim_ohlc), n = 20)
  sim_ohlc$mavg <- bb_sim$mavg
  
  # 应用锤头线识别
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
  sim_ohlc$Signal[which(sim_ohlc$Hammer == 1 & Cl(sim_ohlc) < sim_ohlc$mavg)] <- "Buy"
  
  # 记录交易结果
  trade_dates_sim <- index(sim_ohlc)[which(sim_ohlc$Signal == "Buy")]
  if(length(trade_dates_sim) > 0) {
    sim_results[[sim]] <- data.frame(
      SimID = sim,
      Buy_Date = trade_dates_sim,
      Sell_Date = trade_dates_sim + 1,  # 假设次日卖出
      Profit = as.numeric(Cl(sim_ohlc)[trade_dates_sim + 1]) - 
        as.numeric(Op(sim_ohlc)[trade_dates_sim]))
  }
}

# ========== 📊 分析模拟结果 ==========
all_sim_profits <- do.call(rbind, sim_results)

sim_perf <- all_sim_profits %>%
  group_by(SimID) %>%
  summarise(
    TotalProfit = sum(Profit, na.rm = T),
    WinRate = mean(Profit > 0, na.rm = T),
    AvgProfit = mean(Profit, na.rm = T)
  )

# 计算统计显著性
original_profit <- sum(strategy_df$Profit, na.rm = T)
sim_mean <- mean(sim_perf$TotalProfit)
sim_sd <- sd(sim_perf$TotalProfit)
z_score <- (original_profit - sim_mean) / sim_sd
p_value <- 2*pnorm(-abs(z_score))

# ========== 📈 可视化验证结果 ==========
ggplot(sim_perf, aes(x = TotalProfit)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = original_profit, color = "red", linetype = "dashed") +
  labs(title = paste0("策略收益分布 vs 原始收益 (p = ", round(p_value, 3), ")"),
       x = "累计收益点数", y = "出现次数") +
  annotate("text", x = original_profit, y = Inf, 
           label = "原始策略收益", vjust = 2, color = "red") +
  theme_minimal()

# ========== 📝 输出验证报告 ==========
cat("=== AR(1)模型验证报告 ===\n")
cat("模拟次数：", n_simulations, "\n")
cat("原始策略收益：", original_profit, "\n")
cat("模拟平均收益：", round(sim_mean, 2), "\n")
cat("模拟收益标准差：", round(sim_sd, 2), "\n")
cat("Z值：", round(z_score, 2), "\n")
cat("P值：", format.pval(p_value, digits = 3), "\n\n")

cat("统计结论：\n")
if(p_value < 0.05) {
  cat("原始策略收益显著优于模拟数据表现 (p < 0.05)\n")
  cat("说明策略可能具有真正的预测能力")
} else {
  cat("原始策略收益与模拟数据无显著差异 (p > 0.05)\n")
  cat("策略可能未表现出真正的预测能力")
}

