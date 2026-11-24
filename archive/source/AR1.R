library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(ggplot2)
library(dplyr)
library(boot)
library(kableExtra)

rm(list = ls())

# ========== 📥 Read CSV File ==========
column_names <- read.csv("HSI_25years.csv", skip = 2, nrows = 1, header = FALSE)
col_names <- as.character(unlist(column_names))

data <- read.csv("HSI_25years.csv", skip = 3, header = FALSE, col.names = col_names)
colnames(data) <- c("Datetime", "Open", "High", "Low", "Close", "Volume")

# ========== 🧹 Data Cleaning ==========
data$Datetime <- as.POSIXct(data$Datetime, format = "%Y-%m-%d")
cols <- c("Open", "High", "Low", "Close", "Volume")
data[cols] <- lapply(data[cols], function(x) as.numeric(as.character(x)))

# ========== 🔁 Build XTS Object ==========
ohlc <- xts(data[, cols], order.by = data$Datetime)

# ========== 📊 Calculate Bollinger Bands ==========
bb <- BBands(Cl(ohlc), n = 20)
ohlc$mavg <- bb$mavg

# ========== 🔍 Enhanced Hammer Pattern Detection Function ==========
isHammer <- function(open, high, low, close, lower_shadow_ratio = 2, upper_shadow_ratio = 1) {
  body <- abs(close - open)
  lower_shadow <- pmin(open, close) - low
  upper_shadow <- high - pmax(open, close)
  
  return(lower_shadow > lower_shadow_ratio * body & upper_shadow < upper_shadow_ratio * body)
}

# ========== 🟢 Identify Hammer Patterns ==========
lower_shadow_param <- 2   # Adjustable parameter
upper_shadow_param <- 1   # Adjustable parameter

ohlc$Hammer <- mapply(
  isHammer,
  open = as.numeric(ohlc$Open),
  high = as.numeric(ohlc$High),
  low = as.numeric(ohlc$Low),
  close = as.numeric(ohlc$Close),
  MoreArgs = list(lower_shadow_ratio = lower_shadow_param, upper_shadow_ratio = upper_shadow_param)
)

# ========== 💡 Generate Trading Signals ==========
ohlc$Signal <- NA
ohlc$Signal[which(ohlc$Hammer == 1 & ohlc$Close < ohlc$mavg)] <- "Buy"

upper_shadow <- as.numeric(ohlc$High) - 
  pmax(as.numeric(ohlc$Open), as.numeric(ohlc$Close))

ohlc$Signal[which(
  ohlc$Close > ohlc$mavg &
    upper_shadow > 2 * abs(as.numeric(ohlc$Close) - as.numeric(ohlc$Open))
)] <- "Sell"

# ========== 📌 Build Trade Records ==========
# Adjust trade logic: Execute trades on the day after a signal is generated
trade_dates <- index(ohlc)[which(ohlc$Signal == "Buy") + 1]
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

# ========== 📊 Profit Summary ==========
cat("===== Strategy 1: Hammer Pattern Strategy =====\n")
cat("Number of Trades:", nrow(strategy_df), "\n")
cat("Cumulative Profit Points:", sum(strategy_df$Profit), "\n")
cat("Average Profit per Trade:", round(mean(strategy_df$Profit), 2), "\n")
cat("Win Rate:", round(mean(strategy_df$Profit > 0) * 100, 2), "%\n\n")

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
  geom_point(aes(y = Hammer_Strategy, color = "Hammer Pattern Strategy"), size = 1) +
  labs(title = "Cumulative Profit Comparison of Strategies", x = "Date", y = "Cumulative Profit Points") +
  scale_color_manual(values = c("Daily Holding Strategy" = "blue", "Hammer Pattern Strategy" = "red")) +
  theme_minimal()


# ========== 提取收益率 ==========
Closes <- as.numeric(Cl(ohlc))
log_returns <- diff(log(Closes))[-1]
initial_price <- as.numeric(Cl(ohlc)[1])

# ========== 拟合 AR(1) 模型 ==========
ar_model <- arima(log_returns, order = c(1, 0, 0))
phi <- ar_model$coef["ar1"]
sigma <- sqrt(ar_model$sigma2)
mu <- ifelse("intercept" %in% names(ar_model$coef), ar_model$coef["intercept"], 0)

# ========== 自举法实现 ==========
# 定义自举统计函数：返回 AR(1) 系数
bootstrap_ar1 <- function(data, indices) {
  # 对数据进行自举抽样
  sampled_data <- data[indices]
  # 拟合 AR(1) 模型
  ar_model_boot <- arima(sampled_data, order = c(1, 0, 0))
  # 返回 AR(1) 系数
  return(coef(ar_model_boot)["ar1"])
}

# 执行自举法
set.seed(123)
bootstrap_results <- boot(data = log_returns, statistic = bootstrap_ar1, R = 1000)

# 获取自举法的置信区间
ci_ar1 <- boot.ci(bootstrap_results, type = "perc")

# ========== 模拟 AR(1) 收益序列 ==========
# 定义基于 AR(1) 模型的收益模拟函数
ar1_simulation <- function(mu, phi, sigma, initial_price, length_out) {
  # 模拟收益率序列
  sim_log_returns <- numeric(length_out)
  sim_log_returns[1] <- rnorm(1, mean = mu / (1 - phi), sd = sigma)
  for (i in 2:length_out) {
    sim_log_returns[i] <- mu + phi * sim_log_returns[i - 1] + rnorm(1, mean = 0, sd = sigma)
  }
  
  # 计算累计收益并生成价格序列
  cumulative_returns <- cumsum(sim_log_returns)
  sim_prices <- initial_price * exp(cumulative_returns)
  
  # 返回模拟的价格序列
  return(sim_prices)
}

# 执行 500 次模拟
set.seed(123)
n_sim <- 500
sim_results_ar1 <- replicate(n_sim, {
  sim_prices <- ar1_simulation(mu, phi, sigma, initial_price, length(log_returns))
  # 计算模拟收益
  sum(diff(sim_prices))
})

# ========== 统计显著性检验 ==========
actual_profit <- sum(strategy_df$Profit)
p_value_ar1 <- mean(sim_results_ar1 >= actual_profit)

# ========== 打印结果 ==========
cat("\n===== AR(1) 模型结果 =====\n")
cat("实际策略累计收益:", actual_profit, "\n")
cat("AR(1) 系数估计值:", phi, "\n")
cat("AR(1) 系数自举法均值:", mean(bootstrap_results$t), "\n")
cat("AR(1) 系数 95% 置信区间 (百分位法):\n")
cat("下限:", ci_ar1$percent[4], "上限:", ci_ar1$percent[5], "\n")
cat("p 值（实际收益 vs 模拟收益）:", p_value_ar1, "\n")

# ========== 数据可视化 ==========
# AR(1) 系数自举分布
ggplot(data.frame(Coefficient = bootstrap_results$t), aes(x = Coefficient)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1) +
  geom_vline(xintercept = phi, color = "red", linetype = "dashed") +
  labs(title = "Bootstrap Distribution of AR(1) Coefficient",
       x = "AR(1) Coefficient", y = "Density") +
  theme_minimal()

# 模拟收益分布
ggplot(data.frame(Profit = sim_results_ar1), aes(x = Profit)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1) +
  geom_vline(xintercept = actual_profit, color = "red", linetype = "dashed") +
  labs(title = "AR(1) Simulated Profit Distribution with Density",
       subtitle = paste("p-value =", round(p_value_ar1, 4)),
       x = "Cumulative Profit Points", y = "Density") +
  theme_minimal()

# 模拟收益的累积分布函数 (CDF)
ggplot(data.frame(Profit = sim_results_ar1), aes(x = Profit)) +
  stat_ecdf(geom = "step", color = "blue", size = 1) +
  geom_vline(xintercept = actual_profit, color = "red", linetype = "dashed") +
  annotate("text", x = actual_profit, y = 0.5, 
           label = paste0("Actual Profit: ", round(actual_profit, 2)), 
           color = "red", hjust = -0.1, size = 4) +
  labs(title = "Cumulative Distribution of Simulated Profits",
       subtitle = paste("p-value =", round(p_value_ar1, 4)),
       x = "Cumulative Profit Points", y = "Cumulative Probability") +
  theme_minimal()

# Extract AR(1) parameter estimates
ar1_coef <- coef(ar_model)  # Coefficients: intercept (a) and AR(1) coefficient (b)
ar1_se <- sqrt(diag(vcov(ar_model)))  # Standard errors
ar1_t_values <- ar1_coef / ar1_se  # t-values for coefficients

# Create a data frame for the table
ar1_table <- data.frame(
  Parameter = c("a (Intercept)", "b (AR1 Coefficient)"),
  Estimate = round(ar1_coef, 5),
  `t-value` = round(ar1_t_values, 2)
)

# Print table
print(ar1_table)

# Optional: Use kableExtra for a polished table in reports
kable(ar1_table, caption = "Panel A: AR(1) Parameter Estimates", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


ggplot(data.frame(Profit = sim_results_ar1), aes(x = Profit)) +
  geom_histogram(aes(y = ..density..), binwidth = 100, fill = "skyblue", color = "black") +
  geom_density(color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = actual_profit, color = "red", linetype = "dashed") +
  annotate("text", x = actual_profit, y = 0.0005, 
           label = paste0("Actual Profit: ", round(actual_profit, 2)), 
           color = "red", hjust = -0.1, size = 4) +
  labs(title = "AR(1) Simulated Profit Distribution with Density",
       subtitle = paste("p-value =", round(p_value_ar1, 4)),
       x = "Cumulative Profit Points", y = "Density") +
  theme_minimal()
