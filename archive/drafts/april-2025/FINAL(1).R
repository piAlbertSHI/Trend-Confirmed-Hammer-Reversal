library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(ggplot2)
library(dplyr)
library(boot)

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






# ========== 🎲 随机游走策略复现 ==========
set.seed(123)
sim_prices <- cumsum(rnorm(n = nrow(ohlc), mean = 0, sd = sd(Cl(ohlc))))
n_sim <- 500
sim_results_rw <- replicate(n_sim, {
  # 生成随机游走价格序列
  sim_ohlc <- xts(
    cbind(
      Open = c(as.numeric(Cl(ohlc)[1]), sim_prices[-length(sim_prices)]),
      High = sim_prices + abs(rnorm(nrow(ohlc), 0, 0.01)),
      Low = sim_prices - abs(rnorm(nrow(ohlc), 0, 0.01)),
      Close = sim_prices,
      Volume = rep(0, nrow(ohlc))  # 确保Volume列长度一致
    ),
    order.by = index(ohlc)  # 直接使用原始数据的时间索引
  )
  
  # 重新计算技术指标和信号
  bb_sim <- BBands(Cl(sim_ohlc), n = 20)
  sim_ohlc$mavg <- bb_sim$mavg
  sim_ohlc$Hammer <- mapply(
    isHammer,
    open = as.numeric(sim_ohlc$Open),
    high = as.numeric(sim_ohlc$High),
    low = as.numeric(sim_ohlc$Low),
    close = as.numeric(sim_ohlc$Close),
    MoreArgs = list(lower_shadow_ratio = 2, upper_shadow_ratio = 1)
  )
  
  # 生成交易信号并计算收益
  sim_ohlc$Signal <- NA
  sim_ohlc$Signal[which(sim_ohlc$Hammer == 1 & sim_ohlc$Close < sim_ohlc$mavg)] <- "Buy"
  trade_dates_sim <- index(sim_ohlc)[which(sim_ohlc$Signal == "Buy") + 1]
  if (length(trade_dates_sim) > 0) {
    buy_price_sim <- as.numeric(Op(sim_ohlc)[trade_dates_sim])
    sell_price_sim <- as.numeric(Cl(sim_ohlc)[trade_dates_sim])
    sum(sell_price_sim - buy_price_sim, na.rm = TRUE)
  } else {
    0
  }
})

# ========== 📊 显著性检验 ==========
actual_profit <- sum(strategy_df$Profit)
p_value_rw <- mean(sim_results_rw >= actual_profit)
# ========== 📝 Output Results ==========
cat("\n===== Bootstrap Simulation Analysis Results =====\n")
cat("Actual Strategy Cumulative Profit:", actual_profit, "\n")
cat("Median Simulation Profit:", median(sim_results_rw), "\n")
cat("Number of Simulations Exceeding Actual Profit:", sum(sim_results_rw >= actual_profit), "\n")
cat("Statistical Significance (p-value):", p_value_rw, "\n")

# ========== 📈 Update Visualization ==========
ggplot(data.frame(Profit = sim_results_rw), aes(x = Profit)) +
  geom_histogram(aes(y = ..density..), binwidth = 100, fill = "skyblue", color = "black") +
  geom_density(color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = actual_profit, color = "red", linetype = "dashed") +
  annotate("text", x = actual_profit, y = 0.0005, 
           label = paste0("Actual Profit: ", round(actual_profit, 2)), 
           color = "red", hjust = -0.1, size = 4) +
  labs(title = "Bootstrap Simulated Profit Distribution with Density",
       subtitle = paste("p-value =", round(p_value_rw, 4)),
       x = "Cumulative Profit Points", y = "Density") +
  theme_minimal()

ggplot(data.frame(Profit = sim_results_rw), aes(x = Profit)) +
  stat_ecdf(geom = "step", color = "blue", size = 1) +
  geom_vline(xintercept = actual_profit, color = "red", linetype = "dashed") +
  annotate("text", x = actual_profit, y = 0.5, 
           label = paste0("Actual Profit: ", round(actual_profit, 2)), 
           color = "red", hjust = -0.1, size = 4) +
  labs(title = "Cumulative Distribution of Bootstrap Simulated Profits)",
       subtitle = paste("p-value =", round(p_value_rw, 4)),
       x = "Cumulative Profit Points", y = "Cumulative Probability") +
  theme_minimal()





# ========== 提取收益率 ==========
Closes <- as.numeric(Cl(ohlc))
log_returns <- diff(log(Closes))[-1]
initial_price <- as.numeric(Cl(ohlc)[1])

# ========== 🎲 AR(1) 模型模拟与策略复现 ==========
ar1_simulation <- function(mu, phi, sigma, initial_price, length_out) {
  # 生成AR(1)收益率序列
  sim_log_returns <- numeric(length_out)
  sim_log_returns[1] <- rnorm(1, mean = mu / (1 - phi), sd = sigma)
  for (i in 2:length_out) {
    sim_log_returns[i] <- mu + phi * sim_log_returns[i - 1] + rnorm(1, mean = 0, sd = sigma)
  }
  
  # 转换为价格序列
  cumulative_returns <- cumsum(sim_log_returns)
  sim_prices <- initial_price * exp(cumulative_returns)
  
  # 构建模拟OHLC数据（需生成合理的高、低价）
  sim_ohlc <- xts(
    cbind(
      Open = c(initial_price, sim_prices[-length(sim_prices)]),  # 开盘价取前一日收盘价
      High = sim_prices * (1 + abs(rnorm(length(sim_prices), 0, 0.01))),  # 添加随机波动
      Low = sim_prices * (1 - abs(rnorm(length(sim_prices), 0, 0.01))),
      Close = sim_prices,
      Volume = 0
    ),
    order.by = index(ohlc)[1:length(sim_prices)]  # 保持时间索引一致
  )
  
  # 重新计算技术指标
  bb_sim <- BBands(Cl(sim_ohlc), n = 20)
  sim_ohlc$mavg <- bb_sim$mavg
  
  # 检测锤子形态
  sim_ohlc$Hammer <- mapply(
    isHammer,
    open = as.numeric(sim_ohlc$Open),
    high = as.numeric(sim_ohlc$High),
    low = as.numeric(sim_ohlc$Low),
    close = as.numeric(sim_ohlc$Close),
    MoreArgs = list(lower_shadow_ratio = 2, upper_shadow_ratio = 1)
  )
  
  # 生成交易信号
  sim_ohlc$Signal <- NA
  sim_ohlc$Signal[which(sim_ohlc$Hammer == 1 & sim_ohlc$Close < sim_ohlc$mavg)] <- "Buy"
  
  # 计算策略收益
  trade_dates_sim <- index(sim_ohlc)[which(sim_ohlc$Signal == "Buy") + 1]
  if (length(trade_dates_sim) > 0) {
    buy_price_sim <- as.numeric(Op(sim_ohlc)[trade_dates_sim])
    sell_price_sim <- as.numeric(Cl(sim_ohlc)[trade_dates_sim])
    sum(sell_price_sim - buy_price_sim, na.rm = TRUE)
  } else {
    0
  }
}

ar_model <- arima(log_returns, order = c(1, 0, 0))

# 提取参数（根据实际系数名称调整）
phi <- coef(ar_model)["ar1"]
sigma <- sqrt(ar_model$sigma2)
mu <- ifelse("const" %in% names(coef(ar_model)), 
             coef(ar_model)["const"], 
             0)

# 运行模拟
set.seed(123)
n_sim <- 500
sim_results_ar1 <- replicate(n_sim, ar1_simulation(mu, phi, sigma, initial_price, length(log_returns)))

# ========== 📊 显著性检验 ==========
actual_profit <- sum(strategy_df$Profit)
p_value_ar1 <- mean(sim_results_ar1 >= actual_profit)

# ========== 📝 输出结果 ==========
cat("\n===== AR(1) 模型策略复现结果 =====\n")
cat("实际策略累计收益:", actual_profit, "\n")
cat("AR(1) 模拟收益中位数:", median(sim_results_ar1), "\n")
cat("超越实际收益的模拟次数:", sum(sim_results_ar1 >= actual_profit), "\n")
cat("统计显著性 (p值):", p_value_ar1, "\n")
# ========== 数据可视化 ==========
# AR(1) 系数自举分布
ggplot(data.frame(Coefficient = bootstrap_results$t), aes(x = Coefficient)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1) +
  geom_vline(xintercept = phi, color = "red", linetype = "dashed") +
  labs(title = "Bootstrap Distribution of AR(1) Coefficient",
       x = "AR(1) Coefficient", y = "Density") +
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
  annotate("text", x = actual_profit, y = 0.0001, 
           label = paste0("Actual Profit: ", round(actual_profit, 2)), 
           color = "red", hjust = -0.2, size = 4) +
  labs(title = "AR(1) Simulated Profit Distribution with Density",
       subtitle = paste("p-value =", round(p_value_ar1, 4)),
       x = "Cumulative Profit Points", y = "Density") +
  theme_minimal()






# ========== 📊 GARCH-M 模型参数估计 ==========
library(rugarch)

log_returns_clean <- na.omit(log_returns)
log_returns_clean <- log_returns_clean[is.finite(log_returns_clean)]
# 查看参数估计
cat("alpha1 + beta1 =", coef(garchm_fit)["alpha1"] + coef(garchm_fit)["beta1"], "\n")

# 若参数接近边界，手动设置初始值
garchm_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE, archm = TRUE),
  distribution.model = "norm",
  start.pars = list(omega = 0.01, alpha1 = 0.1, beta1 = 0.8)  # 设置合理初始值
)
garchm_fit <- ugarchfit(garchm_spec, data = log_returns_clean)

boot_func <- function(data, indices) {
  fit <- ugarchfit(garchm_spec, data = data[indices])
  return(coef(fit))
}

# 运行Bootstrap（R=500次）
set.seed(123)
boot_results <- boot(log_returns_clean, boot_func, R = 500)

# 计算Bootstrap标准误和t值
boot_se <- apply(boot_results$t, 2, sd, na.rm = TRUE)
boot_t <- coef(garchm_fit) / boot_se

# 输出结果
garchm_params <- data.frame(
  Parameter = names(coef(garchm_fit)),
  Estimate = round(coef(garchm_fit), 5),
  Std.Error = round(boot_se, 5),
  t.value = round(boot_t, 2)
)
print(garchm_params)

# ========== 🎲 GARCH-M 模型模拟与策略复现 ==========
set.seed(123)
n_sim <- 10
sim_results_garchm <- replicate(n_sim, {
  # --- 生成GARCH-M收益序列 ---
  sim_returns <- ugarchsim(
    garchm_fit, 
    n.sim = length(log_returns),  # 确保长度与原始数据一致
    custom.dist = list(name = "sample", distfit = sample(residuals(garchm_fit, standardize = TRUE), replace = TRUE))
  )$series[[1]]
  
  # --- 生成价格序列（包含初始价格）---
  sim_prices <- initial_price * exp(c(0, cumsum(sim_returns)))  # 长度 = nrow(ohlc)
  
  # --- 构建模拟OHLC数据 ---
  sim_ohlc <- xts(
    cbind(
      Open = c(initial_price, sim_prices[-length(sim_prices)]),  # 开盘价为前一日收盘价
      High = sim_prices * (1 + abs(rnorm(length(sim_prices), 0, 0.01))),  # 随机生成高、低价
      Low = sim_prices * (1 - abs(rnorm(length(sim_prices), 0, 0.01))),
      Close = sim_prices,
      Volume = 0
    ),
    order.by = index(ohlc)[1:length(sim_prices)]  # 时间索引与原始数据对齐
  )
  
  # --- 重新计算技术指标 ---
  bb_sim <- BBands(Cl(sim_ohlc), n = 20)
  sim_ohlc$mavg <- bb_sim$mavg
  sim_ohlc$Hammer <- mapply(
    isHammer,
    open = as.numeric(sim_ohlc$Open),
    high = as.numeric(sim_ohlc$High),
    low = as.numeric(sim_ohlc$Low),
    close = as.numeric(sim_ohlc$Close),
    MoreArgs = list(lower_shadow_ratio = 2, upper_shadow_ratio = 1)
  )
  
  # --- 生成交易信号并计算收益 ---
  sim_ohlc$Signal <- NA
  sim_ohlc$Signal[which(sim_ohlc$Hammer == 1 & sim_ohlc$Close < sim_ohlc$mavg)] <- "Buy"
  trade_dates_sim <- index(sim_ohlc)[which(sim_ohlc$Signal == "Buy") + 1]
  
  if (length(trade_dates_sim) > 0) {
    buy_price_sim <- as.numeric(Op(sim_ohlc)[trade_dates_sim])
    sell_price_sim <- as.numeric(Cl(sim_ohlc)[trade_dates_sim])
    sum(sell_price_sim - buy_price_sim, na.rm = TRUE)
  } else {
    0
  }
})

# ========== 📊 显著性检验与结果输出 ==========
actual_profit <- sum(strategy_df$Profit)
p_value_garchm <- mean(sim_results_garchm >= actual_profit)

cat("\n===== GARCH-M 模型策略复现结果 =====\n")
cat("实际策略累计收益:", actual_profit, "\n")
cat("GARCH-M模拟收益中位数:", median(sim_results_garchm), "\n")
cat("超越实际收益的模拟次数:", sum(sim_results_garchm >= actual_profit), "\n")
cat("统计显著性 (p值):", p_value_garchm, "\n")

# ========== 📈 可视化模拟收益分布 ==========
ggplot(data.frame(Profit = sim_results_garchm), aes(x = Profit)) +
  geom_histogram(aes(y = ..density..), binwidth = 100, fill = "skyblue", color = "black") +
  geom_density(color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = actual_profit, color = "red", linetype = "dashed") +
  annotate("text", x = actual_profit, y = 0.0005, 
           label = paste0("Actual Profit: ", round(actual_profit, 2)), 
           color = "red", hjust = -0.1, size = 4) +
  labs(title = "GARCH-M Simulated Profit Distribution",
       subtitle = paste("p-value =", round(p_value_garchm, 4)),
       x = "Cumulative Profit Points", y = "Density") +
  theme_minimal()




# ========== 📊 E-GARCH 模型参数估计 ==========
library(rugarch)

# 拟合E-GARCH模型（含非对称效应）
egarch_spec <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),  # 指定eGARCH模型
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "norm"
)
egarch_fit <- ugarchfit(egarch_spec, data = log_returns)

# 提取参数估计结果（解决NaN问题）
egarch_coef <- coef(egarch_fit)
egarch_se <- sqrt(diag(vcov(egarch_fit)))
egarch_se[is.na(egarch_se)] <- 0  # 处理可能的NA值

egarch_params <- data.frame(
  Parameter = c("mu (Mean)", "omega", "alpha1", "beta1", "gamma1 (Asymmetry)"),
  Estimate = round(egarch_coef, 5),
  Std.Error = round(egarch_se, 5),
  t.value = round(egarch_coef / egarch_se, 2)
)
print(egarch_params)

# ========== 🎲 E-GARCH 模型模拟与策略复现 ==========
set.seed(123)
n_sim <- 500
sim_results_egarch <- replicate(n_sim, {
  tryCatch({
    # --- 生成E-GARCH收益序列（长度 = nrow(ohlc) - 1）---
    sim_returns <- ugarchsim(
      egarch_fit, 
      n.sim = nrow(ohlc) - 1,
      custom.dist = list(name = "sample", distfit = sample(residuals(egarch_fit, standardize = TRUE), replace = TRUE))
    )$series[[1]]
    
    # --- 生成价格序列（长度 = nrow(ohlc)）---
    sim_prices <- initial_price * exp(c(0, cumsum(sim_returns)))
    
    # --- 动态生成时间索引 ---
    sim_dates <- seq(
      from = start(index(ohlc)), 
      by = "day", 
      length.out = length(sim_prices)
    )
    
    # --- 构建模拟OHLC数据 ---
    sim_ohlc <- xts(
      cbind(
        Open = c(initial_price, sim_prices[-length(sim_prices)]),
        High = sim_prices * (1 + abs(rnorm(length(sim_prices), 0, 0.01))),
        Low = sim_prices * (1 - abs(rnorm(length(sim_prices), 0, 0.01))),
        Close = sim_prices,
        Volume = 0
      ),
      order.by = sim_dates
    )
    
    # --- 重新计算技术指标 ---
    bb_sim <- BBands(Cl(sim_ohlc), n = 20)
    sim_ohlc$mavg <- bb_sim$mavg
    sim_ohlc$Hammer <- mapply(
      isHammer,
      open = as.numeric(sim_ohlc$Open),
      high = as.numeric(sim_ohlc$High),
      low = as.numeric(sim_ohlc$Low),
      close = as.numeric(sim_ohlc$Close),
      MoreArgs = list(lower_shadow_ratio = 2, upper_shadow_ratio = 1)
    )
    
    # --- 生成交易信号并计算收益 ---
    sim_ohlc$Signal <- NA
    sim_ohlc$Signal[which(sim_ohlc$Hammer == 1 & sim_ohlc$Close < sim_ohlc$mavg)] <- "Buy"
    trade_dates_sim <- index(sim_ohlc)[which(sim_ohlc$Signal == "Buy") + 1]
    
    if (length(trade_dates_sim) > 0) {
      buy_price_sim <- as.numeric(Op(sim_ohlc)[trade_dates_sim])
      sell_price_sim <- as.numeric(Cl(sim_ohlc)[trade_dates_sim])
      sum(sell_price_sim - buy_price_sim, na.rm = TRUE)
    } else {
      0
    }
  }, error = function(e) {
    return(0)  # 跳过模拟中的错误
  })
})

# ========== 📊 显著性检验与结果输出 ==========
actual_profit <- sum(strategy_df$Profit)
p_value_egarch <- mean(sim_results_egarch >= actual_profit)

cat("\n===== E-GARCH 模型策略复现结果 =====\n")
cat("实际策略累计收益:", actual_profit, "\n")
cat("E-GARCH模拟收益中位数:", median(sim_results_egarch, na.rm = TRUE), "\n")
cat("超越实际收益的模拟次数:", sum(sim_results_egarch >= actual_profit, na.rm = TRUE), "\n")
cat("统计显著性 (p值):", p_value_egarch, "\n")

# ========== 📈 可视化模拟收益分布 ==========
ggplot(data.frame(Profit = sim_results_egarch), aes(x = Profit)) +
  geom_histogram(aes(y = ..density..), binwidth = 100, fill = "skyblue", color = "black") +
  geom_density(color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = actual_profit, color = "red", linetype = "dashed") +
  annotate("text", x = actual_profit, y = 0.0005, 
           label = paste0("Actual Profit: ", round(actual_profit, 2)), 
           color = "red", hjust = -0.1, size = 4) +
  labs(title = "E-GARCH Simulated Profit Distribution",
       subtitle = paste("p-value =", round(p_value_egarch, 4)),
       x = "Cumulative Profit Points", y = "Density") +
  theme_minimal()