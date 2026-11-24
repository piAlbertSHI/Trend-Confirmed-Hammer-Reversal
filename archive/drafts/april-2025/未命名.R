# ========== 📦 包加载 ==========
library(rugarch)    # GARCH建模
library(xts)        # 时间序列处理
library(TTR)        # 技术指标计算
library(parallel)   # 并行计算
library(ggplot2)    # 可视化
library(zoo)
library(quantmod)
library(dplyr)

# ========== 📥 读取 CSV ==========
column_names <- read.csv("/Users/mengyujie/Desktop/team2/Financial Analytics/Project/HSI_25years.csv", skip = 2, nrows = 1, header = FALSE)
col_names <- as.character(unlist(column_names))

data <- read.csv("/Users/mengyujie/Desktop/team2/Financial Analytics/Project/HSI_25years.csv", skip = 3, header = FALSE, col.names = col_names)
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


# ========== 🧮 GARCH-M模型设定 ==========
# 计算对数收益率
log_returns <- diff(log(ohlc$Close))[-1]

# 模型设定（包含ARCH-M效应）
garchm_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 1), include.mean = TRUE, archm = TRUE),
  distribution.model = "std"
)

# 模型拟合
garchm_fit <- ugarchfit(garchm_spec, log_returns, solver = "hybrid")

# 参数校验
if(convergence(garchm_fit) != 0) warning("模型未完全收敛，结果需谨慎使用！")
if(coef(garchm_fit)["beta1"] > 0.95) warning("高波动持续性可能导致趋势过强")

# 提取参数和残差
garchm_params <- coef(garchm_fit)
garchm_resids <- residuals(garchm_fit, standardize = TRUE)

# ========== 🔄 改进的GARCH-M模拟函数 ==========
simulate_garchm <- function(resids, params, n_obs) {
  # 参数校验
  required_params <- c("mu", "ma1", "omega", "alpha1", "beta1")
  if(!all(required_params %in% names(params))) stop("缺失必要GARCH参数")
  
  # 初始化存储
  sim_ret <- numeric(n_obs)
  sim_h <- numeric(n_obs)
  sim_eps <- numeric(n_obs)
  
  # 初始条件
  sim_h[1] <- params["omega"] / (1 - params["alpha1"] - params["beta1"])
  sim_eps[1] <- sqrt(sim_h[1]) * sample(resids, 1)
  sim_ret[1] <- params["mu"] + params["ma1"] * sim_eps[1] + sim_eps[1]
  
  # 递归生成序列
  for(t in 2:n_obs) {
    sim_h[t] <- params["omega"] + 
      params["alpha1"] * sim_eps[t-1]^2 + 
      params["beta1"] * sim_h[t-1]
    sim_eps[t] <- sqrt(sim_h[t]) * sample(resids, 1)
    sim_ret[t] <- params["mu"] + params["ma1"] * sim_eps[t-1] + sim_eps[t]
  }
  
  # 转换为价格序列
  exp(cumsum(sim_ret))
}

# ========== 📈 改进的OHLC生成函数 ==========
generate_ohlc <- function(prices) {
  n <- length(prices)
  ohlc_matrix <- matrix(nrow = n, ncol = 4)
  colnames(ohlc_matrix) <- c("Open", "High", "Low", "Close")
  
  # 首日数据
  ohlc_matrix[1, ] <- rep(prices[1], 4)
  
  # 生成每日价格波动
  for(i in 2:n) {
    prev_close <- ohlc_matrix[i-1, 4]
    ret <- log(prices[i]/prices[i-1])
    volatility <- abs(ret) * runif(1, 1.5, 3.0)  # 增大波动范围
    
    open <- prev_close * (1 + rnorm(1, 0, 0.002))  # 添加开盘价微扰
    close <- open * exp(ret)
    high <- open * (1 + runif(1, 0, volatility))
    low <- open * (1 - runif(1, 0, volatility))
    
    # 确保价格合理性
    ohlc_matrix[i, ] <- c(
      open,
      max(high, open, close),
      min(low, open, close),
      close
    )
  }
  ohlc_matrix
}

# ========== 🛠️ 优化的策略计算函数 ==========
is_hammer <- function(open, high, low, close, 
                      lower_ratio = 1.2,  # 放宽下影线比例
                      upper_ratio = 0.6) {  # 放宽上影线比例
  body <- abs(close - open)
  lower_shadow <- pmin(open, close) - low
  upper_shadow <- high - pmax(open, close)
  lower_shadow > lower_ratio * body & upper_shadow < upper_ratio * body
}

# ========== 📈 策略收益计算函数 ==========
calculate_profit <- function(sim_xts) {
  # 信号条件（显式类型转换）
  buy_signals <- which(
    as.logical(sim_xts$Hammer) & 
      as.numeric(sim_xts$Close) < as.numeric(sim_xts$mavg)
  )
  
  sell_signals <- which(
    as.numeric(sim_xts$Close) > as.numeric(sim_xts$mavg) &
      (as.numeric(sim_xts$High) - 
         pmax(as.numeric(sim_xts$Open), as.numeric(sim_xts$Close)) > 
         2 * abs(as.numeric(sim_xts$Close) - as.numeric(sim_xts$Open)))
  )
  
  # 交易配对逻辑
  profit <- 0
  if(length(buy_signals) > 0 && length(sell_signals) > 0) {
    # 创建交易时间表
    trades <- data.frame(
      buy_time = index(sim_xts)[buy_signals],
      sell_time = index(sim_xts)[sell_signals]
    )
    
    # 寻找有效交易对（卖出在买入之后）
    valid_trades <- subset(trades, sell_time > buy_time)
    
    if(nrow(valid_trades) > 0) {
      # 计算每笔交易收益
      profit <- sum(
        as.numeric(Cl(sim_xts[valid_trades$sell_time])) - 
          as.numeric(Op(sim_xts[valid_trades$buy_time]))
      )
    }
  }
  round(profit, 2)
}
# ========== ⚡ 健壮的并行计算配置 ==========
cl <- makeCluster(detectCores() - 1)

# 导出必要对象
clusterExport(cl, c("ohlc", "BBands", "generate_ohlc", "is_hammer",
                    "garchm_resids", "garchm_params",
                    "simulate_garchm", "calculate_profit"),
              envir = environment())

# 加载依赖包
clusterEvalQ(cl, {
  library(xts)
  library(TTR)
})

# ========== 🚀 执行Bootstrap模拟 ==========
n_sim <- 500
sim_results <- parLapply(cl, 1:n_sim, function(i) {
  tryCatch({
    # 生成价格序列
    sim_prices <- simulate_garchm(garchm_resids, garchm_params, nrow(ohlc))
    
    # 生成OHLC数据
    ohlc_matrix <- generate_ohlc(sim_prices)
    sim_xts <- xts(ohlc_matrix, order.by = index(ohlc))
    
    # 调试输出
    message(sprintf("模拟 %d: 生成价格范围 [%.2f, %.2f]", i, min(sim_prices), max(sim_prices)))
    
    # 计算策略收益
    profit <- calculate_profit(sim_xts)
    
    # 额外诊断信息
    if(profit == 0) {
      num_buy <- sum(sim_xts$Hammer)
      num_sell <- sum(sim_xts$Close > sim_xts$mavg & 
                        (sim_xts$High - pmax(sim_xts$Open, sim_xts$Close) > 
                           1.2 * abs(sim_xts$Close - sim_xts$Open)))
      message(sprintf("零收益警告: 买入信号=%d, 卖出信号=%d", num_buy, num_sell))
    }
    
    profit
  }, error = function(e) {
    message(sprintf("模拟 %d 失败: %s", i, conditionMessage(e)))
    return(0)
  })
})

stopCluster(cl)

# ========== 📊 结果分析 ==========
sim_profits <- unlist(sim_results)
sim_profits <- sim_profits[is.finite(sim_profits) & !is.na(sim_profits)]

# 生成实际策略交易记录
ohlc$mavg <- BBands(ohlc[,c("High","Low","Close")], n=20)$mavg
ohlc$Hammer <- mapply(is_hammer, 
                      o=as.numeric(ohlc$Open),
                      h=as.numeric(ohlc$High),
                      l=as.numeric(ohlc$Low),
                      c=as.numeric(ohlc$Close))

buy_signals <- which(ohlc$Hammer & ohlc$Close < ohlc$mavg)
sell_signals <- which(ohlc$Close > ohlc$mavg & 
                        (ohlc$High - pmax(ohlc$Open, ohlc$Close) > 
                           1.2 * abs(ohlc$Close - ohlc$Open)))

strategy_df <- data.frame(
  Date = index(ohlc)[buy_signals],
  Buy_Price = round(as.numeric(Op(ohlc)[buy_signals]), 2),
  Sell_Price = round(as.numeric(Cl(ohlc)[buy_signals + 1]), 2)  # 假设次日卖出
)
strategy_df$Profit <- strategy_df$Sell_Price - strategy_df$Buy_Price

# 计算实际策略收益
actual_profit <- sum(strategy_df$Profit, na.rm = TRUE)

# 安全统计函数
safe_mean <- function(x) if(length(x) > 0) mean(x) else NA
safe_sd <- function(x) if(length(x) > 1) sd(x) else NA
empirical_p <- if(length(sim_profits) > 0) mean(sim_profits >= actual_profit) else NA

# 结果输出
cat("\n===== 最终优化结果 =====\n")
cat("有效模拟次数:", length(sim_profits), "/", n_sim, "\n")
cat("实际策略收益:", round(actual_profit, 2), "\n")
cat("模拟平均收益:", round(safe_mean(sim_profits), 2), "\n")
cat("模拟收益标准差:", round(safe_sd(sim_profits), 2), "\n")
cat("经验p值:", if(is.na(empirical_p)) "N/A" else round(empirical_p, 3), "\n")

# 可视化
if(length(sim_profits) > 10) {
  hist_data <- data.frame(Returns = sim_profits)
  p <- ggplot(hist_data, aes(x = Returns)) +
    geom_histogram(aes(y = ..density..), bins = 30, 
                   fill = "steelblue", alpha = 0.7) +
    geom_vline(xintercept = actual_profit, color = "red", 
               linetype = "dashed", linewidth = 1.2) +
    labs(title = "策略收益分布对比",
         subtitle = paste("实际收益:", round(actual_profit, 2)),
         x = "收益", y = "密度") +
    theme_minimal()
  print(p)
} else {
  plot.new()
  text(0.5, 0.5, "有效模拟数据不足\n无法生成可视化", cex = 1.2, col = "darkred")
}


# ========== 🆕 EGARCH模型设定 ==========
# 设定EGARCH模型
egarch_spec <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 1), include.mean = TRUE),
  distribution.model = "std"
)

# 拟合EGARCH模型
egarch_fit <- ugarchfit(egarch_spec, log_returns, solver = "hybrid")

# 参数校验
if(convergence(egarch_fit) != 0) warning("EGARCH模型未完全收敛！")
if(coef(egarch_fit)["beta1"] > 0.99) warning("检测到强持续性参数")

# 提取EGARCH参数和残差
egarch_params <- coef(egarch_fit)
egarch_resids <- residuals(egarch_fit, standardize = TRUE)

# ========== 🔄 EGARCH模拟函数 ==========
simulate_egarch <- function(resids, params, n_obs) {
  # 参数校验
  required_params <- c("mu", "ma1", "omega", "alpha1", "beta1", "gamma1")
  if(!all(required_params %in% names(params))) stop("缺失EGARCH必要参数")
  
  # 初始化存储
  sim_ret <- numeric(n_obs)
  log_h <- numeric(n_obs)
  sim_eps <- numeric(n_obs)
  
  # 初始条件
  log_h[1] <- params["omega"] / (1 - params["beta1"])
  sim_eps[1] <- sqrt(exp(log_h[1])) * sample(resids, 1)
  sim_ret[1] <- params["mu"] + params["ma1"] * sim_eps[1] + sim_eps[1]
  
  # 递归生成序列
  for(t in 2:n_obs) {
    z <- sim_eps[t-1]/sqrt(exp(log_h[t-1]))
    log_h[t] <- params["omega"] + 
      params["beta1"] * log_h[t-1] +
      params["alpha1"] * (abs(z) - sqrt(2/pi)) +
      params["gamma1"] * z
    
    sim_eps[t] <- sqrt(exp(log_h[t])) * sample(resids, 1)
    sim_ret[t] <- params["mu"] + params["ma1"] * sim_eps[t-1] + sim_eps[t]
  }
  
  # 转换为价格序列
  exp(cumsum(sim_ret))
}

# ========== ⚙️ 更新并行计算配置 ==========
cl <- makeCluster(detectCores() - 1)
clusterExport(cl, c("simulate_egarch", "egarch_params", "egarch_resids"), 
              envir = environment())

# ========== 🚀 EGARCH Bootstrap模拟 ==========
egarch_sim_results <- parLapply(cl, 1:n_sim, function(i){
  tryCatch({
    # 生成EGARCH价格序列
    sim_prices <- simulate_egarch(egarch_resids, egarch_params, nrow(ohlc))
    
    # 生成OHLC数据（复用原有函数）
    ohlc_matrix <- generate_ohlc(sim_prices)
    sim_xts <- xts(ohlc_matrix, order.by = index(ohlc))
    
    # 计算策略收益
    profit <- calculate_profit(sim_xts)
    
    profit
  }, error = function(e) {
    message(sprintf("EGARCH模拟 %d 失败: %s", i, conditionMessage(e)))
    return(0)
  })
})

stopCluster(cl)

# ========== 📊 EGARCH结果分析 ==========
egarch_profits <- unlist(egarch_sim_results)
egarch_profits <- egarch_profits[is.finite(egarch_profits) & !is.na(egarch_profits)]

# 计算经验p值
egarch_p <- mean(egarch_profits >= actual_profit)

# 结果输出
cat("\n===== EGARCH 结果 =====\n")
cat("有效模拟次数:", length(egarch_profits), "/", n_sim, "\n")
cat("模拟平均收益:", round(mean(egarch_profits), 2), "\n")
cat("经验p值:", round(egarch_p, 3), "\n")

# 可视化对比
if(length(egarch_profits) > 10) {
  hist_data <- data.frame(
    Model = factor(rep(c("GARCH-M", "EGARCH"), 
                       each = c(length(sim_profits), length(egarch_profits))),
                   Returns = c(sim_profits, egarch_profits)
    )
    
    p <- ggplot(hist_data, aes(x = Returns, fill = Model)) +
      geom_histogram(aes(y = ..density..), alpha = 0.6, 
                     position = "identity", bins = 30) +
      geom_vline(xintercept = actual_profit, color = "red",
                 linetype = "dashed", linewidth = 1.2) +
      labs(title = "Bootstrap收益分布对比",
           subtitle = paste("实际收益:", round(actual_profit, 2)),
           x = "收益", y = "密度") +
      theme_minimal()
    print(p)
}




