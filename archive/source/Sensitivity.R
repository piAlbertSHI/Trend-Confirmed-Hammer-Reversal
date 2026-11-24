# ========== 📦 Load Dependencies ==========
library(zoo)
library(xts)
library(TTR)
library(quantmod)
library(ggplot2)
library(dplyr)

# ========== 📥 Load and Build ohlc2 ==========
column_names <- read.csv("HSI_25years.csv", skip = 2, nrows = 1, header = FALSE)
col_names <- as.character(unlist(column_names))
raw_data <- read.csv("HSI_25years.csv", skip = 3, header = FALSE, col.names = col_names)
colnames(raw_data) <- c("Datetime", "Open", "High", "Low", "Close", "Volume")
raw_data$Datetime <- as.POSIXct(raw_data$Datetime, format = "%Y-%m-%d")

ohlc2 <- xts(
  data.frame(
    Open = as.numeric(as.character(raw_data$Open)),
    High = as.numeric(as.character(raw_data$High)),
    Low = as.numeric(as.character(raw_data$Low)),
    Close = as.numeric(as.character(raw_data$Close)),
    Volume = as.numeric(as.character(raw_data$Volume))
  ),
  order.by = raw_data$Datetime
)

# ========== 📊 Calculate Bollinger Band Midline ==========
bb <- BBands(Cl(ohlc2), n = 20)
ohlc2$mavg <- bb$mavg

# ========== 🔍 Hammer Pattern Detection Function ==========
isHammer <- function(open, high, low, close, lower_shadow_ratio = 2, upper_shadow_ratio = 1) {
  body <- abs(close - open)
  lower_shadow <- pmin(open, close) - low
  upper_shadow <- high - pmax(open, close)
  return(lower_shadow > lower_shadow_ratio * body & upper_shadow < upper_shadow_ratio * body)
}

# ========== 🔬 Parameter Sensitivity Analysis ==========
lower_range <- seq(0.1, 2.2, by = 0.1)
upper_range <- seq(0.1, 2.2, by = 0.1)
sensitivity_results <- data.frame()

for (lower in lower_range) {
  for (upper in upper_range) {
    
    # ✅ Step 1: Extract indices where Close < 20-day moving average
    candidate_indices <- which(ohlc2$Close < ohlc2$mavg)
    
    # ✅ Step 2: Only check hammer patterns on those dates
    hammer_flags <- rep(FALSE, nrow(ohlc2))
    hammer_flags[candidate_indices] <- mapply(
      isHammer,
      open = as.numeric(ohlc2$Open[candidate_indices]),
      high = as.numeric(ohlc2$High[candidate_indices]),
      low = as.numeric(ohlc2$Low[candidate_indices]),
      close = as.numeric(ohlc2$Close[candidate_indices]),
      MoreArgs = list(lower_shadow_ratio = lower, upper_shadow_ratio = upper)
    )
    
    # ✅ Step 3: Generate trading signals
    ohlc2$Signal <- NA
    ohlc2$Signal[which(hammer_flags)] <- "Buy"
    
    # ✅ Step 4: Simulate intraday trading (buy at open, sell at close)
    buy_indices <- which(ohlc2$Signal == "Buy")
    buy_price <- as.numeric(Op(ohlc2)[buy_indices])
    sell_price <- as.numeric(Cl(ohlc2)[buy_indices])
    profit_points <- sell_price - buy_price
    
    # ✅ Step 5: Record performance results
    strategy_perf <- data.frame(
      Lower = lower,
      Upper = upper,
      Trades = length(profit_points),
      Total_Profit = sum(profit_points, na.rm = TRUE),
      Win_Rate = ifelse(length(profit_points) > 0,
                        mean(profit_points > 0, na.rm = TRUE),
                        NA),
      Avg_Profit = ifelse(length(profit_points) > 0,
                          mean(profit_points, na.rm = TRUE),
                          NA)
    )
    
    sensitivity_results <- rbind(sensitivity_results, strategy_perf)
  }
}

# ========== 📈 Visualization Function ==========
plot_heatmap <- function(df, metric, title, fill_label) {
  ggplot(df, aes(x = Lower, y = Upper, fill = .data[[metric]])) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "blue", high = "red", mid = "white",
      midpoint = median(df[[metric]], na.rm = TRUE),
      name = fill_label
    ) +
    labs(
      title = title,
      subtitle = "X-axis: Lower Shadow Ratio, Y-axis: Upper Shadow Ratio",
      x = "Lower Shadow Ratio", y = "Upper Shadow Ratio"
    ) +
    theme_minimal()
}

# ========== 📊 Generate Heatmap Outputs ==========
plot_heatmap(sensitivity_results, "Total_Profit", "Sensitivity Analysis: Total Profit", "Total Profit")
plot_heatmap(sensitivity_results, "Win_Rate", "Sensitivity Analysis: Win Rate", "Win Rate")
plot_heatmap(sensitivity_results, "Avg_Profit", "Sensitivity Analysis: Average Profit", "Average Profit")

# ========== 🧾 Output Best Parameters ==========
top3 <- sensitivity_results %>%
  arrange(desc(Total_Profit)) %>%
  slice(1:3)

cat("📌 Top 3 parameter combinations based on Total Profit:\n")
print(top3)

