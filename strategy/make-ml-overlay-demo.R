#!/usr/bin/env Rscript

source("R/data.R", encoding = "UTF-8")
source("R/strategy.R", encoding = "UTF-8")
source("strategy/engine.R", encoding = "UTF-8")
source("strategy/evaluate.R", encoding = "UTF-8")
source("strategy/pullback.R", encoding = "UTF-8")
source("strategy/fixed/backtest.R", encoding = "UTF-8")

if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is required.")
if (!requireNamespace("scales", quietly = TRUE)) stop("scales is required.")
if (!requireNamespace("ragg", quietly = TRUE)) stop("ragg is required.")
if (!requireNamespace("svglite", quietly = TRUE)) stop("svglite is required.")

library(ggplot2)
library(scales)

demo_dir <- "strategy/fixed/overlay-demo"
figure_dir <- file.path(demo_dir, "figures")
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

fixed <- fixed_setup()
bars <- fixed$bars
rows <- fixed$rows
position <- read.csv("strategy/fixed/results/tchr_ml_overlay_daily.csv",
                     stringsAsFactors = FALSE)
position$tchr_position <- as.logical(position$tchr_position)
position$ml_overlay_position <- as.logical(position$ml_overlay_position)
stopifnot(nrow(position) == length(rows),
          all(as.Date(position$date) == bars$Date[rows]))

base_state <- logical(nrow(bars))
overlay_state <- logical(nrow(bars))
base_state[rows] <- position$tchr_position
overlay_state[rows] <- position$ml_overlay_position
ret <- return_components(bars$Open, bars$Close)
base_run <- portfolio_log(ret, base_state, rows, trial_fee)
overlay_run <- portfolio_log(ret, overlay_state, rows, trial_fee)
benchmark_run <- portfolio_log(ret, rep(TRUE, nrow(bars)), rows, trial_fee)

wealth <- function(x) exp(cumsum(x$daily[rows]))
drawdown <- function(x) {
  value <- wealth(x)
  value / cummax(value) - 1
}
series <- rbind(
  data.frame(date = bars$Date[rows], panel = "Cumulative net value",
             series = "TCHR", value = wealth(base_run) - 1),
  data.frame(date = bars$Date[rows], panel = "Cumulative net value",
             series = "ML overlay", value = wealth(overlay_run) - 1),
  data.frame(date = bars$Date[rows], panel = "Cumulative net value",
             series = "Buy and hold", value = wealth(benchmark_run) - 1),
  data.frame(date = bars$Date[rows], panel = "Drawdown",
             series = "TCHR", value = drawdown(base_run)),
  data.frame(date = bars$Date[rows], panel = "Drawdown",
             series = "ML overlay", value = drawdown(overlay_run)),
  data.frame(date = bars$Date[rows], panel = "Drawdown",
             series = "Buy and hold", value = drawdown(benchmark_run))
)
series$panel <- factor(series$panel, levels = c("Cumulative net value", "Drawdown"))
series$series <- factor(series$series, levels = c("ML overlay", "TCHR", "Buy and hold"))
series <- series[is.finite(series$value), , drop = FALSE]

equity_plot <- ggplot(series, aes(date, value, color = series, linetype = series)) +
  geom_hline(yintercept = 0, linewidth = 0.35, color = "#94A3B8") +
  geom_line(linewidth = 0.72) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("ML overlay" = "#0F766E", "TCHR" = "#2563EB", "Buy and hold" = "#64748B")) +
  scale_linetype_manual(values = c("ML overlay" = "solid", "TCHR" = "42", "Buy and hold" = "13")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", expand = expansion(mult = c(.01, .02))) +
  labs(title = "TCHR and the frozen XGBoost overlay",
       subtitle = "5 bp per-side costs. The overlay is active only after its out-of-time transfer coverage begins.",
       x = NULL, y = NULL, color = NULL, linetype = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"), plot.title = element_text(face = "bold"))

folds <- read.csv("strategy/fixed/results/tchr_ml_overlay_folds.csv", stringsAsFactors = FALSE)
predictions <- read.csv("strategy/fixed/results/tchr_ml_overlay_predictions.csv",
                        stringsAsFactors = FALSE)
fold_summary <- aggregate(
  trade_net ~ fold + selected, predictions, mean
)
baseline <- aggregate(trade_net ~ fold, predictions, mean)
names(baseline)[2L] <- "baseline_bp"
selected_values <- fold_summary[fold_summary$selected, c("fold", "trade_net"), drop = FALSE]
names(selected_values)[2L] <- "selected_bp"
fold_plot_data <- merge(folds[, c("fold", "test_start", "test_end", "events", "selected")],
                        baseline, by = "fold", all.x = TRUE)
fold_plot_data <- merge(fold_plot_data, selected_values, by = "fold", all.x = TRUE)
fold_plot_data$baseline_bp <- fold_plot_data$baseline_bp * 10000
fold_plot_data$selected_bp <- fold_plot_data$selected_bp * 10000
fold_plot_data$uplift_bp <- fold_plot_data$selected_bp - fold_plot_data$baseline_bp
fold_plot_data$rate <- fold_plot_data$selected / fold_plot_data$events
fold_plot_data$period <- paste0(format(as.Date(fold_plot_data$test_start), "%Y"),
                                "-", format(as.Date(fold_plot_data$test_end), "%Y"),
                                " / ", sprintf("%.0f%%", 100 * fold_plot_data$rate))
fold_plot_data$period <- factor(fold_plot_data$period,
                                levels = rev(fold_plot_data$period[order(fold_plot_data$test_start)]))
fold_plot_data$label_x <- pmax(fold_plot_data$baseline_bp, fold_plot_data$selected_bp) +
  max(abs(c(fold_plot_data$baseline_bp, fold_plot_data$selected_bp)), na.rm = TRUE) * .035
fold_plot <- ggplot(fold_plot_data, aes(y = period)) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "#64748B") +
  geom_segment(aes(x = baseline_bp, xend = selected_bp, yend = period),
               linewidth = 1.2, color = "#94A3B8") +
  geom_point(aes(x = baseline_bp, color = "All TCHR entries"), size = 3.1, shape = 16) +
  geom_point(aes(x = selected_bp, color = "ML-selected entries"), size = 3.2, shape = 18) +
  geom_text(aes(x = label_x, label = sprintf("%+.0f bp", uplift_bp)),
            hjust = 0, size = 3.2, color = "#334155") +
  scale_color_manual(values = c("All TCHR entries" = "#64748B", "ML-selected entries" = "#0F766E"),
                     name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(.08, .18))) +
  labs(title = "What the overlay changes in each test fold",
       subtitle = "Circle: all TCHR entries. Diamond: ML-selected entries. Labels show the uplift.",
       x = "Mean realized trade return (bp)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        plot.title = element_text(face = "bold"))

tail_tests <- read.csv("strategy/fixed/results/ml_overlay_model_tail_tests.csv", stringsAsFactors = FALSE)
matched <- tail_tests[tail_tests$metric == "ml_matched_log", , drop = FALSE]
matched$model <- factor(matched$model, levels = c("Random walk", "AR(1)", "GARCH-M", "EGARCH"))
path_data <- read.csv("strategy/fixed/results/ml_overlay_model_paths.csv", stringsAsFactors = FALSE)
path_data$model <- factor(path_data$model, levels = levels(matched$model))
matched_plot <- ggplot(path_data, aes(ml_matched_log)) +
  geom_histogram(aes(y = after_stat(density)), bins = 34, fill = "#B7D8D5",
                 color = "white", linewidth = 0.25) +
  geom_density(color = "#0F766E", linewidth = 0.8, adjust = 1.05) +
  geom_rect(data = matched, aes(xmin = q025, xmax = q975, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "#0F766E", alpha = 0.06) +
  geom_vline(data = matched, aes(xintercept = observed), color = "#D97706", linewidth = 0.9) +
  geom_vline(data = matched, aes(xintercept = median), color = "#2563EB",
             linewidth = 0.65, linetype = "dashed") +
  geom_text(data = matched, aes(x = observed, y = Inf, label = sprintf("observed | p = %.3f", p)),
            inherit.aes = FALSE, vjust = 1.55, size = 3.0, color = "#334155") +
  facet_wrap(~model, ncol = 2, scales = "free_y") +
  geom_vline(xintercept = 0, linewidth = 0.35, color = "#64748B") +
  labs(title = "Four Bootstrap distributions for the equal-exposure active return",
       subtitle = "500 simulated paths per process. Orange: observed value. Dashed blue: simulated median.",
       x = "Simulated log return difference", y = "Density") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

active <- tail_tests[tail_tests$metric == "ml_active_log", , drop = FALSE]
active$model <- factor(active$model, levels = c("Random walk", "AR(1)", "GARCH-M", "EGARCH"))
overlay <- tail_tests[tail_tests$metric == "ml_overlay_log", , drop = FALSE]
metric_map <- rbind(
  transform(active, metric_label = "Absolute active", metric_order = 1L),
  transform(matched, metric_label = "Equal-exposure active", metric_order = 2L),
  transform(overlay, metric_label = "Overlay vs core", metric_order = 3L)
)
metric_map$model_index <- match(metric_map$model, levels(active$model))
metric_map$y <- metric_map$model_index + (metric_map$metric_order - 2L) * .22
metric_map$metric_label <- factor(metric_map$metric_label,
                                  levels = c("Absolute active", "Equal-exposure active", "Overlay vs core"))
active_plot <- ggplot(metric_map, aes(y = y, color = metric_label)) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "#64748B") +
  geom_segment(aes(x = q025, xend = q975, yend = y), linewidth = 1.0) +
  geom_point(aes(x = observed), size = 2.6) +
  geom_text(aes(x = observed, label = sprintf("%.3f", p)),
            nudge_y = .10, size = 2.8, show.legend = FALSE) +
  scale_y_continuous(breaks = seq_along(levels(active$model)),
                     labels = levels(active$model), expand = expansion(mult = c(.08, .12))) +
  scale_color_manual(values = c("Absolute active" = "#2563EB",
                                "Equal-exposure active" = "#0F766E",
                                "Overlay vs core" = "#D97706"), name = NULL) +
  labs(title = "Bootstrap evidence across three return definitions",
       subtitle = "Point: observed value. Segment: 95% simulated interval. Labels: one-sided p-value.",
       x = "Log return difference", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        plot.title = element_text(face = "bold"))

save_plot <- function(plot, name, width, height) {
  ggsave(file.path(figure_dir, paste0(name, ".png")), plot,
         device = ragg::agg_png, width = width, height = height, dpi = 170, bg = "white")
  ggsave(file.path(figure_dir, paste0(name, ".svg")), plot,
         device = svglite::svglite, width = width, height = height, bg = "white")
}
save_plot(equity_plot, "01-overlay-equity-drawdown", 10.5, 7.2)
save_plot(fold_plot, "02-fold-trade-uplift", 10.5, 5.6)
save_plot(matched_plot, "03-bootstrap-matched-active", 10.5, 7.0)
save_plot(active_plot, "04-bootstrap-active", 10.5, 5.8)

summary <- read.csv("ml/results/tchr_integration/final/unified_closure_summary.csv", stringsAsFactors = FALSE)
format_pct <- function(x) sprintf("%.2f%%", 100 * x)
format_bp <- function(x) sprintf("%.2f bp", x)
html <- c(
  "<!doctype html>",
  "<html lang='en'><head><meta charset='utf-8'><meta name='viewport' content='width=device-width, initial-scale=1'>",
  "<title>TCHR ML Overlay Local Preview</title>",
  "<style>body{margin:0;background:#f3f6f8;color:#163047;font:15px/1.55 Arial,sans-serif}.wrap{max-width:1120px;margin:0 auto;padding:42px 26px 64px}.hero{background:#102a43;color:#fff;padding:32px;border-radius:18px}.hero h1{margin:0 0 8px;font-size:30px}.hero p{margin:0;color:#d6e4ef}.grid{display:grid;grid-template-columns:repeat(4,1fr);gap:14px;margin:22px 0}.card{background:#fff;border:1px solid #d7e2ea;border-radius:14px;padding:18px}.label{color:#60798d;font-size:12px;text-transform:uppercase;letter-spacing:.08em}.value{font-size:25px;font-weight:700;margin-top:4px}.figure{background:#fff;border:1px solid #d7e2ea;border-radius:14px;padding:16px;margin-top:18px}.figure img{display:block;width:100%;height:auto}.figure h2{font-size:18px;margin:0 0 12px}.note{color:#526b7d;font-size:13px}.footer{margin-top:24px;color:#60798d;font-size:13px}@media(max-width:760px){.grid{grid-template-columns:repeat(2,1fr)}.wrap{padding:24px 14px}.hero h1{font-size:24px}}</style></head><body>",
  "<main class='wrap'>",
  "<section class='hero'><h1>TCHR · Frozen XGBoost Overlay</h1><p>Local preview of the integrated walk-forward and four-process Bootstrap validation.</p></section>",
  sprintf("<section class='grid'><div class='card'><div class='label'>Trade uplift</div><div class='value'>%s</div><div class='note'>mean across 7 folds</div></div><div class='card'><div class='label'>Bootstrap matched p</div><div class='value'>%.3f–%.3f</div><div class='note'>four processes</div></div><div class='card'><div class='label'>Overlay compound</div><div class='value'>%s</div><div class='note'>covered interval</div></div><div class='card'><div class='label'>Max drawdown</div><div class='value'>%s</div><div class='note'>covered interval</div></div></section>",
          format_bp(summary$transfer_mean_trade_uplift_bp), summary$matched_active_min_p,
          summary$matched_active_max_p, format_pct(summary$overlay_compound),
          format_pct(summary$overlay_max_drawdown)),
  "<section class='figure'><h2>Equity and drawdown</h2><img src='figures/01-overlay-equity-drawdown.png' alt='TCHR and ML overlay cumulative net value and drawdown'></section>",
  "<section class='figure'><h2>Time-fold trade uplift</h2><img src='figures/02-fold-trade-uplift.png' alt='Realized TCHR trade uplift by time fold'></section>",
  "<section class='figure'><h2>Four Bootstrap distributions</h2><img src='figures/03-bootstrap-matched-active.png' alt='Four Bootstrap simulated distributions for equal-exposure active return'></section>",
  "<section class='figure'><h2>Bootstrap metric comparison</h2><img src='figures/04-bootstrap-active.png' alt='Bootstrap intervals for absolute active, equal-exposure active, and overlay increment'></section>",
  "<p class='footer'>The local preview is generated from the fixed 2001-03-01 to 2025-05-30 sample. It is not connected to GitHub.</p>",
  "</main></body></html>"
)
writeLines(html, file.path(demo_dir, "index.html"), useBytes = TRUE)
cat("Generated local demo at ", normalizePath(file.path(demo_dir, "index.html")), "\n", sep = "")
