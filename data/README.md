# Data

## Primary Hang Seng archive

HSI_25years.csv is the daily OHLCV file used by the TCHR study. It covers 2000-04-07 to 2025-04-01. The loader validates date order, positive prices, and OHLC geometry.

SHA-256 e8e453bd22e7a2efca47039b7b5f1b278146b27f76b03ebe1867d7eee680cd09.

## Extended input archive

HSI_extension_archive.json preserves a supplemental market response for source cross-checking. The fixed TCHR report uses only 2001-03-01 to 2025-05-30.

SHA-256 d5a31d37668fcb57dc14f21f46509f7a2638cb7660522e269efee89173c8fb9f.

[Fixed sample](../strategy/fixed/results/sample.csv) · [Simulation window](../strategy/fixed/results/simulation_sample.csv)

## 中文补充

主数据为恒生指数日线 OHLCV。固定 TCHR 报告只使用到 2025 年 5 月 30 日。扩展响应仅用于归档和来源核对。
