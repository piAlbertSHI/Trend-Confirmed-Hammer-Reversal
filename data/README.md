# Data

## Primary Hang Seng archive

HSI_25years.csv is the archived daily OHLCV file used by the study. It covers 2000-04-07 to 2025-04-01 and contains the original CSV metadata rows. The first price field is Close and the fourth is Open. The R loader validates date order, positive prices, and OHLC geometry.

SHA-256 e8e453bd22e7a2efca47039b7b5f1b278146b27f76b03ebe1867d7eee680cd09.

## Extended input archive

HSI_extension_archive.json preserves the supplemental Yahoo Finance response used for source cross-checking. The fixed TCHR report uses only the 2001-03-01 to 2025-05-30 sample. The archive input is retained for provenance and overlap verification.

SHA-256 d5a31d37668fcb57dc14f21f46509f7a2638cb7660522e269efee89173c8fb9f.

[Fixed sample](../strategy/fixed/results/sample.csv) · [Simulation window](../strategy/fixed/results/simulation_sample.csv)

## Data conventions

Open, High, Low, and Close are index points. The Hang Seng Index is used as a price proxy. Returns exclude dividends, spreads, tracking error, and market impact. Simulated paths are inference inputs and are kept separate from the observed market archive.

## 中文补充

主数据为恒生指数日线 OHLCV。固定 TCHR 报告只使用到 2025 年 5 月 30 日。扩展响应保留在归档中，用于来源核对和复现数据边界。
