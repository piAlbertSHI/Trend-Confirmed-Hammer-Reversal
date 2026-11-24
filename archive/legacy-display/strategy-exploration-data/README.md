# 第二轮策略研究的补充数据

主恒指行情沿用本地现有档案及扩展，当前固定策略报告只使用 2001-03-01 至 2025-05-30。恒指开盘和收盘点位用于统一回测基准。[主行情来源](../../data/README.md)

## 盈富基金 2800.HK

TraHK_chart_2026-11-20.json 留存 Yahoo Finance 的港币柜台行情，作为输入来源归档。固定报告只截取至 2025-05-30。请求从 2000-04-01 起，响应实际覆盖 2008-01-02 至 2026-11-20，共 4,659 条时间记录、28 条派息事件。对齐恒指后可用 4,631 日。24 条基金记录价格缺失或不可用，4 日存在有效基金报价但无法与恒指日期对齐。在共同日期范围内，20 个恒指交易日没有有效基金报价，用后一可得报价日继续持仓或调仓。有关日期单列。 [请求](https://query1.finance.yahoo.com/v8/finance/chart/2800.HK?period1=954547200&period2=1795219200&interval=1d&events=div%2Csplits&includeAdjustedClose=true) · [日期审计](../results/trahk_audit.csv) · [缺少基金报价的恒指日期](../results/trahk_missing_dates.csv)

基金收盘使用数据商的调整收盘。基金开盘乘同日的调整收盘与原收盘之比，令跨日价格比率沿用同一派息调整口径。策略信号由恒指交易日确定，两种基金组合都执行相同的费用和调整规则。调整行情用于收益近似，报价可得性与交易容量需在实盘执行中另外核查。[基金管理人资料](https://www.trahk.com.hk/en-hk/trahk-fund/)

SHA-256 d0ff45e05d8318de6b2fef334b97c87c08549a38aa6fea7f0fdf93a05501e5de

## 早期恒指日线

HSI_pre2000_chart.json 留存 Yahoo Finance 从 1986 年至 2000 年 6 月的请求结果。1986–1989 年部分有效记录的开高低收完全相同，早期复核采用 1990 年起的真实非空开高低收记录。响应中的全字段空值日期单独计数，未填补价格。56 个与原始档案重叠的交易日逐字段相符。复核只使用原档起点之前的 2,545 个交易日，去掉 220 日预热后有 2,325 个交易日。样本数据与两轮开发区间无重叠。[请求](https://query1.finance.yahoo.com/v8/finance/chart/%5EHSI?period1=504921600&period2=962409600&interval=1d) · [日期与字段审计](../results/early_hsi_audit.csv)

SHA-256 8a472039a2df264ab818090563bbc422d9824b0f302fcc76d75fe7b87ae562c5
