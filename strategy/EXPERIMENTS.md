# Trend-Confirmed Hammer Reversal（TCHR）研究说明

当前展示入口是[固定历史样本报告](report.html)。项目统一使用 2001-03-01 至 2025-05-30 的 5,973 个交易日，前 220 日用于指标预热。

以单边 5 bp 计，TCHR 策略净复利 **+173.89%**，同期恒指买入持有 **+58.31%**，最大回撤分别为 **−43.25%** 和 **−65.18%**。报告中的候选规则、费用情景和四个随机过程都使用这个固定窗口。

旧的扩展探索文件保留在 archive/，不属于当前展示入口。固定回测的 R 入口是 run-fixed.R，固定结果和图表位于 fixed/。

~~~sh
Rscript --vanilla run-fixed.R
Rscript --vanilla run-fixed.R verify
~~~
