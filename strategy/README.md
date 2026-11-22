# Trend-Confirmed Hammer Reversal (TCHR)

[English research report](report-en.html) · [Chinese report](report.html)

I developed TCHR as the main strategy in this study. It combines a 100-day moving-average trend buffer, a hammer-line setup, close confirmation, and next-session execution. The fixed sample runs from 2001-03-01 to 2025-05-30 and contains 5,973 sessions.

## Research structure

The central evidence comes from four Bootstrap-style stochastic checks. Each process generates 500 equal-length paths, reconstructs valid OHLC candles, and reruns TCHR, the buy-and-hold benchmark, and the ablation strategies.

- Random Walk Bootstrap
- AR(1) Bootstrap
- Student-t GARCH-M Bootstrap
- EGARCH Bootstrap

The same window also supports a circular block max-t correction across 51 candidate rules. The report presents the fixed-sample return, risk, cost, path distributions, and incremental hammer-line effects.

## Reproduce

~~~sh
Rscript --vanilla strategy/run-fixed.R
Rscript --vanilla strategy/run-fixed.R verify
~~~

- [Fixed protocol](fixed/protocol.md)
- [TCHR performance](fixed/results/performance.csv)
- [TCHR trades](fixed/results/tchr_trades.csv)
- [Daily ledger](fixed/results/daily.csv)
- [Bootstrap tail probabilities](fixed/results/model_tail_tests.csv)
- [Simulated paths](fixed/results/model_paths.csv)
- [Candidate-family max-t correction](fixed/results/simultaneous_rule_test.csv)
- [Model coefficients and diagnostics](fixed/results/model_coefficients.csv)
- [EGARCH optimizer comparison](fixed/results/egarch_optimization.csv)

## 中文补充

TCHR 是我建立的趋势确认锤头线反转策略。这里的主要工作是用四种 Bootstrap 型随机过程和候选规则族校正来验证策略表现，固定回测只作为统计检验的基础。
