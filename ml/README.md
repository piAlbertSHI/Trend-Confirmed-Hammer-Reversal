# Machine Learning Research Extension

This directory develops an out-of-time machine-learning layer for the Trend-Confirmed Hammer Reversal (TCHR) research. The original deterministic backtest remains unchanged. The machine-learning layer is designed as a signal-quality filter that estimates whether a five-session forward trade can produce a positive net return after 5 bp per-side transaction costs.

## Research design

The feature table is built from the verified Hang Seng Index daily OHLCV history covering 1 March 2001 to 30 May 2025. All predictors are observable at the signal-session close. The target uses the next session open and the close five sessions later, so the label includes execution timing and transaction-cost drag.

The pipeline includes:

- OHLC and volume validation, duplicate-date checks, fixed-window checks, warm-up controls, and finite-value audits.
- Candle geometry, gap and intraday returns, trend distances and slopes, Bollinger-style location, ATR, volatility, RSI, drawdown, volume, and an explicit TCHR setup indicator.
- A binary net-return label and an excess-return label relative to the passive close-to-close benchmark.
- Chronological train, validation, and holdout periods with a ten-session embargo at each validation boundary.
- Median imputation and scaling fit inside each training window only. Warm-up rows are excluded from model fitting.
- Elastic Net logistic regression as a sparse, regularized reference model.
- Regularized XGBoost with depth-6 trees, an 1,800-round cap, eta 0.03, row and column subsampling at 0.82, minimum child weight 12, L1 penalty 1, L2 penalty 20, histogram trees, and validation AUPRC early stopping.

The declared split is:

| Split | Period | Use |
| --- | --- | --- |
| Train | 2001-03-01 to 2013-12-31 | model fitting and chronological cross-validation |
| Validation | 2014-01-01 to 2018-12-31 | model selection and early stopping |
| Holdout | 2019-01-01 to 2025-05-30 | final out-of-time evaluation |

## Local execution

From the project root:

Rscript ml/run.R check

runs the data-quality and split checks without fitting models.

Rscript ml/run.R models

fits both baselines and writes evaluation tables.

Rscript ml/run.R all

runs the complete local pipeline.

Generated artifacts are written to ml/results/ and include the data-quality audit, feature dictionary, time-split definitions, chronological CV folds, validation and holdout metrics, Elastic Net lambda diagnostics, XGBoost feature importance, serialized model objects, and a JSON run summary.

This research extension is intentionally local at this stage. It is not part of the current GitHub publication.

## 中文补充

该目录在 TCHR 确定性回测之外建立机器学习研究层，用于评估五个交易日净收益为正的概率，并作为交易信号质量过滤器。特征只使用信号日收盘时能够获得的信息，标签从下一交易日开盘计算到第五个交易日收盘，并计入双边交易成本。

处理流程覆盖 OHLCV 数据校验、重复日期检查、预热期控制、有限值审计、时间切分、训练集内插补与标准化、Elastic Net 逻辑回归基线，以及带正则化和早停机制的 XGBoost。最终留出区间只用于样本外评估，模型输出暂存于本地 ml/results/。




## Event-level validation

The event extension removes overlapping setup observations with a five-session gap. It currently contains 58 independent TCHR events. Two labels are evaluated:

- label_net_5d identifies a positive five-session net return after costs.
- label_excess_5d identifies a positive return relative to the passive close-to-close benchmark.

The event model uses a compact, pre-specified feature set and a shallow, strongly regularized XGBoost configuration. Walk-forward folds contain expanding training events, an eight-event validation block, a six-event test block, and a ten-session embargo. Both validation-threshold selection and fixed top-half ranking selection are recorded.

Event diagnostics and walk-forward outputs are written to ml/results/event/. This event-level experiment remains a diagnostic module. The production overlay uses the candidate-event daily transfer model described in strategy/ML_OVERLAY.md.

## 中文补充

事件级模块将相互重叠的 setup 合并为至少间隔五个交易日的独立事件。目前共有 58 个独立事件，同时检验交易成本后净收益标签和相对恒生指数的超额收益标签。

walk-forward 验证使用扩展训练窗口、验证区间、测试区间和时间 embargo。事件级模块保留为诊断对照，正式接入路径见 strategy/ML_OVERLAY.md。

## Continuous-return regression

The regression extension predicts five-session net or excess log returns on a 10,000-basis-point training scale, then restores the original return scale for evaluation. This keeps the target magnitude compatible with the regularization terms. Training targets are winsorized using training-window quantiles only.

Each purged walk-forward fold compares the fixed event feature set with a correlation-pruned set. Correlation pruning is fitted inside the training events of each fold. Test selection uses a pre-specified top-half ranking rule. Fold-level uplift is accompanied by a bootstrap confidence interval.

The regression outputs are written to ml/results/event_regression/. The strongest candidate in the current run is the correlation-pruned model trained on the excess-return target. It shows positive net-return uplift, but its excess-return median and confidence interval still cross zero. The stability gate remains false.

Run the regression extension with Rscript ml/event_regression_run.R all. Regression code is in ml/event_regression.R and ml/event_regression_run.R.

## 中文补充

连续收益回归分别预测五日净收益和超额收益。训练时将收益转换为基点尺度，完成训练后再还原为收益小数。每个时间折单独计算相关性裁剪，避免把测试区间的信息带入特征选择。

连续收益回归保留为独立诊断。TCHR 的正式 ML overlay 使用经过稳定性审计的 daily utility transfer 方案。


## Candidate-event expansion

The candidate layer keeps all independent TCHR events and adds relaxed setup candidates. It currently contains 136 events, including 58 original TCHR events and 78 candidate-only events. The relaxed event definition uses support at 0.94 of the one-hundred-session average, a lower-shadow ratio above 1.5, an upper-shadow ratio below 1.5, a body-range cap, and a five-session event gap.

The candidate model uses rolling pre-event market expectations as the economic benchmark. It evaluates five-, ten-, and twenty-session net return, expected-market excess return, and a drawdown-penalized utility. Hyperparameters and the selection policy are chosen inside each validation window.

Run the candidate experiment with Rscript ml/candidate_regression_run.R. Random selection controls are generated with Rscript ml/candidate_random_baseline_run.R. Outputs are written to ml/results/candidates/ and ml/results/candidate_regression/.

The expanded candidate sample does not currently pass the integration gate. The five-session correlation-pruned run is close to neutral, while ten- and twenty-session runs are negative. No candidate filter is connected to TCHR.

## 中文补充

候选事件层保留全部独立 TCHR 事件，并加入较宽松的候选 setup。目前共有 136 个事件，其中 58 个来自原始 TCHR，78 个为新增候选事件。

经济目标使用信号日前滚动计算的市场期望收益，分别评估五日、十日和二十日净收益、超额收益以及回撤惩罚效用。参数、特征裁剪和选取比例均只在验证窗口内确定。当前扩样本后的模型没有通过稳定增益门槛，因此仍不接入 TCHR。
