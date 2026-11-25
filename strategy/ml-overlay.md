# TCHR Machine-Learning Overlay

The overlay uses the validated daily XGBoost utility model as a quality filter for TCHR entries. The model uses correlation-pruned close-time features, a five-session economic target, and a fixed 1,000-session time-decay weight. Model and policy selection occur on candidate-event validation windows. Scores are transferred only to TCHR entry events inside the corresponding out-of-time test windows. Thresholds are calibrated from past TCHR entries and then frozen before each test window.

Entries before the transfer coverage retain the deterministic TCHR rule. The overlay can suppress a TCHR position before its entry and leaves the original exit logic unchanged.

## Local results

The transfer audit contains 36 TCHR entry events across seven test folds. Actual trade-net uplift is 417.94 bp on average, with a 95% fold bootstrap lower bound of 143.74 bp. Six of seven folds are positive. The same-fold random-selection p-value is 0.0097.

On the covered portfolio interval, net compound return increases from 9.34% to 34.82%, maximum drawdown improves from −43.25% to −29.57%, and Sharpe ratio increases from 0.119 to 0.257.

The overlay ranks TCHR entries with the same trade ledger used by the base strategy.

## Unified Bootstrap closure

The frozen overlay is applied to 500 paths from each of four calibrated processes. Historical volume is retained as an exogenous known covariate while the OHLC path is simulated. The equal-exposure active return is positive in the observed path and has a one-sided p-value below 0.05 under all four processes. The absolute active-return p-value is below 0.05 under GARCH-M and EGARCH.

## Reproduce locally

~~~sh
Rscript --vanilla strategy/run-ml-overlay.R
Rscript --vanilla strategy/audit-ml-overlay.R
Rscript --vanilla strategy/run-ml-bootstrap.R --paths=500
Rscript --vanilla strategy/audit-ml-bootstrap.R
~~~

Key outputs are [overlay fold metrics](fixed/results/tchr_ml_overlay_folds.csv), [overlay portfolio comparison](fixed/results/tchr_ml_overlay_portfolio.csv), and [stability audit](../ml/results/tchr_integration/final/overlay_stability_audit.csv).

The unified closure is recorded in [the Bootstrap tail tests](fixed/results/ml_overlay_model_tail_tests.csv), [the closure checks](../ml/results/tchr_integration/final/unified_closure_checks.csv), and [the closure summary](../ml/results/tchr_integration/final/unified_closure_summary.csv).

The local visual preview is [overlay-demo/index.html](fixed/overlay-demo/index.html). Its figures are regenerated with `Rscript --vanilla strategy/make-ml-overlay-demo.R`.
