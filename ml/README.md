# Machine-Learning TCHR Overlay

This directory contains the feature, event, and walk-forward components behind the frozen XGBoost filter used by TCHR.

## Pipeline

The workflow builds close-time features from the fixed Hang Seng Index OHLCV window, forms five-session trade labels, and evaluates Elastic Net and XGBoost models with chronological splits and purged walk-forward folds. Feature construction covers candle geometry, trend distance, volatility, momentum, volume, drawdown and the TCHR setup state.

The published overlay uses a correlation-pruned feature set, a five-session utility target and a fixed 1,000-session time-decay weight. The selected scores transfer to TCHR entry events in seven out-of-time folds, where thresholds are frozen before each test window.

## Integrated result

The transfer audit covers 36 TCHR entry events. Mean realized trade-net uplift is **+417.94 bp**, the fold-bootstrap lower bound is **+143.74 bp**, and six of seven folds are positive. The overlay raises covered-interval compound return from 9.34% to 34.82% while reducing maximum drawdown from −43.25% to −29.57%.

The frozen overlay is applied to 500 paths from each of four Bootstrap processes. Equal-exposure active return remains positive across all four processes. The complete integration and closure tables are linked from [the TCHR ML overlay](../strategy/ml-overlay.md).

## Reproduce

~~~sh
Rscript --vanilla strategy/run-ml-overlay.R
Rscript --vanilla strategy/audit-ml-overlay.R
Rscript --vanilla strategy/run-ml-bootstrap.R --paths=500
Rscript --vanilla strategy/audit-ml-bootstrap.R
~~~

Key outputs include feature diagnostics, walk-forward folds, overlay portfolio tables, transfer stability checks and the unified Bootstrap closure tables under ml/results/ and strategy/fixed/results/.

The lower-level scripts in this directory retain feature construction, event tables, regression comparisons and model diagnostics used to build the frozen overlay.
