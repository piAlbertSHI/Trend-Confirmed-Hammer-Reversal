# TCHR Fixed-Sample Protocol

The study evaluates Trend-Confirmed Hammer Reversal (TCHR) from 2001-03-01 to 2025-05-30. The first 220 sessions provide indicator warm-up. Every strategy, benchmark, cost scenario, and stochastic path uses the same fixed boundary.

## Strategy definition

TCHR combines two position components.

- **Trend filter**. Enter at the next session open when the close is at least 1% above the 100-session moving average. Exit at the next session open when the close is at least 1% below it. The buffer preserves the previous state between the two thresholds.
- **Hammer confirmation**. A hammer has a lower shadow longer than twice its body and an upper shadow shorter than its body. The close must be below the 20-session average and no more than 4% below the 100-session average. A setup can wait for up to three closing sessions. A close above the setup high confirms the entry. The position lasts at most 10 sessions and exits after a close below the setup low or trend support.

Both components share a single full-capital account. Decisions use the closing bar and trades execute at the next session open. Overnight returns belong to the position held at the previous close. Cash earns zero. The Hang Seng Index buy-and-hold benchmark buys at the first open and liquidates at the final close.

## Statistical validation

I generate 500 equal-length paths under four calibrated processes.

1. Random Walk Bootstrap with empirical daily returns.
2. AR(1) Bootstrap with fitted first-order dependence and resampled innovations.
3. Student-t GARCH-M Bootstrap with conditional volatility and a mean-in-volatility term.
4. EGARCH Bootstrap with asymmetric volatility responses. Converged solnp and nlminb fits are compared by likelihood before simulation.

Each path receives a valid OHLC reconstruction, a fresh TCHR run, a same-path buy-and-hold benchmark, and the same ablation rules. Circular block resampling then evaluates the observed daily active return. A max-t correction is applied across 51 candidate rules.

## Costs and outputs

The primary cost is 5 basis points per side. The analysis also reports 0, 2, 10, 20, and 30 basis points. Results include net compound return, drawdown, active return, exposure-adjusted return, path-level tail probabilities, Monte Carlo intervals, and the candidate-family correction.
