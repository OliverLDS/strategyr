# Philosophy and Architecture

## Ad-hoc vs. Backtesting-based TA Indicators

`strategyr` is not confined to technical analysis; its broader ambition is to support every component of a financial trading strategy. Nonetheless, the development begins with TA-based methods.

Technical analysis is rarely emphasized in academic finance and often sits outside the scope of mainstream theory.
Because of that, I learned TA concepts from the ground up—a process that gave me a fresh perspective and a strong curiosity about the logic behind popular indicators.

The objective of this package is to reorganize and reinterpret TA indicators based on a deeper understanding of *what each indicator truly captures* in different market contexts, supplemented by empirical backtesting on real and simulated data. The aim is to separate ad-hoc, mechanically used indicators from features that consistently demonstrate predictive or behavioural value in testing.

At this stage, much of the package reflects my own intuition and exploratory thinking. As development progresses, I will refine these components with academic literature, theoretical foundations, and empirical evidence.

## Exported Object Types

At the core of any trading strategy lies the prediction—or at least structured estimation—of one or more numerical outcomes such as price, return, volatility for equities; greeks for options; or duration and yield for bonds. We refer to these collectively as *trading-relevant outcomes*.

`strategyr` exports five categories of objects:

1. Non-prediction-oriented Features
Indicators computed purely from observed data using predefined rules, without explicit predictive intent. 
Examples include EMA-20, MACD lines, and RSI. 
These describe market characteristics but often lack validated links to trading outcomes.
The package includes such indicators mainly for compatibility with traditional TA language and workflows.

2. Prediction-oriented Features
Indicators explicitly designed to estimate or forecast trading-relevant outcomes.
These constitute the majority of features exported by the package.
Each prediction-oriented feature also carries a dynamic metric representing its recent predictive accuracy.

3. Binary Events
Exogenous events (e.g., FOMC meetings) or endogenous signals (e.g., a breakout from resistance) encoded as binary indicators.
For each event type, the package computes post-event trading outcomes, giving these event markers predictive value.
Compared with continuous features, binary events often align more naturally with decision-making logic.

4. Positions
Numeric values between −1 and 1 representing the directional stance and relative size on an asset, ignoring leverage. Positions are derived from selected strategies during backtesting.
- −1: full short
- 1: full long
- 0: flat
- 0.5: half-sized long, etc.

5. Orders
Standardized executable trading orders produced by strategy logic, ready for integration into a trading engine.

## data.table vs. dplyr

Although dplyr and data.frame structures dominate typical R data workflows, this package is built primarily on **data.table**.
The main reasons are:
- **Reference semantics**, which avoid unnecessary copying and significantly reduce memory overhead.
- **Highly optimized C-level operations**, which are crucial for performance-intensive environments like backtesting and real-time order generation.

## R vs. Rcpp

To maximize performance, most iteration-heavy computations are implemented in Rcpp.
However, vectorized logic, user-facing functions, and wrapper layers remain in pure R so users can easily read, understand, and modify the code.

This hybrid approach provides both computational efficiency and accessibility for users who want to inspect or extend the framework.



