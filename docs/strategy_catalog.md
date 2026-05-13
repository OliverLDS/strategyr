# strategyr Strategy Catalog

This catalog lists the public `strat_*` families currently exposed by
`strategyr`. Each family should provide a target-position function and an
action-plan function:

- `strat_<name>_tgt_pos()`
- `strat_<name>_action_plan()`

Most families are single-instrument strategies that return a numeric target
position path and translate the latest target through `gen_action_plan_rcpp()`.
Portfolio-level families return target weights and route through
`plan_portfolio_adjustment()` plus `build_order_intents()`.

## Strategy Levels

| Level | Meaning | Action-plan path |
|---|---|---|
| Single instrument | One traded exposure per strategy call | `.action_plan_from_tgt_pos()` |
| Summarized panel | Curve or option-chain data summarized before target generation | `.action_plan_from_tgt_pos()` on latest summary target |
| Portfolio level | Cross-sectional weights across assets | `plan_portfolio_adjustment()` and `build_order_intents()` |

## Core And Ladder

| Strategy family | Category | Main inputs | Level | Default `strat_id` |
|---|---|---|---|---:|
| `strat_buy_and_hold_*` | baseline | `DT`, `value` | single instrument | `1` |
| `strat_ladder_bounce_*` | ladder/cycle reversion | OHLC, ladder-cycle parameters | single instrument | `201` |
| `strat_ladder_breakout_*` | ladder/cycle breakout | OHLC, ladder-cycle parameters | single instrument | `202` |

## Trend And Momentum

| Strategy family | Category | Main inputs | Level | Default `strat_id` |
|---|---|---|---|---:|
| `strat_ema_cross_*` | trend | `close`, EMA windows, ATR freshness settings | single instrument | `101` |
| `strat_ema_cross_adx_*` | trend filter | EMA windows, `adx_n`, `adx_threshold` | single instrument | `103` |
| `strat_ema_cross_slope_confirm_*` | trend filter | EMA windows, `slope_lag` | single instrument | `104` |
| `strat_ema_triple_trend_*` | trend alignment | fast, mid, and slow EMA windows | single instrument | `106` |
| `strat_macd_cross_*` | momentum | MACD fast, slow, and signal windows | single instrument | `304` |
| `strat_macd_contrarian_*` | contrarian momentum | MACD fast, slow, and signal windows | single instrument | `305` |
| `strat_macd_zero_line_*` | momentum filter | MACD fast, slow, and signal windows | single instrument | `310` |
| `strat_macd_histogram_momentum_*` | momentum acceleration | MACD histogram and `accel_lag` | single instrument | `314` |
| `strat_relative_strength_*` | relative momentum | `x_col`, `y_col`, relative-strength window | single instrument | `503` |
| `strat_relative_strength_dual_momentum_*` | dual momentum | relative strength plus absolute momentum | single instrument | `504` |
| `strat_relative_strength_persistence_*` | persistent relative momentum | relative strength plus `persist_n` | single instrument | `508` |

## Mean Reversion And Oscillators

| Strategy family | Category | Main inputs | Level | Default `strat_id` |
|---|---|---|---|---:|
| `strat_bollinger_revert_*` | price reversion | Bollinger window and band width | single instrument | `301` |
| `strat_bollinger_revert_rsi_*` | reversion confirmation | Bollinger bands plus RSI thresholds | single instrument | `308` |
| `strat_bollinger_low_adx_revert_*` | regime-gated reversion | Bollinger bands plus ADX ceiling | single instrument | `312` |
| `strat_rsi_revert_*` | oscillator reversion | RSI window, oversold, overbought, exit level | single instrument | `303` |
| `strat_rsi_logr_revert_*` | log-return oscillator reversion | log-return RSI half-life and thresholds | single instrument | `306` |
| `strat_rsi_trend_aware_revert_*` | trend-aware oscillator reversion | RSI plus trend EMA | single instrument | `311` |
| `strat_rsi_dynamic_threshold_revert_*` | adaptive oscillator reversion | RSI plus rolling quantile thresholds | single instrument | `315` |
| `strat_rsi_divergence_*` | pivot divergence | close, RSI, pivot windows | single instrument | `316` |
| `strat_vwap_revert_*` | fair-value reversion | VWAP deviation, imbalance, spread filters | single instrument | `506` |

## Breakout, Volatility, And Pullback

| Strategy family | Category | Main inputs | Level | Default `strat_id` |
|---|---|---|---|---:|
| `strat_donchian_breakout_*` | channel breakout | Donchian window | single instrument | `302` |
| `strat_donchian_turtle_*` | channel breakout with exit | entry and exit Donchian windows | single instrument | `307` |
| `strat_donchian_retest_breakout_*` | breakout retest | Donchian window, retest buffer, confirm window | single instrument | `313` |
| `strat_atr_breakout_*` | volatility breakout | ATR window and breakout multiple | single instrument | `401` |
| `strat_atr_breakout_trailing_stop_*` | breakout with trailing exit | ATR entry and trailing-stop multiples | single instrument | `405` |
| `strat_bollinger_squeeze_breakout_*` | volatility compression breakout | Bollinger width squeeze threshold | single instrument | `309` |
| `strat_vol_target_*` | volatility targeting | trend window, realized-vol window, target vol | single instrument | `402` |
| `strat_vol_target_regime_floor_*` | volatility targeting with risk-off floor | target vol plus realized-vol ceiling | single instrument | `406` |
| `strat_trend_pullback_*` | trend pullback | trend EMA and RSI pullback levels | single instrument | `403` |
| `strat_trend_pullback_atr_*` | trend pullback with ATR zone | trend EMA, RSI, ATR distance band | single instrument | `404` |
| `strat_regime_switch_*` | meta strategy | trend, reversion, volatility, breadth gates | single instrument | `105` |

## Relative Value And Cross-Sectional

| Strategy family | Category | Main inputs | Level | Default `strat_id` |
|---|---|---|---|---:|
| `strat_pair_spread_revert_*` | spread reversion | `x_col`, `y_col`, spread z-score | single instrument | `501` |
| `strat_pair_spread_bollinger_*` | spread band reversion | pair spread plus Bollinger bands | single instrument | `505` |
| `strat_pair_spread_half_life_revert_*` | spread reversion filter | spread z-score plus half-life proxy | single instrument | `510` |
| `strat_ratio_revert_*` | ratio reversion | `x_col`, `y_col`, ratio z-score | single instrument | `502` |
| `strat_cross_sectional_rank_allocator_*` | generic rank allocation | date, asset, score columns | portfolio level | `507` |

## FX, Fixed Income, Credit, And Curves

| Strategy family | Category | Main inputs | Level | Default `strat_id` |
|---|---|---|---|---:|
| `strat_fx_carry_*` | FX carry | domestic and foreign rates | single instrument | `601` |
| `strat_fx_carry_trend_*` | FX carry with trend | FX carry plus spot trend | single instrument | `612` |
| `strat_fx_carry_basket_rank_*` | FX carry rank allocation | date, asset, carry signal | portfolio level | `511` |
| `strat_bond_carry_roll_*` | bond carry and roll | bond terms, yield, optional funding | single instrument | `602` |
| `strat_bond_carry_roll_spread_filter_*` | bond carry and roll filter | carry-roll score plus credit spread filter | single instrument | `606` |
| `strat_bond_carry_roll_duration_cap_*` | bond carry and roll filter | carry-roll score plus duration bucket/cap | single instrument | `613` |
| `strat_credit_spread_momentum_*` | credit spread momentum | credit spread or computed spread momentum | single instrument | `609` |
| `strat_credit_spread_revert_*` | credit spread reversion | excess spread z-score | single instrument | `610` |
| `strat_curve_steepener_*` | curve slope | short and long rates | single instrument | `603` |
| `strat_curve_steepener_zscore_*` | normalized curve slope | curve slope z-score and mode | single instrument | `605` |
| `strat_curve_butterfly_*` | curve butterfly | short, mid, and long rates | single instrument | `608` |

## Futures And Funding

| Strategy family | Category | Main inputs | Level | Default `strat_id` |
|---|---|---|---|---:|
| `strat_roll_yield_*` | futures roll yield | contract rank, price, time to expiry | summarized panel | `604` |
| `strat_roll_yield_momentum_*` | roll yield with price momentum | roll-yield summary plus front-price momentum | summarized panel | `607` |
| `strat_roll_yield_cross_sectional_*` | roll-yield rank allocation | date, asset, roll-yield signal | portfolio level | `509` |
| `strat_roll_yield_mean_revert_*` | roll-yield reversion | roll-yield z-score | summarized panel | `614` |
| `strat_funding_basis_convergence_*` | funding/basis convergence | basis, funding, or FX-basis inputs | single instrument | `611` |

## Options And Volatility

| Strategy family | Category | Main inputs | Level | Default `strat_id` |
|---|---|---|---|---:|
| `strat_iv_skew_*` | option skew | option chain, moneyness, IV | summarized panel | `701` |
| `strat_iv_skew_zscore_*` | option skew z-score | option skew summary and rolling z-score | summarized panel | `706` |
| `strat_iv_skew_realized_vol_confirm_*` | option skew with IV/RV confirmation | skew plus implied-realized volatility spread | summarized panel | `709` |
| `strat_iv_directional_overlay_*` | option skew with trend overlay | skew plus underlying trend proxy | summarized panel | `710` |
| `strat_iv_term_structure_*` | option IV term structure | option chain and expiry structure | summarized panel | `702` |
| `strat_vol_carry_*` | volatility carry | implied vol, realized vol, IV/RV spread | single instrument | `707` |
| `strat_gamma_scalp_support_*` | gamma support proxy | option IV, realized vol, time to expiry | single instrument | `708` |
| `strat_straddle_*` | option structure proxy | ATM option IV summary | summarized panel | `703` |
| `strat_strangle_*` | option structure proxy | OTM put/call IV summary | summarized panel | `704` |
| `strat_vertical_spread_*` | option structure proxy | option type, moneyness, IV | summarized panel | `705` |

## Notes

- `*_tgt_pos()` functions are the backtest-facing rule surface.
- `*_action_plan()` functions are the latest-state execution-planning surface.
- `debug = TRUE` may return additional feature columns, summarized data, or
  target-weight paths depending on the family.
- Portfolio-level families intentionally use `portfolio_state`, `equity`, and
  order-intent output instead of the single-instrument `state` contract.
