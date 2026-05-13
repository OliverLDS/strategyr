// rcpp_strategies.cpp

#include <Rcpp.h>
#include <algorithm>

#include "strat_buy_and_hold.h"

// [[Rcpp::export]]
Rcpp::NumericVector strat_buy_and_hold_rcpp(const Rcpp::NumericVector& timestamp) {
    size_t n = timestamp.size();
    Rcpp::NumericVector tgt_pos(n);

    strat_buy_and_hold(REAL(tgt_pos), REAL(timestamp), n);

    return tgt_pos;
}

// [[Rcpp::export]]
Rcpp::NumericVector strat_rsi_revert_signal_cpp(
    const Rcpp::NumericVector& rsi,
    double oversold = 30.0,
    double overbought = 70.0,
    double exit_level = 50.0,
    double target_size = 1.0
) {
    int n = rsi.size();
    Rcpp::NumericVector out(n);
    double pos_now = 0.0;

    for (int i = 0; i < n; ++i) {
        double x = rsi[i];
        if (Rcpp::NumericVector::is_na(x)) {
            out[i] = pos_now;
            continue;
        }

        if (pos_now > 0.0 && x >= exit_level) {
            pos_now = 0.0;
        } else if (pos_now < 0.0 && x <= exit_level) {
            pos_now = 0.0;
        }

        if (pos_now == 0.0) {
            if (x <= oversold) {
                pos_now = target_size;
            } else if (x >= overbought) {
                pos_now = -target_size;
            }
        }
        out[i] = pos_now;
    }

    return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector strat_donchian_turtle_signal_cpp(
    const Rcpp::NumericVector& close,
    const Rcpp::NumericVector& entry_high,
    const Rcpp::NumericVector& entry_low,
    const Rcpp::NumericVector& exit_high,
    const Rcpp::NumericVector& exit_low,
    double target_size = 1.0
) {
    int n = close.size();
    if (entry_high.size() != n || entry_low.size() != n || exit_high.size() != n || exit_low.size() != n) {
        Rcpp::stop("All input vectors must have the same length.");
    }

    Rcpp::NumericVector out(n);
    double pos_now = 0.0;

    for (int i = 0; i < n; ++i) {
        if (i == 0 ||
            Rcpp::NumericVector::is_na(close[i]) ||
            Rcpp::NumericVector::is_na(entry_high[i - 1]) ||
            Rcpp::NumericVector::is_na(entry_low[i - 1]) ||
            Rcpp::NumericVector::is_na(exit_high[i - 1]) ||
            Rcpp::NumericVector::is_na(exit_low[i - 1])) {
            out[i] = pos_now;
            continue;
        }

        if (pos_now > 0.0 && close[i] < exit_low[i - 1]) {
            pos_now = 0.0;
        } else if (pos_now < 0.0 && close[i] > exit_high[i - 1]) {
            pos_now = 0.0;
        }

        if (pos_now == 0.0) {
            if (close[i] > entry_high[i - 1]) {
                pos_now = target_size;
            } else if (close[i] < entry_low[i - 1]) {
                pos_now = -target_size;
            }
        }
        out[i] = pos_now;
    }

    return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector strat_atr_breakout_trailing_stop_signal_cpp(
    const Rcpp::NumericVector& close,
    const Rcpp::NumericVector& atr,
    double atr_mult = 1.0,
    double trail_mult = 2.0,
    double target_size = 1.0
) {
    int n = close.size();
    if (atr.size() != n) {
        Rcpp::stop("All input vectors must have the same length.");
    }

    Rcpp::NumericVector out(n);
    double pos_now = 0.0;
    double high_water = NA_REAL;
    double low_water = NA_REAL;

    for (int i = 0; i < n; ++i) {
        if (i == 0 ||
            Rcpp::NumericVector::is_na(close[i]) ||
            Rcpp::NumericVector::is_na(close[i - 1]) ||
            Rcpp::NumericVector::is_na(atr[i]) ||
            Rcpp::NumericVector::is_na(atr[i - 1])) {
            out[i] = pos_now;
            continue;
        }

        bool exited_now = false;
        if (pos_now > 0.0) {
            high_water = Rcpp::NumericVector::is_na(high_water) ? close[i] : std::max(high_water, close[i]);
            if (close[i] <= high_water - trail_mult * atr[i]) {
                pos_now = 0.0;
                high_water = NA_REAL;
                exited_now = true;
            }
        } else if (pos_now < 0.0) {
            low_water = Rcpp::NumericVector::is_na(low_water) ? close[i] : std::min(low_water, close[i]);
            if (close[i] >= low_water + trail_mult * atr[i]) {
                pos_now = 0.0;
                low_water = NA_REAL;
                exited_now = true;
            }
        }

        if (pos_now == 0.0 && !exited_now) {
            if (close[i] >= close[i - 1] + atr_mult * atr[i - 1]) {
                pos_now = target_size;
                high_water = close[i];
                low_water = NA_REAL;
            } else if (close[i] <= close[i - 1] - atr_mult * atr[i - 1]) {
                pos_now = -target_size;
                low_water = close[i];
                high_water = NA_REAL;
            }
        }
        out[i] = pos_now;
    }

    return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector strat_pair_spread_revert_signal_cpp(
    const Rcpp::NumericVector& zscore_value,
    double entry_z = 2.0,
    double exit_z = 0.5,
    double target_size = 1.0
) {
    int n = zscore_value.size();
    Rcpp::NumericVector out(n);
    double pos_now = 0.0;

    for (int i = 0; i < n; ++i) {
        double z = zscore_value[i];
        if (Rcpp::NumericVector::is_na(z)) {
            out[i] = pos_now;
            continue;
        }

        if (pos_now > 0.0 && z >= -exit_z) {
            pos_now = 0.0;
        } else if (pos_now < 0.0 && z <= exit_z) {
            pos_now = 0.0;
        }

        if (pos_now == 0.0) {
            if (z <= -entry_z) {
                pos_now = target_size;
            } else if (z >= entry_z) {
                pos_now = -target_size;
            }
        }
        out[i] = pos_now;
    }

    return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector strat_rsi_divergence_signal_cpp(
    const Rcpp::NumericVector& close,
    const Rcpp::NumericVector& rsi,
    int pivot_left = 2,
    int pivot_right = 2,
    double exit_level = 50.0,
    double target_size = 1.0
) {
    int n = close.size();
    if (rsi.size() != n) {
        Rcpp::stop("All input vectors must have the same length.");
    }
    if (pivot_left < 1 || pivot_right < 1) {
        Rcpp::stop("`pivot_left` and `pivot_right` must be positive.");
    }

    std::vector<bool> price_low(n, false), price_high(n, false), rsi_low(n, false), rsi_high(n, false);
    auto mark_pivots = [&](const Rcpp::NumericVector& x, std::vector<bool>& low, std::vector<bool>& high) {
        if (n < pivot_left + pivot_right + 1) return;
        for (int i = pivot_left; i < n - pivot_right; ++i) {
            double center = x[i];
            if (Rcpp::NumericVector::is_na(center)) continue;
            bool all_finite = true;
            double min_v = center;
            double max_v = center;
            int equal_count = 0;
            for (int j = i - pivot_left; j <= i + pivot_right; ++j) {
                double v = x[j];
                if (Rcpp::NumericVector::is_na(v)) {
                    all_finite = false;
                    break;
                }
                min_v = std::min(min_v, v);
                max_v = std::max(max_v, v);
            }
            if (!all_finite) continue;
            for (int j = i - pivot_left; j <= i + pivot_right; ++j) {
                if (x[j] == center) ++equal_count;
            }
            low[i] = center == min_v && equal_count == 1;
            high[i] = center == max_v && equal_count == 1;
        }
    };

    mark_pivots(close, price_low, price_high);
    mark_pivots(rsi, rsi_low, rsi_high);

    std::vector<bool> bull_signal(n, false), bear_signal(n, false);
    int last_low_idx = -1;
    int last_high_idx = -1;
    for (int i = 0; i < n; ++i) {
        if (price_low[i] && rsi_low[i]) {
            if (last_low_idx >= 0 && close[i] < close[last_low_idx] && rsi[i] > rsi[last_low_idx]) {
                bull_signal[std::min(n - 1, i + pivot_right)] = true;
            }
            last_low_idx = i;
        }
        if (price_high[i] && rsi_high[i]) {
            if (last_high_idx >= 0 && close[i] > close[last_high_idx] && rsi[i] < rsi[last_high_idx]) {
                bear_signal[std::min(n - 1, i + pivot_right)] = true;
            }
            last_high_idx = i;
        }
    }

    Rcpp::NumericVector out(n);
    double pos_now = 0.0;
    for (int i = 0; i < n; ++i) {
        if (Rcpp::NumericVector::is_na(rsi[i])) {
            out[i] = pos_now;
            continue;
        }
        if (pos_now > 0.0 && rsi[i] >= exit_level) {
            pos_now = 0.0;
        } else if (pos_now < 0.0 && rsi[i] <= exit_level) {
            pos_now = 0.0;
        }
        if (pos_now == 0.0) {
            if (bull_signal[i]) {
                pos_now = target_size;
            } else if (bear_signal[i]) {
                pos_now = -target_size;
            }
        }
        out[i] = pos_now;
    }

    return out;
}

// [[Rcpp::export]]
Rcpp::List backtest_portfolio_weights_core_cpp(
    const Rcpp::IntegerVector& date_id,
    const Rcpp::IntegerVector& asset_id,
    const Rcpp::NumericVector& open,
    const Rcpp::NumericVector& close,
    const Rcpp::NumericVector& target_weight,
    const Rcpp::NumericVector& contract_size,
    double initial_equity = 1.0,
    double fee_rt = 0.0,
    double rebalance_tol_weight = 0.0,
    int n_assets = 0
) {
    int n = date_id.size();
    if (asset_id.size() != n || open.size() != n || close.size() != n ||
        target_weight.size() != n || contract_size.size() != n) {
        Rcpp::stop("All input vectors must have the same length.");
    }
    if (n == 0) {
        return Rcpp::List::create(
            Rcpp::Named("date_id") = Rcpp::IntegerVector(),
            Rcpp::Named("equity") = Rcpp::NumericVector(),
            Rcpp::Named("cash") = Rcpp::NumericVector(),
            Rcpp::Named("gross_exposure") = Rcpp::NumericVector(),
            Rcpp::Named("net_exposure") = Rcpp::NumericVector(),
            Rcpp::Named("turnover") = Rcpp::NumericVector(),
            Rcpp::Named("fee_paid") = Rcpp::NumericVector(),
            Rcpp::Named("n_assets") = Rcpp::IntegerVector()
        );
    }
    if (n_assets <= 0) {
        for (int i = 0; i < n; ++i) n_assets = std::max(n_assets, asset_id[i]);
    }

    std::vector<double> units(n_assets + 1, 0.0);
    double cash = initial_equity;
    std::vector<int> out_date_id;
    std::vector<double> out_equity, out_cash, out_gross, out_net, out_turnover, out_fee;
    std::vector<int> out_n_assets;

    int i = 0;
    while (i < n) {
        int d = date_id[i];
        int j = i;
        while (j < n && date_id[j] == d) ++j;

        double open_equity = cash;
        for (int k = i; k < j; ++k) {
            int a = asset_id[k];
            open_equity += units[a] * contract_size[k] * open[k];
        }
        if (!std::isfinite(open_equity) || open_equity <= 0.0) {
            Rcpp::stop("Portfolio equity became non-positive or non-finite.");
        }

        double traded_notional_sum = 0.0;
        std::vector<double> delta_units(j - i, 0.0);
        for (int k = i; k < j; ++k) {
            int a = asset_id[k];
            double current_weight = units[a] * contract_size[k] * open[k] / open_equity;
            double tw = Rcpp::NumericVector::is_na(target_weight[k]) ? 0.0 : target_weight[k];
            if (std::abs(tw - current_weight) > rebalance_tol_weight) {
                double target_notional = tw * open_equity;
                double target_units = target_notional / (contract_size[k] * open[k]);
                delta_units[k - i] = target_units - units[a];
                traded_notional_sum += std::abs(delta_units[k - i]) * contract_size[k] * open[k];
            }
        }

        double fee_paid = traded_notional_sum * fee_rt;
        for (int k = i; k < j; ++k) {
            int a = asset_id[k];
            cash -= delta_units[k - i] * contract_size[k] * open[k];
            units[a] += delta_units[k - i];
        }
        cash -= fee_paid;

        double close_equity = cash;
        double gross_notional = 0.0;
        double net_notional = 0.0;
        for (int k = i; k < j; ++k) {
            int a = asset_id[k];
            double notional = units[a] * contract_size[k] * close[k];
            close_equity += notional;
            gross_notional += std::abs(notional);
            net_notional += notional;
        }

        out_date_id.push_back(d);
        out_equity.push_back(close_equity);
        out_cash.push_back(cash);
        out_gross.push_back(gross_notional / close_equity);
        out_net.push_back(net_notional / close_equity);
        out_turnover.push_back(traded_notional_sum / open_equity);
        out_fee.push_back(fee_paid);
        out_n_assets.push_back(j - i);
        i = j;
    }

    return Rcpp::List::create(
        Rcpp::Named("date_id") = out_date_id,
        Rcpp::Named("equity") = out_equity,
        Rcpp::Named("cash") = out_cash,
        Rcpp::Named("gross_exposure") = out_gross,
        Rcpp::Named("net_exposure") = out_net,
        Rcpp::Named("turnover") = out_turnover,
        Rcpp::Named("fee_paid") = out_fee,
        Rcpp::Named("n_assets") = out_n_assets
    );
}
