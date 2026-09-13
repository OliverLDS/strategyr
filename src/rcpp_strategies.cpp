// rcpp_strategies.cpp

#include <Rcpp.h>
#include <algorithm>
#include <numeric>

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

// [[Rcpp::export]]
Rcpp::List strat_portfolio_daily_backtest_core_cpp(
    const Rcpp::IntegerVector& date_id,
    const Rcpp::IntegerVector& asset_id,
    const Rcpp::NumericVector& open,
    const Rcpp::NumericVector& close,
    const Rcpp::NumericVector& target_weight,
    const Rcpp::LogicalVector& rebalance_eligible,
    int n_dates,
    int n_assets,
    double initial_cash,
    double fee_rt,
    double rebalance_tolerance
) {
    const int n_rows = date_id.size();
    if (asset_id.size() != n_rows || open.size() != n_rows || close.size() != n_rows ||
        target_weight.size() != n_rows || rebalance_eligible.size() != n_rows) {
        Rcpp::stop("All portfolio backtest input vectors must have the same length.");
    }
    if (n_dates <= 0 || n_assets <= 0) {
        Rcpp::stop("`n_dates` and `n_assets` must be positive.");
    }

    Rcpp::NumericVector equity(n_dates), cash_out(n_dates), cash_weight(n_dates);
    Rcpp::NumericVector gross_exposure(n_dates), net_exposure(n_dates), turnover(n_dates), fee_paid(n_dates);
    Rcpp::IntegerVector available_assets(n_dates), unavailable_assets(n_dates), traded_assets(n_dates);
    Rcpp::IntegerVector rebalance_due(n_dates);
    Rcpp::NumericVector traded_notional(n_dates), buy_scale(n_dates);

    const int n_output = n_dates * n_assets;
    Rcpp::NumericVector target_out(n_output), realized_weight(n_output);
    Rcpp::NumericVector units_out(n_output), valuation_price(n_output);
    Rcpp::IntegerVector available_out(n_output), stale_valuation(n_output), rebalance_out(n_output);

    std::vector<double> units(n_assets, 0.0), last_close(n_assets, NA_REAL);
    std::vector<double> open_day(n_assets, NA_REAL), close_day(n_assets, NA_REAL), target_day(n_assets, NA_REAL);
    std::vector<int> eligible_day(n_assets, 0), available_day(n_assets, 0);
    std::vector<double> valuation_open(n_assets, NA_REAL), current_weight(n_assets, 0.0), delta_units(n_assets, 0.0);
    double cash = initial_cash;
    int row_start = 0;

    for (int day = 0; day < n_dates; ++day) {
        int row_end = row_start;
        while (row_end < n_rows && date_id[row_end] == day + 1) {
            ++row_end;
        }
        if (row_start == row_end) {
            Rcpp::stop("Every date must have at least one available asset row.");
        }

        std::fill(open_day.begin(), open_day.end(), NA_REAL);
        std::fill(close_day.begin(), close_day.end(), NA_REAL);
        std::fill(target_day.begin(), target_day.end(), NA_REAL);
        std::fill(eligible_day.begin(), eligible_day.end(), 0);
        std::fill(available_day.begin(), available_day.end(), 0);
        for (int row = row_start; row < row_end; ++row) {
            const int asset = asset_id[row] - 1;
            if (asset < 0 || asset >= n_assets) Rcpp::stop("Invalid asset identifier.");
            open_day[asset] = open[row];
            close_day[asset] = close[row];
            target_day[asset] = target_weight[row];
            eligible_day[asset] = rebalance_eligible[row] == TRUE;
            available_day[asset] = 1;
        }

        double open_equity = cash;
        for (int asset = 0; asset < n_assets; ++asset) {
            valuation_open[asset] = available_day[asset] ? open_day[asset] : last_close[asset];
            if (units[asset] != 0.0 && !R_finite(valuation_open[asset])) {
                Rcpp::stop("A held asset has no available valuation price.");
            }
            if (R_finite(valuation_open[asset])) {
                open_equity += units[asset] * valuation_open[asset];
            }
        }
        if (!R_finite(open_equity) || open_equity <= 0.0) {
            Rcpp::stop("Portfolio equity became non-positive or non-finite.");
        }

        std::fill(current_weight.begin(), current_weight.end(), 0.0);
        std::fill(delta_units.begin(), delta_units.end(), 0.0);
        for (int asset = 0; asset < n_assets; ++asset) {
            if (R_finite(valuation_open[asset])) {
                current_weight[asset] = units[asset] * valuation_open[asset] / open_equity;
            }
            if (available_day[asset] && eligible_day[asset] &&
                std::abs(target_day[asset] - current_weight[asset]) > rebalance_tolerance) {
                delta_units[asset] = target_day[asset] * open_equity / open_day[asset] - units[asset];
            }
        }

        double sell_notional = 0.0;
        for (int asset = 0; asset < n_assets; ++asset) {
            if (delta_units[asset] < 0.0) sell_notional += -delta_units[asset] * open_day[asset];
        }
        const double cash_after_sells = cash + sell_notional - sell_notional * fee_rt;
        double planned_buy_notional = 0.0;
        for (int asset = 0; asset < n_assets; ++asset) {
            if (delta_units[asset] > 0.0) planned_buy_notional += delta_units[asset] * open_day[asset];
        }
        const double scale = planned_buy_notional > 0.0 ?
            std::min(1.0, std::max(0.0, cash_after_sells / (planned_buy_notional * (1.0 + fee_rt)))) : 1.0;
        for (int asset = 0; asset < n_assets; ++asset) {
            if (delta_units[asset] > 0.0) delta_units[asset] *= scale;
        }

        double traded = 0.0;
        int traded_count = 0;
        for (int asset = 0; asset < n_assets; ++asset) {
            traded += std::abs(delta_units[asset]) * (available_day[asset] ? open_day[asset] : 0.0);
            if (std::abs(delta_units[asset]) > 0.0) ++traded_count;
            cash -= delta_units[asset] * (available_day[asset] ? open_day[asset] : 0.0);
            units[asset] += delta_units[asset];
        }
        const double fees = traded * fee_rt;
        cash -= fees;

        for (int asset = 0; asset < n_assets; ++asset) {
            if (available_day[asset]) last_close[asset] = close_day[asset];
            if (units[asset] != 0.0 && !R_finite(last_close[asset])) {
                Rcpp::stop("A held asset has no close valuation price.");
            }
        }
        double close_equity = cash, gross = 0.0, net = 0.0;
        for (int asset = 0; asset < n_assets; ++asset) {
            if (R_finite(last_close[asset])) {
                const double notional = units[asset] * last_close[asset];
                close_equity += notional;
                gross += std::abs(notional);
                net += notional;
            }
        }
        if (!R_finite(close_equity) || close_equity <= 0.0) {
            Rcpp::stop("Portfolio equity became non-positive or non-finite.");
        }

        equity[day] = close_equity;
        cash_out[day] = cash;
        cash_weight[day] = cash / close_equity;
        gross_exposure[day] = gross / close_equity;
        net_exposure[day] = net / close_equity;
        turnover[day] = traded / open_equity;
        fee_paid[day] = fees;
        available_assets[day] = std::accumulate(available_day.begin(), available_day.end(), 0);
        unavailable_assets[day] = n_assets - available_assets[day];
        traded_assets[day] = traded_count;
        rebalance_due[day] = std::any_of(eligible_day.begin(), eligible_day.end(), [](int x) { return x != 0; });
        traded_notional[day] = traded;
        buy_scale[day] = scale;

        for (int asset = 0; asset < n_assets; ++asset) {
            const int output = day * n_assets + asset;
            available_out[output] = available_day[asset];
            stale_valuation[output] = !available_day[asset] && R_finite(last_close[asset]);
            rebalance_out[output] = available_day[asset] && eligible_day[asset];
            target_out[output] = available_day[asset] ? target_day[asset] : NA_REAL;
            realized_weight[output] = R_finite(last_close[asset]) ? units[asset] * last_close[asset] / close_equity : 0.0;
            units_out[output] = units[asset];
            valuation_price[output] = last_close[asset];
        }
        row_start = row_end;
    }
    if (row_start != n_rows) Rcpp::stop("Input rows must be ordered by date identifier.");

    return Rcpp::List::create(
        Rcpp::Named("equity") = equity,
        Rcpp::Named("cash") = cash_out,
        Rcpp::Named("cash_weight") = cash_weight,
        Rcpp::Named("gross_exposure") = gross_exposure,
        Rcpp::Named("net_exposure") = net_exposure,
        Rcpp::Named("turnover") = turnover,
        Rcpp::Named("fee_paid") = fee_paid,
        Rcpp::Named("available_assets") = available_assets,
        Rcpp::Named("unavailable_assets") = unavailable_assets,
        Rcpp::Named("rebalance_due") = rebalance_due,
        Rcpp::Named("traded_assets") = traded_assets,
        Rcpp::Named("traded_notional") = traded_notional,
        Rcpp::Named("buy_scale") = buy_scale,
        Rcpp::Named("target_weight") = target_out,
        Rcpp::Named("realized_weight") = realized_weight,
        Rcpp::Named("units") = units_out,
        Rcpp::Named("valuation_price") = valuation_price,
        Rcpp::Named("available") = available_out,
        Rcpp::Named("stale_valuation") = stale_valuation,
        Rcpp::Named("rebalance_eligible") = rebalance_out
    );
}
