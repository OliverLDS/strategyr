#include <Rcpp.h>
#include <algorithm> // for std::sort
using namespace Rcpp;

// Helper: compute quantile from sorted vector
double get_quantile(std::vector<double> sorted_vals, double prob) {
  int n = sorted_vals.size();
  if (n == 0) return NA_REAL;

  std::sort(sorted_vals.begin(), sorted_vals.end());
  double index = prob * (n - 1);
  int lo = std::floor(index);
  int hi = std::ceil(index);
  double w = index - lo;

  if (hi >= n) return sorted_vals[lo];
  return (1.0 - w) * sorted_vals[lo] + w * sorted_vals[hi];
}

// [[Rcpp::export]]
DataFrame zones_quantile_fixed_cpp(NumericVector high, NumericVector low, int window = 20) {
  int n = high.size();
  NumericVector res_str(n, NA_REAL), res_weak(n, NA_REAL);
  NumericVector sup_str(n, NA_REAL), sup_weak(n, NA_REAL);

  for (int i = window - 1; i < n; ++i) {
    std::vector<double> hi_win, lo_win;
    hi_win.reserve(window);
    lo_win.reserve(window);

    for (int j = i - window + 1; j <= i; ++j) {
      if (!NumericVector::is_na(high[j])) hi_win.push_back(high[j]);
      if (!NumericVector::is_na(low[j]))  lo_win.push_back(low[j]);
    }

    if (hi_win.size() > 0) {
      res_str[i]  = get_quantile(hi_win, 0.90);
      res_weak[i] = get_quantile(hi_win, 0.80);
    }

    if (lo_win.size() > 0) {
      sup_str[i]  = get_quantile(lo_win, 0.10);
      sup_weak[i] = get_quantile(lo_win, 0.20);
    }
  }

  return DataFrame::create(
    _["support_strong_qt_f"]     = sup_str,
    _["support_weak_qt_f"]       = sup_weak,
    _["resistance_weak_qt_f"]    = res_weak,
    _["resistance_strong_qt_f"]  = res_str
  );
}

// [[Rcpp::export]]
DataFrame zones_quantile_dyn_cpp(NumericVector high,
                                      NumericVector low,
                                      NumericVector close,
                                      NumericVector atr14,
                                      double mom_threshold = 0.015,
                                      int min_window = 20,
                                      int max_window = 120) {
  int n = high.size();

  NumericVector res_str(n, NA_REAL), res_weak(n, NA_REAL);
  NumericVector sup_str(n, NA_REAL), sup_weak(n, NA_REAL);

  int win_start = -1; // invalid initially

  for (int i = 0; i < n; ++i) {
    bool weak_now = !NumericVector::is_na(atr14[i]) &&
                    !NumericVector::is_na(close[i]) &&
                    (atr14[i] / close[i]) < mom_threshold;

    if (weak_now) {
      if (win_start == -1) win_start = i;

      int left = std::max(win_start, i - max_window + 1);
      int wlen = i - left + 1;

      if (wlen >= min_window) {
        std::vector<double> hi_win, lo_win;
        hi_win.reserve(wlen);
        lo_win.reserve(wlen);

        for (int j = left; j <= i; ++j) {
          if (!NumericVector::is_na(high[j])) hi_win.push_back(high[j]);
          if (!NumericVector::is_na(low[j]))  lo_win.push_back(low[j]);
        }

        if (!hi_win.empty()) {
          std::sort(hi_win.begin(), hi_win.end());
          double q90 = hi_win[(int)((hi_win.size() - 1) * 0.90)];
          double q80 = hi_win[(int)((hi_win.size() - 1) * 0.80)];
          res_str[i] = q90;
          res_weak[i] = q80;
        }

        if (!lo_win.empty()) {
          std::sort(lo_win.begin(), lo_win.end());
          double q10 = lo_win[(int)((lo_win.size() - 1) * 0.10)];
          double q20 = lo_win[(int)((lo_win.size() - 1) * 0.20)];
          sup_str[i] = q10;
          sup_weak[i] = q20;
        }
      }
    } else {
      win_start = -1;

      // carry previous zone values forward
      if (i > 0) {
        sup_str[i]  = sup_str[i - 1];
        sup_weak[i] = sup_weak[i - 1];
        res_weak[i] = res_weak[i - 1];
        res_str[i]  = res_str[i - 1];
      }
    }
  }

  return DataFrame::create(
    _["support_strong_qt_d"]     = sup_str,
    _["support_weak_qt_d"]       = sup_weak,
    _["resistance_weak_qt_d"]    = res_weak,
    _["resistance_strong_qt_d"]  = res_str
  );
}
