#include <Rcpp.h>
#include <limits>
#include <algorithm>
#include <cmath>
using namespace Rcpp;

// with NA values in the beginning, aligning with default TTR::EMA
// [[Rcpp::export]]
NumericVector ema_cpp(NumericVector x, int n) { 
  int len = x.size();
  NumericVector ema(len, NA_REAL);
  if (len < n || n <= 0) return ema;

  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    sum += x[i];
  }
  ema[n - 1] = sum / n;

  double alpha = 2.0 / (n + 1.0);
  for (int i = n; i < len; ++i) {
    ema[i] = alpha * x[i] + (1 - alpha) * ema[i - 1];
  }

  return ema;
}
  
// no NA values in the beginning of the series
// [[Rcpp::export]]
NumericVector ema_cpp2(NumericVector x, int n) {
  int len = x.size();
  NumericVector ema(len);
  if (len == 0 || n <= 0) return ema;

  double alpha = 2.0 / (n + 1.0);
  ema[0] = x[0];  // Initialize with first value

  for (int i = 1; i < len; ++i) {
    ema[i] = alpha * x[i] + (1 - alpha) * ema[i - 1];
  }

  return ema;
}


// Utility: sigmoid function
inline double sigmoid(double z) {
  return 1.0 / (1.0 + std::exp(-z));
}

// all rcpp functions should only use past information, so they can also be used in live trading
// [[Rcpp::export]]
DataFrame ema_metrics_cpp(NumericVector ema_fast,
                          NumericVector ema_slow,
                          int slope_window = 3,
                          int rolling_window = 200,
                          double eps = 1e-6) {

  int n = ema_fast.size();
  IntegerVector trend_signal(n);
  IntegerVector bars_since_cross(n);
  IntegerVector cycle_id(n, NA_INTEGER);
  NumericVector raw_score(n);
  NumericVector confidence_score(n);

  int last_cross = NA_INTEGER;
  int cycle_counter = 0;

  for (int i = 0; i < n; ++i) {
    double fast = ema_fast[i];
    double slow = ema_slow[i];

    int signal = NA_INTEGER;
    if (!NumericVector::is_na(fast) && !NumericVector::is_na(slow)) {
      if (fast > slow) signal = 1;
      else if (fast < slow) signal = -1;
      else signal = 0;
    }
    trend_signal[i] = signal;

    if (i > 0) {
      double fast_prev = ema_fast[i - 1];
      double slow_prev = ema_slow[i - 1];
      bool crossed_up   = (fast_prev < slow_prev) && (fast >= slow);
      bool crossed_down = (fast_prev > slow_prev) && (fast <= slow);

      if (crossed_up || crossed_down) {
        last_cross = i;
        bars_since_cross[i] = 0;
        ++cycle_counter;
        cycle_id[i] = cycle_counter;
      } else if (last_cross != NA_INTEGER) {
        if (!NumericVector::is_na(fast) && !NumericVector::is_na(slow))
          bars_since_cross[i] = i - last_cross;
        else
          bars_since_cross[i] = NA_INTEGER;
        cycle_id[i] = cycle_counter;
      } else {
        bars_since_cross[i] = NA_INTEGER;
      }
    } else { // i == 0
      bars_since_cross[i] = NA_INTEGER;
    }

    // raw slope-diff / distance score
    if (i >= slope_window && !NumericVector::is_na(fast) && !NumericVector::is_na(slow)) {
      double d_fast = ema_fast[i] - ema_fast[i - slope_window];
      double d_slow = ema_slow[i] - ema_slow[i - slope_window];
      double dist = std::abs(fast - slow);
      raw_score[i] = (d_fast - d_slow) / (dist + eps);
    } else {
      raw_score[i] = NA_REAL;
    }
  }

  // rolling z-score -> sigmoid (O(n·W) baseline)
  for (int i = 0; i < n; ++i) {
    if (NumericVector::is_na(raw_score[i])) { confidence_score[i] = NA_REAL; continue; }
    int start = std::max(0, i - rolling_window + 1);
    int count = 0; double sum = 0.0, sum_sq = 0.0;
    for (int j = start; j <= i; ++j) {
      double v = raw_score[j];
      if (!NumericVector::is_na(v)) { sum += v; sum_sq += v*v; ++count; }
    }
    if (count >= 5) {
      double mean = sum / count;
      double var  = (sum_sq / count) - mean*mean;
      double sd   = std::sqrt(std::max(var, eps));
      double z    = (raw_score[i] - mean) / sd;
      confidence_score[i] = 1.0 / (1.0 + std::exp(-z)); // sigmoid
    } else {
      confidence_score[i] = NA_REAL;
    }
  }

  return DataFrame::create(
    Named("trend_signal")         = trend_signal,
    Named("bars_since_cross")     = bars_since_cross,
    Named("raw_confidence_score") = raw_score,
    Named("confidence_score")     = confidence_score,
    Named("cycle_id")             = cycle_id
  );
}

