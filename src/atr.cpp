#include <Rcpp.h>
using namespace Rcpp;

// atr_cpp uses a smaller smoothing factor 1/n, J. Welles Wilder’s original ATR (1978)
// ATR(HLC, n) will have n NA values in the beginning. it is because when we calculate TR, there is also one NA. so ATR is still based on the current price info, which is only avaialbe in next bar when we are doing backtesting
// [[Rcpp::export]]
NumericVector atr_cpp(NumericVector high, NumericVector low, NumericVector close, int n) {
  int len = high.size();
  if (len != low.size() || len != close.size()) {
    stop("Input vectors must be the same length.");
  }
  if (len < n || n <= 0) {
    return NumericVector(len, NA_REAL);
  }

  NumericVector tr(len, NA_REAL);
  for (int i = 1; i < len; ++i) {
    double high_low = high[i] - low[i];
    double high_close = std::abs(high[i] - close[i - 1]);
    double low_close = std::abs(low[i] - close[i - 1]);
    tr[i] = std::max({high_low, high_close, low_close});
  }

  NumericVector atr(len, NA_REAL);
  double sum = 0.0;
  for (int i = 1; i <= n; ++i) {
    sum += tr[i];  // Skip tr[0] since it is NA
  }
  atr[n] = sum / n;

  // Wilder smoothing: alpha = 1/n
  for (int i = n + 1; i < len; ++i) {
    atr[i] = ((atr[i - 1] * (n - 1)) + tr[i]) / n;
  }

  return atr;
}

// atr_cpp2 uses a larger smoothing factor 2/(n + 1), more responsive to recent TRs
// [[Rcpp::export]]
NumericVector atr_cpp2(NumericVector high, NumericVector low, NumericVector close, int n) {
  int len = high.size();
  if (len != low.size() || len != close.size()) {
    stop("Input vectors must be the same length.");
  }
  if (len < n || n <= 0) {
    return NumericVector(len, NA_REAL);
  }

  NumericVector tr(len, NA_REAL);
  for (int i = 1; i < len; ++i) {
    double high_low = high[i] - low[i];
    double high_close = std::abs(high[i] - close[i - 1]);
    double low_close = std::abs(low[i] - close[i - 1]);
    tr[i] = std::max({high_low, high_close, low_close});
  }

  // Now apply EMA on tr
  NumericVector atr(len, NA_REAL);
  double sum = 0.0;
  for (int i = 1; i <= n; ++i) {
    sum += tr[i];  // tr[0] is NA, start from tr[1]
  }
  atr[n] = sum / n;

  double alpha = 2.0 / (n + 1.0);
  for (int i = n + 1; i < len; ++i) {
    atr[i] = alpha * tr[i] + (1 - alpha) * atr[i - 1];
  }

  return atr;
}
