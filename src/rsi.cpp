#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector rsi_cpp(NumericVector price, int n = 14) {
  int len = price.size();
  NumericVector rsi(len, NA_REAL);
  NumericVector up(len, 0.0);
  NumericVector down(len, 0.0);
  
  // Compute momentum
  for (int i = 1; i < len; ++i) {
    double diff = price[i] - price[i - 1];
    up[i] = diff > 0 ? diff : 0;
    down[i] = diff < 0 ? -diff : 0;
  }

  // Initial averages
  double avgUp = 0.0, avgDown = 0.0;
  for (int i = 1; i <= n; ++i) {
    avgUp += up[i];
    avgDown += down[i];
  }
  avgUp /= n;
  avgDown /= n;

  // First RSI value
  if (n < len) {
    double rs = avgDown == 0 ? NA_REAL : avgUp / avgDown;
    rsi[n] = R_IsNA(rs) ? 100.0 : 100.0 - (100.0 / (1.0 + rs));
  }

  // Wilder's smoothing for EMA
  for (int i = n + 1; i < len; ++i) {
    avgUp = ((avgUp * (n - 1)) + up[i]) / n;
    avgDown = ((avgDown * (n - 1)) + down[i]) / n;
    double rs = avgDown == 0 ? NA_REAL : avgUp / avgDown;
    rsi[i] = R_IsNA(rs) ? 100.0 : 100.0 - (100.0 / (1.0 + rs));
  }

  return rsi;
}
