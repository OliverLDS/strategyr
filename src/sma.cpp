#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector sma_cpp(NumericVector x, int n) {
  int len = x.size();
  NumericVector sma(len, NA_REAL);

  if (n > len || n <= 0) return sma;

  double sum = 0.0;

  for (int i = 0; i < len; ++i) {
    sum += x[i];
    if (i >= n) {
      sum -= x[i - n];
    }
    if (i >= n - 1) {
      sma[i] = sum / n;
    }
  }

  return sma;
}
