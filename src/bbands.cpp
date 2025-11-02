#include <Rcpp.h>
using namespace Rcpp;

// This version aligns with TTR, which uses population sd.
// [[Rcpp::export]]
List bbands_cpp(NumericVector x, int n = 20, double sd = 2.0) {
  int len = x.size();
  NumericVector ma(len, NA_REAL);
  NumericVector upper(len, NA_REAL);
  NumericVector lower(len, NA_REAL);
  NumericVector stddev(len, NA_REAL);
  NumericVector pctB(len, NA_REAL);

  if (n > len || n <= 1) return List::create(
    _["dn"] = lower,
    _["mavg"] = ma,
    _["up"] = upper,
    _["pctB"] = pctB
  );

  for (int i = n - 1; i < len; ++i) {
    double sum = 0.0;
    for (int j = i - n + 1; j <= i; ++j) sum += x[j];
    double mean = sum / n;
    ma[i] = mean;

    // Population standard deviation (denominator = n)
    double sq_sum = 0.0;
    for (int j = i - n + 1; j <= i; ++j) {
      double diff = x[j] - mean;
      sq_sum += diff * diff;
    }
    double sd_val = std::sqrt(sq_sum / n);
    stddev[i] = sd_val;

    upper[i] = mean + sd * sd_val;
    lower[i] = mean - sd * sd_val;

    if ((upper[i] - lower[i]) != 0) {
      pctB[i] = (x[i] - lower[i]) / (upper[i] - lower[i]);
    }
  }

  return List::create(
    _["dn"] = lower,
    _["mavg"] = ma,
    _["up"] = upper,
    _["pctB"] = pctB
  );
}

// Sample-size adjusted SD
// [[Rcpp::export]]
List bbands_cpp2(NumericVector x, int n = 20, double sd = 2.0) {
  int len = x.size();
  NumericVector ma(len, NA_REAL);
  NumericVector upper(len, NA_REAL);
  NumericVector lower(len, NA_REAL);
  NumericVector stddev(len, NA_REAL);

  if (n > len || n <= 1) return List::create(
    _["dn"] = lower,
    _["mavg"] = ma,
    _["up"] = upper,
    _["pctB"] = NumericVector(len, NA_REAL)
  );

  for (int i = n - 1; i < len; ++i) {
    double sum = 0.0;
    for (int j = i - n + 1; j <= i; ++j) {
      sum += x[j];
    }
    double mean = sum / n;
    ma[i] = mean;

    // std deviation
    double sq_sum = 0.0;
    for (int j = i - n + 1; j <= i; ++j) {
      sq_sum += (x[j] - mean) * (x[j] - mean);
    }
    double sd_val = std::sqrt(sq_sum / (n - 1));
    stddev[i] = sd_val;

    upper[i] = mean + sd * sd_val;
    lower[i] = mean - sd * sd_val;
  }

  // %B indicator
  NumericVector pctB(len, NA_REAL);
  for (int i = 0; i < len; ++i) {
    if (!NumericVector::is_na(upper[i]) && !NumericVector::is_na(lower[i]) && (upper[i] != lower[i])) {
      pctB[i] = (x[i] - lower[i]) / (upper[i] - lower[i]);
    }
  }

  return List::create(
    _["dn"] = lower,
    _["mavg"] = ma,
    _["up"] = upper,
    _["pctB"] = pctB
  );
}
