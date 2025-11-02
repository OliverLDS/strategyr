#include <Rcpp.h>
#include <algorithm>
#include <cmath>
using namespace Rcpp;

// Helper: check if center is swing high/low in span window
bool is_swing_high(const NumericVector& high, int i, int span) {
  for (int j = i - span; j <= i + span; ++j) {
    if (j == i) continue;
    if (high[i] <= high[j]) return false;
  }
  return true;
}

bool is_swing_low(const NumericVector& low, int i, int span) {
  for (int j = i - span; j <= i + span; ++j) {
    if (j == i) continue;
    if (low[i] >= low[j]) return false;
  }
  return true;
}

// [[Rcpp::export]]
DataFrame pivots_cpp(NumericVector high,
                     NumericVector low,
                     int span = 5) {
						 
  // output vectors
  IntegerVector idx;
  NumericVector pivot_price;
  CharacterVector type;
  
  int n = high.size();
  
  for (int i = span; i < n - span; ++i) {
    if (!NumericVector::is_na(high[i]) && is_swing_high(high, i, span)) {
      idx.push_back(i + 1); // 1-based index for R
      pivot_price.push_back(high[i]);
      type.push_back("H");
    }
    if (!NumericVector::is_na(low[i]) && is_swing_low(low, i, span)) {
      idx.push_back(i + 1);
      pivot_price.push_back(low[i]);
      type.push_back("L");
    }
  }

  return DataFrame::create(
    _["idx"]   = idx,
    _["price"] = pivot_price,
    _["type"]  = type
  );
}


// [[Rcpp::export]]
DataFrame zones_pivots_cpp(NumericVector high,
                                NumericVector low,
                                NumericVector atr,
                                int span = 2,
                                int k = 6,
                                double tol_mult = 0.15) {
  int n = high.size();

  LogicalVector is_piv_hi(n, false), is_piv_lo(n, false);
  NumericVector sup1(n, NA_REAL), sup2(n, NA_REAL), res1(n, NA_REAL), res2(n, NA_REAL);
  IntegerVector sup1_sc(n, NA_INTEGER), sup2_sc(n, NA_INTEGER), res1_sc(n, NA_INTEGER), res2_sc(n, NA_INTEGER);

  // Step 1: mark pivot points
  for (int i = span; i < n - span; ++i) {
    if (!NumericVector::is_na(high[i]) && is_swing_high(high, i, span))
      is_piv_hi[i] = true;
    if (!NumericVector::is_na(low[i]) && is_swing_low(low, i, span))
      is_piv_lo[i] = true;
  }

  // Step 2: loop through each bar and build zones
  for (int i = 0; i < n; ++i) {
    double tol = NumericVector::is_na(atr[i]) ? 0.0 : atr[i] * tol_mult;

    // --- Resistance ---
    std::vector<double> last_k_hi;
    for (int j = i; j >= 0 && (int)last_k_hi.size() < k; --j) {
      if (is_piv_hi[j] && !NumericVector::is_na(high[j]))
        last_k_hi.push_back(high[j]);
    }
    if (!last_k_hi.empty()) {
      std::sort(last_k_hi.begin(), last_k_hi.end(), std::greater<double>());

      std::vector<double> hi_lvls;
      for (double val : last_k_hi) {
        if (hi_lvls.empty() || std::abs(val - hi_lvls.back()) > tol)
          hi_lvls.push_back(val);
      }

      std::vector<int> scores(hi_lvls.size(), 0);
      for (size_t j = 0; j < hi_lvls.size(); ++j) {
        for (double v : last_k_hi) {
          if (std::abs(v - hi_lvls[j]) <= tol) scores[j]++;
        }
      }

      res1[i] = hi_lvls[0];
      res1_sc[i] = scores[0];
      if (hi_lvls.size() > 1) {
        res2[i] = hi_lvls[1];
        res2_sc[i] = scores[1];
      } else {
        res2[i] = hi_lvls[0];
        res2_sc[i] = scores[0];
      }
    }

    // --- Support ---
    std::vector<double> last_k_lo;
    for (int j = i; j >= 0 && (int)last_k_lo.size() < k; --j) {
      if (is_piv_lo[j] && !NumericVector::is_na(low[j]))
        last_k_lo.push_back(low[j]);
    }
    if (!last_k_lo.empty()) {
      std::sort(last_k_lo.begin(), last_k_lo.end());

      std::vector<double> lo_lvls;
      for (double val : last_k_lo) {
        if (lo_lvls.empty() || std::abs(val - lo_lvls.back()) > tol)
          lo_lvls.push_back(val);
      }

      std::vector<int> scores(lo_lvls.size(), 0);
      for (size_t j = 0; j < lo_lvls.size(); ++j) {
        for (double v : last_k_lo) {
          if (std::abs(v - lo_lvls[j]) <= tol) scores[j]++;
        }
      }

      sup1[i] = lo_lvls[0];
      sup1_sc[i] = scores[0];
      if (lo_lvls.size() > 1) {
        sup2[i] = lo_lvls[1];
        sup2_sc[i] = scores[1];
      } else {
        sup2[i] = lo_lvls[0];
        sup2_sc[i] = scores[0];
      }
    }
  }

  return DataFrame::create(
    _["support_1"] = sup1,
    _["support_2"] = sup2,
    _["resistance_1"] = res1,
    _["resistance_2"] = res2,
    _["support_score_1"] = sup1_sc,
    _["support_score_2"] = sup2_sc,
    _["resistance_score_1"] = res1_sc,
    _["resistance_score_2"] = res2_sc
  );
}
