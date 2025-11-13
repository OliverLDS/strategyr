// [[Rcpp::depends(Rcpp)]]
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
DataFrame pivots_cpp_slow(NumericVector high, NumericVector low, int span = 5) { 
	int n = high.size(); 
	LogicalVector is_pivot_high(n, false); 
	LogicalVector is_pivot_low(n, false); 
	for (int i = span; i < n - span; ++i) { 
		if (!NumericVector::is_na(high[i]) && is_swing_high(high, i, span)) is_pivot_high[i] = true; 
		if (!NumericVector::is_na(low[i]) && is_swing_low(low, i, span)) is_pivot_low[i] = true; 
	} // output vectors 
	IntegerVector idx; 
	NumericVector pivot_price; 
	CharacterVector type; 
	for (int i = 0; i < n; i++) { 
		if (is_pivot_high[i]) { 
			idx.push_back(i + 1); // 1-based index for R 
			pivot_price.push_back(high[i]); 
			type.push_back("H"); 
		} if (is_pivot_low[i]) { 
			idx.push_back(i + 1); 
			pivot_price.push_back(low[i]); 
			type.push_back("L"); 
		} 
	} 
	return DataFrame::create( _["idx"] = idx, _["price"] = pivot_price, _["type"] = type ); 
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

// helper: make run-id like data.table::rleid(type)
inline IntegerVector rleid_char(const CharacterVector &x) {
  int n = x.size();
  IntegerVector rid(n);
  if(n == 0) return rid;
  int cur = 1;
  rid[0] = cur;
  for(int i=1;i<n;++i){
    if (x[i] != x[i-1]) ++cur;
    rid[i] = cur;
  }
  return rid;
}

// helper: absolute swing from previous: abs(prev/cur - 1)
inline NumericVector swing_from_prev(const NumericVector &price) {
  int n = price.size();
  NumericVector out(n, NA_REAL);
  for (int i=1;i<n;++i) {
    double prev = price[i-1], cur = price[i];
    if (R_finite(prev) && R_finite(cur) && cur != 0.0) {
      out[i] = std::fabs(prev/cur - 1.0);
    }
  }
  return out;
}

// carry POSIXct class/tzone from a reference vector
inline void copy_posixct_attrs(NumericVector &target, const NumericVector &ref) {
  SEXP cls = ref.attr("class");
  if (!Rf_isNull(cls)) target.attr("class") = cls;
  SEXP tz = ref.attr("tzone");
  if (!Rf_isNull(tz)) target.attr("tzone") = tz;
}


// [[Rcpp::export]]
DataFrame get_now_pivots_cpp(const NumericVector &high,
                             const NumericVector &low,
                             const NumericVector &datetime,
                             int span = 3,
                             Rcpp::Nullable<int> latest_n = R_NilValue,
                             bool refined = true,
                             double min_swing = 0.05) {
  // 1) call existing R function pivots_cpp(high, low, span)
  // Function pivots_cpp("pivots_cpp");
  // List piv = pivots_cpp(_["high"]=high, _["low"]=low, _["span"]=span);
  // DataFrame pivDF(piv);
  DataFrame pivDF = pivots_cpp(high, low, span);

  IntegerVector idx = pivDF["idx"];          // 1-based indices into DT
  CharacterVector type = pivDF["type"];      // "H" or "L"
  NumericVector price = pivDF["price"];

  int n = idx.size();
  if (n == 0) return pivDF; // nothing to do

  // 2) optional tail
  if (latest_n.isNotNull()) {
    int k = Rcpp::as<int>(latest_n);
    if (k < n) {
      int start = n - k; // 0-based
      idx   = idx[Range(start, n-1)];
      type  = type[Range(start, n-1)];
      price = price[Range(start, n-1)];
      n = k;
    }
  }

  // 3) attach datetime by index
  NumericVector dt_out(n);
  for (int i=0;i<n;++i) {
    int j = idx[i] - 1; // 1-based -> 0-based
    if (j >= 0 && j < datetime.size()) dt_out[i] = datetime[j];
    else dt_out[i] = NA_REAL;
  }
  copy_posixct_attrs(dt_out, datetime);

  // if not refined, return as-is (+ datetime)
  if (!refined) {
    return DataFrame::create(
      _["idx"] = idx,
      _["type"] = type,
      _["price"] = price,
      _["datetime"] = dt_out,
      _["stringsAsFactors"] = false
    );
  }

  // ---- refined mode ----
  // a) collapse each rleid(type) run into single extreme:
  IntegerVector rid = rleid_char(type);
  // count groups
  int gmax = 0;
  for (int i=0;i<rid.size();++i) if (rid[i] > gmax) gmax = rid[i];

  std::vector<int> keep_ix; keep_ix.reserve(gmax);
  for (int g=1; g<=gmax; ++g) {
    // find slice of group g
    int start=-1, end=-1;
    for (int i=0;i<n;++i) {
      if (rid[i]==g) { start = i; break; }
    }
    for (int i=n-1;i>=0;--i) {
      if (rid[i]==g) { end = i; break; }
    }
    if (start < 0 || end < 0) continue;

    // pick max for "H", min for "L"
    int best = start;
    if (type[start] == "H") {
      double bestp = price[start];
      for (int i=start+1;i<=end;++i) if (price[i] > bestp) { bestp=price[i]; best=i; }
    } else { // "L"
      double bestp = price[start];
      for (int i=start+1;i<=end;++i) if (price[i] < bestp) { bestp=price[i]; best=i; }
    }
    keep_ix.push_back(best);
  }

  // build collapsed vectors in order
  int m = (int)keep_ix.size();
  IntegerVector idx2(m);
  CharacterVector type2(m);
  NumericVector price2(m);
  NumericVector dt2(m);
  for (int i=0;i<m;++i) {
    int j = keep_ix[i];
    idx2[i] = idx[j];
    type2[i] = type[j];
    price2[i] = price[j];
    dt2[i] = dt_out[j];
  }
  copy_posixct_attrs(dt2, datetime);

  // b) iterative removal of too-small swings
  NumericVector swing = swing_from_prev(price2);
  bool remove_latest = false;
  if (m >= 1) {
    double lastSwing = swing[m-1];
    if (R_finite(lastSwing) && lastSwing < min_swing) remove_latest = true;
  }
  // first, second, last set to Inf
  if (m >= 1) swing[0] = R_PosInf;
  if (m >= 2) swing[1] = R_PosInf;
  if (m >= 1) swing[m-1] = R_PosInf;

  // loop: drop i-1 and i where swing[i] is min and < min_swing
  auto recompute_swing = [&](NumericVector &p) {
    NumericVector s = swing_from_prev(p);
    int mm = p.size();
    if (mm >= 1) s[0] = R_PosInf;
    if (mm >= 2) s[1] = R_PosInf;
    if (mm >= 1) s[mm-1] = R_PosInf;
    return s;
  };

  while (true) {
    // find minimal swing value
    int mi = -1;
    double best = R_PosInf;
    for (int i=0;i<swing.size();++i) {
      double v = swing[i];
      if (R_finite(v) && v < best) { best = v; mi = i; }
    }
    if (mi == -1 || !(best < min_swing)) break;

    // drop rows (mi-1, mi). Need to handle edges safely.
    int a = mi - 1;
    int b = mi;
    // build new vectors
    int mm = price2.size();
    std::vector<int> keep;
    keep.reserve(mm-2);
    for (int i=0;i<mm;++i) if (i != a && i != b) keep.push_back(i);

    IntegerVector idx3(keep.size());
    CharacterVector type3(keep.size());
    NumericVector price3(keep.size());
    NumericVector dt3(keep.size());
    for (int t=0;t<(int)keep.size();++t) {
      int j = keep[t];
      idx3[t]=idx2[j]; type3[t]=type2[j]; price3[t]=price2[j]; dt3[t]=dt2[j];
    }
    copy_posixct_attrs(dt3, datetime);

    idx2 = idx3; type2 = type3; price2 = price3; dt2 = dt3;
    swing = recompute_swing(price2);
  }

  if (remove_latest && price2.size() >= 1) {
    int mm = price2.size() - 1;
    idx2 = idx2[Range(0, mm-1)];
    type2 = type2[Range(0, mm-1)];
    price2 = price2[Range(0, mm-1)];
    dt2 = dt2[Range(0, mm-1)];
  }

  return DataFrame::create(
    _["idx"] = idx2,
    _["type"] = type2,
    _["price"] = price2,
    _["datetime"] = dt2,
    _["stringsAsFactors"] = false
  );
}
