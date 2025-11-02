// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
using namespace Rcpp;

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
  Function pivots_cpp("pivots_cpp");
  List piv = pivots_cpp(_["high"]=high, _["low"]=low, _["span"]=span);
  DataFrame pivDF(piv);

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
