// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
using namespace Rcpp;

// equality like R's identical() for doubles (exact match)
inline bool same_double(double a, double b) {
  return (a == b) || (R_IsNA(a) && R_IsNA(b));
}

// order (argsort) by two keys ascending: end then start
inline IntegerVector order_by_end_start(const NumericVector &end,
                                        const NumericVector &start) {
  int n = end.size();
  IntegerVector ord = seq(0, n - 1);
  // stable sort by second key then first key (or just do one custom comparator)
  std::sort(ord.begin(), ord.end(), [&](int i, int j) {
    if (end[i] < end[j]) return true;
    if (end[i] > end[j]) return false;
    // ends equal -> compare start
    return start[i] < start[j];
  });
  return ord;
}

// [[Rcpp::export]]
DataFrame get_now_cycles_cpp(const DataFrame &pivots,
                                     int min_backward_bars = 360,
                                     int min_stable_pivots = 7) {
  IntegerVector idx   = pivots["idx"];
  NumericVector price = pivots["price"];
  NumericVector dt    = pivots["datetime"]; // POSIXct numeric
  int n = idx.size();

  if (n < 2) {
    // empty result with expected columns
    return DataFrame::create(
      _["start"]     = NumericVector(0),
      _["end"]       = NumericVector(0),
      _["bg_price"]  = NumericVector(0),
      _["ed_price"]  = NumericVector(0),
      _["direction"] = CharacterVector(0),
      _["stringsAsFactors"] = false
    );
  }

  // preserve POSIXct attrs
  SEXP cls = dt.attr("class");
  SEXP tz  = dt.attr("tzone");

  int max_idx = idx[n - 1];
  int max_stop_idx = max_idx - min_backward_bars;

  // storage
  std::vector<double> bg_p, ed_p, bg_t, ed_t;
  std::vector<std::string> direc;

  double last_up_bg = NA_REAL, last_up_ed = NA_REAL;
  double last_dn_bg = NA_REAL, last_dn_ed = NA_REAL;
  int stable_pivots = 0;

  for (int i = 2; i <= n; ++i) {
    int start_tail = n - i; // 0-based start for the tail slice

    // find min/max within [start_tail, n-1]
    int idx_low_rel  = start_tail;
    int idx_high_rel = start_tail;
    double minp = price[start_tail];
    double maxp = price[start_tail];

    for (int j = start_tail + 1; j < n; ++j) {
      double pj = price[j];
      if (pj < minp) { minp = pj; idx_low_rel = j; }
      if (pj > maxp) { maxp = pj; idx_high_rel = j; }
    }

    if (idx_high_rel > idx_low_rel) {
      // upward cycle: low -> high
      double bgp = price[idx_low_rel],  edp = price[idx_high_rel];
      double bgt = dt[idx_low_rel],     edt = dt[idx_high_rel];

      bool same_as_last = same_double(bgp, last_up_bg) && same_double(edp, last_up_ed);
      if (!same_as_last) {
        bg_p.push_back(bgp); ed_p.push_back(edp);
        bg_t.push_back(bgt); ed_t.push_back(edt);
        direc.emplace_back("up");
        last_up_bg = bgp; last_up_ed = edp;
        // Uncomment next line if you want to reset stability on new unique cycle:
        // stable_pivots = 0;
      } else {
        ++stable_pivots;
      }
    } else {
      // downward cycle: high -> low
      double bgp = price[idx_high_rel], edp = price[idx_low_rel];
      double bgt = dt[idx_high_rel],    edt = dt[idx_low_rel];

      bool same_as_last = same_double(bgp, last_dn_bg) && same_double(edp, last_dn_ed);
      if (!same_as_last) {
        bg_p.push_back(bgp); ed_p.push_back(edp);
        bg_t.push_back(bgt); ed_t.push_back(edt);
        direc.emplace_back("down");
        last_dn_bg = bgp; last_dn_ed = edp;
        // stable_pivots = 0;
      } else {
        ++stable_pivots;
      }
    }

    // stopping rule
    if (idx[start_tail] < max_stop_idx && stable_pivots >= min_stable_pivots) {
      break;
    }
  }

  int m = (int)bg_p.size();
  if (m == 0) {
    return DataFrame::create(
      _["start"]     = NumericVector(0),
      _["end"]       = NumericVector(0),
      _["bg_price"]  = NumericVector(0),
      _["ed_price"]  = NumericVector(0),
      _["direction"] = CharacterVector(0),
      _["stringsAsFactors"] = false
    );
  }

  // build vectors
  NumericVector start_dt(m), end_dt(m), bgp(m), edp(m);
  CharacterVector dirv(m);
  for (int i = 0; i < m; ++i) {
    start_dt[i] = bg_t[i];
    end_dt[i]   = ed_t[i];
    bgp[i]      = bg_p[i];
    edp[i]      = ed_p[i];
    dirv[i]     = direc[i];
  }
  start_dt.attr("class") = cls; start_dt.attr("tzone") = tz;
  end_dt.attr("class")   = cls; end_dt.attr("tzone")   = tz;

  // sort by end, then start (ascending) so the freshest (max end) is last
  IntegerVector ord = order_by_end_start(end_dt, start_dt);
  NumericVector start_dt_s(m), end_dt_s(m), bgp_s(m), edp_s(m);
  CharacterVector dirv_s(m);
  for (int i = 0; i < m; ++i) {
    int j = ord[i];
    start_dt_s[i] = start_dt[j];
    end_dt_s[i]   = end_dt[j];
    bgp_s[i]      = bgp[j];
    edp_s[i]      = edp[j];
    dirv_s[i]     = dirv[j];
  }
  start_dt_s.attr("class") = cls; start_dt_s.attr("tzone") = tz;
  end_dt_s.attr("class")   = cls; end_dt_s.attr("tzone")   = tz;

  // final unified data.frame
  DataFrame out = DataFrame::create(
    _["start"]     = start_dt_s,
    _["end"]       = end_dt_s,
    _["bg_price"]  = bgp_s,
    _["ed_price"]  = edp_s,
    _["direction"] = dirv_s,
    _["stringsAsFactors"] = false
  );
  return out;
}

// [[Rcpp::export]]
DataFrame get_now_fresh_main_cycles_cpp(const DataFrame &pivots,
                                int min_backward_bars = 360,
                                int min_stable_pivots = 7) {

  IntegerVector idx   = pivots["idx"];
  NumericVector price = pivots["price"];
  NumericVector dt    = pivots["datetime"]; // POSIXct numeric
  int n = idx.size();

  // empty result
  if (n < 2) {
    return DataFrame::create(
      _["fresh_start"]      = NumericVector::create(NA_REAL),
      _["fresh_end"]        = NumericVector::create(NA_REAL),
      _["fresh_bg_price"]   = NumericVector::create(NA_REAL),
      _["fresh_ed_price"]   = NumericVector::create(NA_REAL),
      _["fresh_direction"]  = CharacterVector::create(NA_STRING),
      _["main_start"]       = NumericVector::create(NA_REAL),
      _["main_end"]         = NumericVector::create(NA_REAL),
      _["main_bg_price"]    = NumericVector::create(NA_REAL),
      _["main_ed_price"]    = NumericVector::create(NA_REAL),
      _["main_direction"]   = CharacterVector::create(NA_STRING),
      _["stringsAsFactors"] = false
    );
  }

  // preserve POSIXct attrs
  SEXP cls = dt.attr("class");
  SEXP tz  = dt.attr("tzone");

  int max_idx = idx[n - 1];
  int max_stop_idx = max_idx - min_backward_bars;

  double last_up_bg = NA_REAL, last_up_ed = NA_REAL;
  double last_dn_bg = NA_REAL, last_dn_ed = NA_REAL;
  int stable_pivots = 0;
  bool have_any = false;

  double fresh_start = NA_REAL, fresh_end = NA_REAL, fresh_bg = NA_REAL, fresh_ed = NA_REAL;
  std::string fresh_dir = "";
  double main_start = NA_REAL, main_end = NA_REAL, main_bg = NA_REAL, main_ed = NA_REAL;
  std::string main_dir = "";

  for (int i = 2; i <= n; ++i) {
    int start_tail = n - i;
    int idx_low_rel = start_tail;
    int idx_high_rel = start_tail;
    double minp = price[start_tail];
    double maxp = price[start_tail];

    for (int j = start_tail + 1; j < n; ++j) {
      double pj = price[j];
      if (pj < minp) { minp = pj; idx_low_rel = j; }
      if (pj > maxp) { maxp = pj; idx_high_rel = j; }
    }

    double cyc_bg_p, cyc_ed_p, cyc_bg_t, cyc_ed_t;
    std::string cyc_dir;
    if (idx_high_rel > idx_low_rel) {
      cyc_bg_p = price[idx_low_rel]; cyc_ed_p = price[idx_high_rel];
      cyc_bg_t = dt[idx_low_rel];    cyc_ed_t = dt[idx_high_rel];
      cyc_dir = "up";
      if (same_double(cyc_bg_p, last_up_bg) && same_double(cyc_ed_p, last_up_ed)) {
        ++stable_pivots;
        if (idx[start_tail] < max_stop_idx && stable_pivots >= min_stable_pivots) break;
        continue;
      }
      last_up_bg = cyc_bg_p; last_up_ed = cyc_ed_p;
    } else {
      cyc_bg_p = price[idx_high_rel]; cyc_ed_p = price[idx_low_rel];
      cyc_bg_t = dt[idx_high_rel];    cyc_ed_t = dt[idx_low_rel];
      cyc_dir = "down";
      if (same_double(cyc_bg_p, last_dn_bg) && same_double(cyc_ed_p, last_dn_ed)) {
        ++stable_pivots;
        if (idx[start_tail] < max_stop_idx && stable_pivots >= min_stable_pivots) break;
        continue;
      }
      last_dn_bg = cyc_bg_p; last_dn_ed = cyc_ed_p;
    }

    if (!have_any) {
      have_any = true;
      fresh_start = main_start = cyc_bg_t;
      fresh_end   = main_end   = cyc_ed_t;
      fresh_bg    = main_bg    = cyc_bg_p;
      fresh_ed    = main_ed    = cyc_ed_p;
      fresh_dir   = main_dir   = cyc_dir;
    } else {
      if (cyc_ed_t > fresh_end) {
        fresh_start = cyc_bg_t;
        fresh_end   = cyc_ed_t;
        fresh_bg    = cyc_bg_p;
        fresh_ed    = cyc_ed_p;
        fresh_dir   = cyc_dir;
      }
      if (cyc_ed_t < main_end) {
        main_start = cyc_bg_t;
        main_end   = cyc_ed_t;
        main_bg    = cyc_bg_p;
        main_ed    = cyc_ed_p;
        main_dir   = cyc_dir;
      }
    }

    if (idx[start_tail] < max_stop_idx && stable_pivots >= min_stable_pivots) {
      break;
    }
  }

  NumericVector fs(1), fe(1), fbg(1), fed(1), ms(1), me(1), mbg(1), med(1);
  CharacterVector fd(1), md(1);

  fs[0] = fresh_start; fe[0] = fresh_end; fbg[0] = fresh_bg; fed[0] = fresh_ed; fd[0] = fresh_dir;
  ms[0] = main_start;  me[0] = main_end;  mbg[0] = main_bg;  med[0] = main_ed;  md[0] = main_dir;

  fs.attr("class") = cls; fs.attr("tzone") = tz;
  fe.attr("class") = cls; fe.attr("tzone") = tz;
  ms.attr("class") = cls; ms.attr("tzone") = tz;
  me.attr("class") = cls; me.attr("tzone") = tz;

  return DataFrame::create(
    _["fresh_start"]     = fs,
    _["fresh_end"]       = fe,
    _["fresh_bg_price"]  = fbg,
    _["fresh_ed_price"]  = fed,
    _["fresh_direction"] = fd,
    _["main_start"]      = ms,
    _["main_end"]        = me,
    _["main_bg_price"]   = mbg,
    _["main_ed_price"]   = med,
    _["main_direction"]  = md,
    _["stringsAsFactors"] = false
  );
}
