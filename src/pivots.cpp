// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
#include <vector>
#include <cmath>
#include "utils_rcpp.h"


// [[Rcpp::export]]
Rcpp::DataFrame detect_pivots_cpp(const Rcpp::NumericVector &high,
                                  const Rcpp::NumericVector &low,
                                  const Rcpp::NumericVector &datetime,
                                  int span = 3,
                                  Rcpp::Nullable<int> latest_n = R_NilValue,
                                  bool refined = true,
                                  double min_swing = 0.05) {
  const int n = high.size();
  if (low.size() != n || datetime.size() != n) {
    Rcpp::stop("high, low, datetime must have the same length.");
  }

  const double *h = REAL(high);
  const double *l = REAL(low);
	const double *dt = REAL(datetime);

  // ---- 1) Detect raw pivots (H/L) ----
  std::vector<int>    piv_idx;
  std::vector<double> piv_price;
  std::vector<char>   piv_type;  // 'H' or 'L'
	std::vector<double> piv_availtime; // time when pivot becomes observable

  // heuristic: at most n/span pivots; reserve to reduce reallocation
  piv_idx.reserve(n / span + 10);
  piv_price.reserve(n / span + 10);
  piv_type.reserve(n / span + 10);
	piv_availtime.reserve(n / span + 10);

  for (int i = span; i < n - span; ++i) {
		
		int avail_i = i + span + 1;          // 0-based index of bar that confirms the pivot
		double avail_t = dt[avail_i];    // datetime[avail_i]
				
    if (!Rcpp::NumericVector::is_na(high[i])) {
      double center = h[i];
      bool is_high = true;
      for (int j = i - span; j <= i + span; ++j) {
        if (j == i) continue;
        if (!(center > h[j])) { // not strictly greater
          is_high = false;
          break;
        }
      }
      if (is_high) {
        piv_idx.push_back(i + 1);      // 1-based for R
        piv_price.push_back(center);
        piv_type.push_back('H');
				piv_availtime.push_back(avail_t);
      }
    }

    if (!Rcpp::NumericVector::is_na(low[i])) {
      double center = l[i];
      bool is_low = true;
      for (int j = i - span; j <= i + span; ++j) {
        if (j == i) continue;
        if (!(center < l[j])) {
          is_low = false;
          break;
        }
      }
      if (is_low) {
        piv_idx.push_back(i + 1);
        piv_price.push_back(center);
        piv_type.push_back('L');
				piv_availtime.push_back(avail_t);
      }
    }
  }

  int m = static_cast<int>(piv_idx.size());
  if (m == 0) {
    // nothing found
    return Rcpp::DataFrame::create(
      Rcpp::_["idx"]      = Rcpp::IntegerVector(0),
      Rcpp::_["type"]     = Rcpp::CharacterVector(0),
      Rcpp::_["price"]    = Rcpp::NumericVector(0),
      Rcpp::_["datetime"] = Rcpp::NumericVector(0),
			Rcpp::_["availtime"] = Rcpp::NumericVector(0),
      Rcpp::_["stringsAsFactors"] = false
    );
  }

  // ---- 2) Optional tail cropping (latest_n) ----
  int start0 = 0;
  if (latest_n.isNotNull()) {
    int k = Rcpp::as<int>(latest_n);
    if (k < m) {
      start0 = m - k;
      m = k;
    }
  }

  // wrap the chosen slice into Rcpp vectors
  Rcpp::IntegerVector idx(m);
  Rcpp::NumericVector price(m);
  Rcpp::CharacterVector type(m);
  Rcpp::NumericVector dt_pivot(m);
	Rcpp::NumericVector dt_avail(m);

  for (int i = 0; i < m; ++i) {
    int src = start0 + i;
    idx[i]   = piv_idx[src];
    price[i] = piv_price[src];
    type[i]  = (piv_type[src] == 'H') ? "H" : "L";
		dt_avail[i]  = piv_availtime[src];

    int j = idx[i] - 1; // 1-based -> 0-based
    dt_pivot[i] = (j >= 0 && j < n) ? datetime[j] : NA_REAL;
  }
	copy_posixct_attrs(dt_pivot, datetime);
	copy_posixct_attrs(dt_avail, datetime);

  if (!refined) {
    return Rcpp::DataFrame::create(
      Rcpp::_["idx"]      = idx,
      Rcpp::_["type"]     = type,
      Rcpp::_["price"]    = price,
			Rcpp::_["datetime"]  = dt_pivot,
			Rcpp::_["availtime"] = dt_avail,
      Rcpp::_["stringsAsFactors"] = false
    );
  }

  // ---- 3) Refine: collapse each H/L run into one extreme ----
  std::vector<int>    idx2;
  std::vector<double> price2;
  std::vector<char>   type2c;
  std::vector<double> dt2;
	std::vector<double> avail2;

  idx2.reserve(m);
  price2.reserve(m);
  type2c.reserve(m);
  dt2.reserve(m);
	avail2.reserve(m);

  int run_start = 0;
  auto get_type_char = [&](int i) -> char {
    return (type[i] == "H") ? 'H' : 'L';
  };

  for (int i = 1; i <= m; ++i) {
    bool end_run = (i == m) || (get_type_char(i) != get_type_char(run_start));
    if (end_run) {
      char tchar = get_type_char(run_start);
      int best = run_start;
      double bestp = price[run_start];

      if (tchar == 'H') {
        for (int j = run_start + 1; j < i; ++j) {
          if (price[j] > bestp) {
            bestp = price[j];
            best  = j;
          }
        }
      } else {
        for (int j = run_start + 1; j < i; ++j) {
          if (price[j] < bestp) {
            bestp = price[j];
            best  = j;
          }
        }
      }

      idx2.push_back(idx[best]);
      price2.push_back(price[best]);
      type2c.push_back(tchar);
			dt2.push_back(dt_pivot[best]);
			avail2.push_back(dt_avail[best]);

      run_start = i;
    }
  }

  int k2 = static_cast<int>(idx2.size());
  if (k2 == 0) {
    return Rcpp::DataFrame::create(
      Rcpp::_["idx"]      = Rcpp::IntegerVector(0),
      Rcpp::_["type"]     = Rcpp::CharacterVector(0),
      Rcpp::_["price"]    = Rcpp::NumericVector(0),
      Rcpp::_["datetime"] = Rcpp::NumericVector(0),
			Rcpp::_["availtime"] = Rcpp::NumericVector(0),
      Rcpp::_["stringsAsFactors"] = false
    );
  }

  // ---- 4) Iterative removal of too-small swings ----
  auto compute_swing = [](const std::vector<double> &p) {
    int len = static_cast<int>(p.size());
    std::vector<double> s(len, R_PosInf);
    for (int i = 1; i < len; ++i) {
      double prev = p[i - 1];
      double cur  = p[i];
      if (R_finite(prev) && R_finite(cur) && cur != 0.0) {
        s[i] = std::fabs(prev / cur - 1.0);
      }
    }
    if (len >= 1) s[0] = R_PosInf;
    if (len >= 2) s[1] = R_PosInf;
    if (len >= 1) s[len - 1] = R_PosInf;
    return s;
  };

  std::vector<double> price2v = price2; // local copy already
  std::vector<int>    idx2v   = idx2;
  std::vector<char>   type2v  = type2c;
  std::vector<double> dt2v    = dt2;
	std::vector<double> avail2v  = avail2;

  std::vector<double> swing = compute_swing(price2v);

  bool remove_latest = false;
  {
    int len = static_cast<int>(price2v.size());
    if (len >= 1) {
      double lastSwing = swing[len - 1];
      if (R_finite(lastSwing) && lastSwing < min_swing) {
        remove_latest = true;
      }
    }
  }

  while (true) {
    int len = static_cast<int>(price2v.size());
    if (len <= 3) break; // no more meaningful deletions

    // find smallest swing
    int mi = -1;
    double best = R_PosInf;
    for (int i = 0; i < len; ++i) {
      double v = swing[i];
      if (R_finite(v) && v < best) {
        best = v;
        mi = i;
      }
    }
    if (mi == -1 || !(best < min_swing)) break;

    int a = mi - 1;
    int b = mi;
    if (a < 0) a = 0;
    if (b >= len) b = len - 1;
    if (a == b) break; // nothing to drop

    // erase from std::vector (note: erase second index first)
    if (b > a) {
      price2v.erase(price2v.begin() + b);
      idx2v.erase(idx2v.begin() + b);
      type2v.erase(type2v.begin() + b);
      dt2v.erase(dt2v.begin() + b);
			avail2v.erase(avail2v.begin() + b);
			
      price2v.erase(price2v.begin() + a);
      idx2v.erase(idx2v.begin() + a);
      type2v.erase(type2v.begin() + a);
      dt2v.erase(dt2v.begin() + a);
			avail2v.erase(avail2v.begin() + a);
    }

    swing = compute_swing(price2v);
  }

  if (remove_latest && !price2v.empty()) {
    price2v.pop_back();
    idx2v.pop_back();
    type2v.pop_back();
    dt2v.pop_back();
		avail2v.pop_back();
  }

  int out_n = static_cast<int>(price2v.size());
  Rcpp::IntegerVector idx_out(out_n);
  Rcpp::CharacterVector type_out(out_n);
  Rcpp::NumericVector price_out(out_n);
  Rcpp::NumericVector dt_out(out_n);
	Rcpp::NumericVector avail_out(out_n);

  for (int i = 0; i < out_n; ++i) {
    idx_out[i]   = idx2v[i];
    price_out[i] = price2v[i];
    type_out[i]  = (type2v[i] == 'H') ? "H" : "L";
    dt_out[i]    = dt2v[i];
		avail_out[i]  = avail2v[i];
  }
  copy_posixct_attrs(dt_out, datetime);
	copy_posixct_attrs(avail_out, datetime);

  return Rcpp::DataFrame::create(
    Rcpp::_["idx"]      = idx_out,
    Rcpp::_["type"]     = type_out,
    Rcpp::_["price"]    = price_out,
		Rcpp::_["datetime"]  = dt_out,      // bar where pivot happens
		Rcpp::_["availtime"] = avail_out,   // first bar where pivot is observable
    Rcpp::_["stringsAsFactors"] = false
  );
}

