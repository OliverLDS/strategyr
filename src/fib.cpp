// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
using namespace Rcpp;

inline CharacterVector pct_labels(const NumericVector &v) {
  int n = v.size();
  CharacterVector out(n);
  std::ostringstream oss;
  for (int i = 0; i < n; ++i) {
    oss.str("");                    // reset
    oss.clear();
    oss << std::fixed << std::setprecision(1) << v[i] * 100.0;
    out[i] = oss.str() + "%";
  }
  return out;
}

// [[Rcpp::export]]
List fib_all_cpp(double trend_start, double trend_end,
                 NumericVector retr_levels = NumericVector::create(0.0, 0.236, 0.382, 0.5, 0.618, 0.786, 1.0),
                 NumericVector ext_levels  = NumericVector::create(0.272, 0.618, 1.0, 1.272, 1.618, 2.0)) {

  if (!R_finite(trend_start) || !R_finite(trend_end)) {
    return List::create(
      _["direction"]        = NA_STRING,
      _["H"]                = NA_REAL,
      _["L"]                = NA_REAL,
      _["diff"]             = NA_REAL,
      _["retracements"]     = R_NilValue,
      _["extensions_down"]  = R_NilValue,
      _["extensions_up"]    = R_NilValue,
      _["points"]           = DataFrame::create()
    );
  }
  double H = std::max(trend_start, trend_end);
  double L = std::min(trend_start, trend_end);
  double diff = H - L;
  if (diff <= 0.0) {
    return List::create(
      _["direction"]        = NA_STRING,
      _["H"]                = NA_REAL,
      _["L"]                = NA_REAL,
      _["diff"]             = NA_REAL,
      _["retracements"]     = R_NilValue,
      _["extensions_down"]  = R_NilValue,
      _["extensions_up"]    = R_NilValue,
      _["points"]           = DataFrame::create()
    );
  }
  
  const bool is_down = (trend_end < trend_start);
  const std::string direction = is_down ? "down" : "up";

  // Retracements always inside [L, H]
  NumericVector retr(retr_levels.size());
  if (is_down) { // down-leg: resistances above L
    for (int i = 0; i < retr.size(); ++i) retr[i] = L + retr_levels[i] * diff;
  } else {                       // up-leg: supports below H
    for (int i = 0; i < retr.size(); ++i) retr[i] = H - retr_levels[i] * diff;
  }
  CharacterVector retr_lbl = pct_labels(retr_levels);
  retr.attr("names") = retr_lbl;

  // Extensions in trend direction (continuation)
  NumericVector ext_down(ext_levels.size()); // below L
  NumericVector ext_up  (ext_levels.size()); // above H

  if (is_down) {
	for (int i = 0; i < ext_down.size(); ++i) ext_down[i] = L - ext_levels[i] * diff; // continuous ext
	for (int i = 0; i < ext_up.size();   ++i) ext_up[i]   = H + ext_levels[i] * diff; // reversal ext
  } else {
	for (int i = 0; i < ext_up.size();   ++i) ext_up[i]   = H + ext_levels[i] * diff; // continuous ext
	for (int i = 0; i < ext_down.size(); ++i) ext_down[i] = L - ext_levels[i] * diff; // reversal ext
  }

  CharacterVector ext_lbl = pct_labels(ext_levels);
  ext_down.attr("names") = ext_lbl;
  ext_up.attr("names")   = ext_lbl;
  
  const int n_retr = retr.size();
  const int n_xd   = ext_down.size();
  const int n_xu   = ext_up.size();
  const int N      = n_retr + n_xd + n_xu;
	
  NumericVector level(N);
  CharacterVector label(N), point_type(N), sr_hint(N), group(N);

  int k = 0;

  // Retracements
  for (int i = 0; i < n_retr; ++i, ++k) {
	  level[k]      = retr[i];
	  label[k]      = retr_lbl[i];
	  point_type[k] = "retracement";
	  // Without price_now: in an up-cycle, retracements are supports; in a down-cycle, resistances.
	  sr_hint[k]    = is_down ? "resistance" : "support";
	  group[k]      = "retr";
  }

  // Extensions DOWN (below L)
  for (int i = 0; i < n_xd; ++i, ++k) {
	  level[k]      = ext_down[i];
	  label[k]      = ext_lbl[i];
	  group[k]      = "ext_down";
	  // In a down-cycle, ext_down is continuous; in an up-cycle, it's reversal.
	  const bool continuous = is_down;
	  point_type[k] = continuous ? "continuous_extension" : "reversal_extension";
	  // S/R hint without price_now: below-market targets are typically SUPPORT zones
	  sr_hint[k]    = "support";
  }

  // Extensions UP (above H)
  for (int i = 0; i < n_xu; ++i, ++k) {
	  level[k]      = ext_up[i];
	  label[k]      = ext_lbl[i];
	  group[k]      = "ext_up";
	  // In an up-cycle, ext_up is continuous; in a down-cycle, it's reversal.
	  const bool continuous = !is_down;
	  point_type[k] = continuous ? "continuous_extension" : "reversal_extension";
	  // S/R hint without price_now: above-market targets are typically RESISTANCE zones
	  sr_hint[k]    = "resistance";
  }

  DataFrame points = DataFrame::create(
	  _["level"]      = level,
	  _["label"]      = label,
	  _["point_type"] = point_type,
	  _["sr_hint"]    = sr_hint,
	  _["group"]      = group,
	  _["stringsAsFactors"] = false
  );

  // Return meta + legacy vectors for backward compatibility
  return List::create(
	  _["direction"]        = direction,  // "up" or "down"
	  _["H"]                = H,
	  _["L"]                = L,
	  _["diff"]             = diff,
	  _["retracements"]     = retr,
	  _["extensions_down"]  = ext_down,
	  _["extensions_up"]    = ext_up,
	  _["points"]           = points
  );
}

// [[Rcpp::export]]
Rcpp::NumericMatrix fib_all_vec_cpp(const Rcpp::NumericVector& bg,
                                    const Rcpp::NumericVector& ed) {
  const int n = bg.size();
  const int K = 19;
  Rcpp::NumericMatrix out(n, K);
  std::fill(out.begin(), out.end(), NA_REAL);

  for (int i = 0; i < n; ++i) {
    double b = bg[i], e = ed[i];
    if (!R_finite(b) || !R_finite(e)) continue;

    Rcpp::List res = fib_all_cpp(b, e);
    if (!res.containsElementNamed("points")) continue;

    // explicit cast avoids ambiguous conversion
    Rcpp::DataFrame pts = Rcpp::as<Rcpp::DataFrame>(res["points"]);
    if (!pts.containsElementNamed("level")) continue;

	Rcpp::NumericVector lvl = pts["level"];
	    if (lvl.size() == 0) continue;

	    // drop NAs (defensive), then sort ascending
	    Rcpp::NumericVector good_lvl = lvl[!Rcpp::is_na(lvl)];
	    if (good_lvl.size() == 0) continue;

	    Rcpp::NumericVector sorted = Rcpp::clone(good_lvl);
	    std::sort(sorted.begin(), sorted.end());   // ascending

	    const int m = std::min(K, static_cast<int>(sorted.size()));
	    for (int k = 0; k < m; ++k) out(i, k) = sorted[k];
	    // remaining columns stay NA if m < K
  }
  return out;
}

