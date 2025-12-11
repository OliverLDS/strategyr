#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <cmath>

// Helper: check if a value is finite
inline bool is_finite(double x) {
  return R_finite(x);
}

//' Cluster generic price levels into zones
//'
//' @param level Numeric vector of price levels (Fibs, EMAs, previous pivots, etc.).
//' @param score Numeric vector of scores for each level (same length as level).
//' @param tolerance Numeric vector of tolerance values. If length 1, used as
//'        global tolerance; if length == length(level), used per-point.
//' @param score_mode Integer flag for how to aggregate scores within a zone:
//'        0 = sum of scores, 1 = max score.
//'
//' @return Data frame with clustered zones:
//'         zone_low, zone_high, zone_center, zone_score, zone_n
// [[Rcpp::export]]
Rcpp::DataFrame cluster_price_levels(const Rcpp::NumericVector &level,
                                     const Rcpp::NumericVector &score,
                                     const Rcpp::NumericVector &tolerance,
                                     int score_mode = 0) {
  const int n = level.size();
  if (score.size() != n) {
    Rcpp::stop("level and score must have the same length.");
  }
  if (tolerance.size() != 1 && tolerance.size() != n) {
    Rcpp::stop("tolerance must be length 1 or length(level).");
  }
  if (n == 0) {
    return Rcpp::DataFrame::create(
      Rcpp::Named("zone_low")    = Rcpp::NumericVector(0),
      Rcpp::Named("zone_high")   = Rcpp::NumericVector(0),
      Rcpp::Named("zone_center") = Rcpp::NumericVector(0),
      Rcpp::Named("zone_score")  = Rcpp::NumericVector(0),
      Rcpp::Named("zone_n")      = Rcpp::IntegerVector(0)
    );
  }

  // Build index and sort by level
  std::vector<int> idx(n);
  for (int i = 0; i < n; ++i) idx[i] = i;
  std::sort(idx.begin(), idx.end(), [&](int a, int b) {
    return level[a] < level[b];
  });

  std::vector<double> zone_low;
  std::vector<double> zone_high;
  std::vector<double> zone_score;
  std::vector<int>    zone_n;

  bool in_cluster = false;
  double current_low = 0.0;
  double current_high = 0.0;
  double current_score = 0.0;
  int current_n = 0;
  double last_level = 0.0;

  for (int k = 0; k < n; ++k) {
    int i = idx[k];
    double lvl = level[i];
    double sc  = score[i];

    // Skip non-finite levels
    if (!is_finite(lvl)) continue;

    double tol = (tolerance.size() == 1) ? tolerance[0] : tolerance[i];

    if (!in_cluster) {
      // Start first cluster
      in_cluster   = true;
      current_low  = lvl;
      current_high = lvl;
      current_score = sc;
      current_n     = 1;
      last_level    = lvl;
    } else {
      // Check if this level belongs to the current zone
      if (std::fabs(lvl - last_level) <= tol) {
        // Same zone
        if (lvl < current_low)  current_low = lvl;
        if (lvl > current_high) current_high = lvl;

        if (score_mode == 0) {
          current_score += sc;                 // sum
        } else if (score_mode == 1) {
          current_score = std::max(current_score, sc); // max
        } else {
          // Fallback: sum
          current_score += sc;
        }

        current_n   += 1;
        last_level   = lvl;
      } else {
        // Close previous cluster
        zone_low.push_back(current_low);
        zone_high.push_back(current_high);
        zone_score.push_back(current_score);
        zone_n.push_back(current_n);

        // Start new cluster
        current_low  = lvl;
        current_high = lvl;
        current_score = sc;
        current_n     = 1;
        last_level    = lvl;
      }
    }
  }

  // Push the last cluster
  if (in_cluster) {
    zone_low.push_back(current_low);
    zone_high.push_back(current_high);
    zone_score.push_back(current_score);
    zone_n.push_back(current_n);
  }

  const int m = zone_low.size();
  Rcpp::NumericVector zone_low_vec(m);
  Rcpp::NumericVector zone_high_vec(m);
  Rcpp::NumericVector zone_center_vec(m);
  Rcpp::NumericVector zone_score_vec(m);
  Rcpp::IntegerVector zone_n_vec(m);

  for (int j = 0; j < m; ++j) {
    zone_low_vec[j]    = zone_low[j];
    zone_high_vec[j]   = zone_high[j];
    zone_center_vec[j] = 0.5 * (zone_low[j] + zone_high[j]);
    zone_score_vec[j]  = zone_score[j];
    zone_n_vec[j]      = zone_n[j];
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("zone_low")    = zone_low_vec,
    Rcpp::Named("zone_high")   = zone_high_vec,
    Rcpp::Named("zone_center") = zone_center_vec,
    Rcpp::Named("zone_score")  = zone_score_vec,
    Rcpp::Named("zone_n")      = zone_n_vec
  );
}


//' Merge overlapping or nearby zones
//'
//' @param zone_low Numeric vector of zone lower bounds.
//' @param zone_high Numeric vector of zone upper bounds.
//' @param zone_score Numeric vector of zone scores.
//' @param zone_n Integer vector of counts per zone.
//' @param gap_tolerance Zones with low <= last.high + gap_tolerance are merged.
//' @param score_mode Integer flag for score merge:
//'        0 = sum scores, 1 = max score.
//'
//' @return Data frame with merged zones:
//'         zone_low, zone_high, zone_center, zone_score, zone_n
// [[Rcpp::export]]
Rcpp::DataFrame merge_overlapping_zones(const Rcpp::NumericVector &zone_low,
                                        const Rcpp::NumericVector &zone_high,
                                        const Rcpp::NumericVector &zone_score,
                                        const Rcpp::IntegerVector &zone_n,
                                        double gap_tolerance = 0.0,
                                        int score_mode = 0) {
  const int m = zone_low.size();
  if (zone_high.size() != m || zone_score.size() != m || zone_n.size() != m) {
    Rcpp::stop("zone_low, zone_high, zone_score, zone_n must have same length.");
  }
  if (m == 0) {
    return Rcpp::DataFrame::create(
      Rcpp::Named("zone_low")    = Rcpp::NumericVector(0),
      Rcpp::Named("zone_high")   = Rcpp::NumericVector(0),
      Rcpp::Named("zone_center") = Rcpp::NumericVector(0),
      Rcpp::Named("zone_score")  = Rcpp::NumericVector(0),
      Rcpp::Named("zone_n")      = Rcpp::IntegerVector(0)
    );
  }

  // Sort indices by zone_low
  std::vector<int> idx(m);
  for (int i = 0; i < m; ++i) idx[i] = i;
  std::sort(idx.begin(), idx.end(), [&](int a, int b) {
    return zone_low[a] < zone_low[b];
  });

  std::vector<double> out_low;
  std::vector<double> out_high;
  std::vector<double> out_score;
  std::vector<int>    out_n;

  // Initialize with first zone
  int i0 = idx[0];
  double cur_low   = zone_low[i0];
  double cur_high  = zone_high[i0];
  double cur_score = zone_score[i0];
  int    cur_n     = zone_n[i0];

  for (int k = 1; k < m; ++k) {
    int i = idx[k];
    double z_low   = zone_low[i];
    double z_high  = zone_high[i];
    double z_score = zone_score[i];
    int    z_n     = zone_n[i];

    // Overlap or "touching" within gap_tolerance
    if (z_low <= cur_high + gap_tolerance) {
      // Merge
      if (z_low < cur_low)   cur_low  = z_low;
      if (z_high > cur_high) cur_high = z_high;

      if (score_mode == 0) {
        cur_score += z_score;                  // sum
      } else if (score_mode == 1) {
        cur_score = std::max(cur_score, z_score); // max
      } else {
        cur_score += z_score;
      }

      cur_n += z_n;
    } else {
      // Close current merged zone
      out_low.push_back(cur_low);
      out_high.push_back(cur_high);
      out_score.push_back(cur_score);
      out_n.push_back(cur_n);

      // Start new merged zone
      cur_low   = z_low;
      cur_high  = z_high;
      cur_score = z_score;
      cur_n     = z_n;
    }
  }

  // Push last merged zone
  out_low.push_back(cur_low);
  out_high.push_back(cur_high);
  out_score.push_back(cur_score);
  out_n.push_back(cur_n);

  const int k_out = out_low.size();
  Rcpp::NumericVector zone_low_vec(k_out);
  Rcpp::NumericVector zone_high_vec(k_out);
  Rcpp::NumericVector zone_center_vec(k_out);
  Rcpp::NumericVector zone_score_vec(k_out);
  Rcpp::IntegerVector zone_n_vec(k_out);

  for (int j = 0; j < k_out; ++j) {
    zone_low_vec[j]    = out_low[j];
    zone_high_vec[j]   = out_high[j];
    zone_center_vec[j] = 0.5 * (out_low[j] + out_high[j]);
    zone_score_vec[j]  = out_score[j];
    zone_n_vec[j]      = out_n[j];
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("zone_low")    = zone_low_vec,
    Rcpp::Named("zone_high")   = zone_high_vec,
    Rcpp::Named("zone_center") = zone_center_vec,
    Rcpp::Named("zone_score")  = zone_score_vec,
    Rcpp::Named("zone_n")      = zone_n_vec
  );
}
