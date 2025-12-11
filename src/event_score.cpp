#include <Rcpp.h>
#include <cmath>
#include <unordered_map>

// Per-eid score and error storage
struct BucketState {
  double score;
	double error;
  double t_last;
  bool initialized;

  BucketState() : score(0.0), error(0.0), t_last(0.0), initialized(false) {}
};


// [[Rcpp::export]]
Rcpp::DataFrame event_score(const Rcpp::IntegerVector &event_id, const Rcpp::NumericVector &future_outcome, double h, double coef = 1.0)
{
  const int n = event_id.size();
  if (future_outcome.size() != n) {
    Rcpp::stop("event_id and future_outcome must have the same length.");
  }
	if (h <= 0.0) {
    Rcpp::stop("h must be positive.");
  }

  Rcpp::NumericVector bucket_indicator(n, NA_REAL);
	Rcpp::NumericVector bucket_error(n, NA_REAL);

  std::unordered_map<int, BucketState> event_map;
	event_map.reserve(std::min(n, 1024));

  const double lambda = std::log(2.0) / h;  // half-life h in "time units"

  for (int i = 0; i < n; ++i) {

    const double t_i = static_cast<double>(i);
		const int eid  = event_id[i];
    const double fo  = future_outcome[i];
		
		const bool has_event   = (eid != NA_INTEGER);
    const bool has_outcome = !Rcpp::NumericVector::is_na(fo);
		
		if (!has_event) {
      bucket_indicator[i] = NA_REAL;
      bucket_error[i]     = NA_REAL;
      continue;
    }
		
		BucketState &et = event_map[eid];

    if (et.initialized) {
      double dt = t_i - et.t_last;
      if (dt < 0.0) dt = 0.0;

      const double decaying_ratio = std::exp(-lambda * dt);
      const double decayed_score = decaying_ratio * et.score;
			const double decayed_error = decaying_ratio * et.error;

      // prediction based only on past
      bucket_indicator[i] = et.score;
			bucket_error[i] = et.error;

      if (has_outcome) {
				double new_error = fo - decayed_score;
        et.score = (1.0 - decaying_ratio) * coef*fo + decayed_score;
				et.error = (1.0 - decaying_ratio) * new_error + decayed_error;
        et.t_last = t_i;
      }

    } else {
      // First time this state is seen
      bucket_indicator[i] = NA_REAL;
			bucket_error[i] = NA_REAL;

      if (has_outcome) {
        et.score = coef*fo;
				et.error = 0.0;
        et.t_last = t_i;
        et.initialized = true;
      }
    }
	}

  return Rcpp::DataFrame::create(
    Rcpp::_["score"] = bucket_indicator,
	  Rcpp::_["error"] = bucket_error
  );
}