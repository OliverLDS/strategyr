// ker_rolling.h

#include <deque>

int rolling_1st_moment_kernel(double* out, const double* in, size_t len, size_t n) {
	if (n == 0 || len == 0) return 1;
	rolling_1st_moment rolling_engine;
	rolling_engine.inv_n = 1.0/n;
	for (size_t i = 0; i < len; ++i) {
		if (i < n) {
			rolling_engine.rolling_sum += in[i];
			out[i] = (i == n-1) ? rolling_engine.mean() : STRATEGYR::kNaReal;
		} else {
			rolling_engine.update(in[i], in[i-n]);
			out[i] = rolling_engine.mean();
		}
	}
	return 0;
}

int rolling_sum_kernel(double* out, const double* in, size_t len, size_t n) {
	if (n == 0 || len == 0) return 1;

  std::fill(out, out + len, STRATEGYR::kNaReal);
  double window_sum = 0.0;
  size_t na_count = 0;

  for (size_t i = 0; i < len; ++i) {
    if (is_na(in[i])) {
      na_count += 1;
    } else {
      window_sum += in[i];
    }

    if (i >= n) {
      const double outgoing = in[i - n];
      if (is_na(outgoing)) {
        na_count -= 1;
      } else {
        window_sum -= outgoing;
      }
    }

    if (i >= n - 1 && na_count == 0) {
      out[i] = window_sum;
    }
  }

  return 0;
}

int rolling_2nd_moment_kernel(double* out, const double* in, size_t len, size_t n, bool sample = false) {
	if (n == 0 || len == 0) return 1;
	if (sample && n < 2) return 1;
	rolling_2nd_moment rolling_engine;
	rolling_engine.inv_n = 1.0/n;
	rolling_engine.inv_denom = sample ? 1.0/(n - 1) : 1.0/n;
	for (size_t i = 0; i < len; ++i) {
		if (i < n) {
			rolling_engine.rolling_sum += in[i];
			rolling_engine.rolling_sum_sq += in[i]*in[i];
			out[i] = (i == n-1) ? rolling_engine.sd() : STRATEGYR::kNaReal;
		} else {
			rolling_engine.update(in[i], in[i-n]);
			out[i] = rolling_engine.sd();
		}
	}
	return 0;
}

int rolling_mean_abs_dev_kernel(double* out, const double* in, size_t len, size_t n) {
  if (n == 0 || len == 0) return 1;

  std::vector<double> means(len, STRATEGYR::kNaReal);
  const int err = rolling_1st_moment_kernel(means.data(), in, len, n);
  if (err != 0) return err;

  std::fill(out, out + len, STRATEGYR::kNaReal);

  for (size_t i = n - 1; i < len; ++i) {
    const double mu = means[i];
    if (is_na(mu)) continue;

    double abs_dev_sum = 0.0;
    bool has_na = false;
    for (size_t j = i + 1 - n; j <= i; ++j) {
      const double xj = in[j];
      if (is_na(xj)) {
        has_na = true;
        break;
      }
      abs_dev_sum += std::abs(xj - mu);
    }

    if (!has_na) {
      out[i] = abs_dev_sum / static_cast<double>(n);
    }
  }

  return 0;
}

template <bool is_max>
int rolling_extrema_kernel(double* out, const double* in, size_t len, size_t n) {
	if (n == 0 || len == 0) return 1;

  std::fill(out, out + len, STRATEGYR::kNaReal);
  std::deque<size_t> q;
  size_t na_count = 0;

  for (size_t i = 0; i < len; ++i) {
    const double incoming = in[i];
    if (is_na(incoming)) {
      na_count += 1;
    } else {
      while (!q.empty()) {
        const double back_val = in[q.back()];
        const bool worse = is_max ? (back_val <= incoming) : (back_val >= incoming);
        if (!worse) break;
        q.pop_back();
      }
      q.push_back(i);
    }

    if (i >= n) {
      const size_t out_idx = i - n;
      if (is_na(in[out_idx])) {
        na_count -= 1;
      }
      while (!q.empty() && q.front() <= out_idx) {
        q.pop_front();
      }
    }

    if (i >= n - 1 && na_count == 0 && !q.empty()) {
      out[i] = in[q.front()];
    }
  }

  return 0;
}

template <bool is_max>
int rolling_extrema_position_pct_kernel(double* out, const double* in, size_t len, size_t n) {
  if (n == 0 || len == 0) return 1;

  std::fill(out, out + len, STRATEGYR::kNaReal);
  std::deque<size_t> q;
  size_t na_count = 0;

  for (size_t i = 0; i < len; ++i) {
    const double incoming = in[i];
    if (is_na(incoming)) {
      na_count += 1;
    } else {
      while (!q.empty()) {
        const double back_val = in[q.back()];
        const bool worse = is_max ? (back_val <= incoming) : (back_val >= incoming);
        if (!worse) break;
        q.pop_back();
      }
      q.push_back(i);
    }

    if (i >= n) {
      const size_t out_idx = i - n;
      if (is_na(in[out_idx])) {
        na_count -= 1;
      }
      while (!q.empty() && q.front() <= out_idx) {
        q.pop_front();
      }
    }

    if (i >= n - 1 && na_count == 0 && !q.empty()) {
      const size_t window_start = i + 1 - n;
      const double pos = static_cast<double>(q.front() - window_start + 1);
      out[i] = 100.0 * pos / static_cast<double>(n);
    }
  }

  return 0;
}

int rolling_linear_wma_kernel(double* out, const double* in, size_t len, size_t n) {
  if (n == 0 || len == 0) return 1;

  std::fill(out, out + len, STRATEGYR::kNaReal);
  if (n > len) return 0;

  const double denom = static_cast<double>(n) * static_cast<double>(n + 1) / 2.0;
  double window_sum = 0.0;
  double weighted_sum = 0.0;
  size_t na_count = 0;

  for (size_t i = 0; i < n; ++i) {
    if (is_na(in[i])) {
      na_count += 1;
    } else {
      window_sum += in[i];
      weighted_sum += static_cast<double>(i + 1) * in[i];
    }
  }

  if (na_count == 0) {
    out[n - 1] = weighted_sum / denom;
  }

  for (size_t end = n; end < len; ++end) {
    const double outgoing = in[end - n];
    const double incoming = in[end];
    const size_t old_na_count = na_count;
    const double prev_window_sum = window_sum;

    if (is_na(outgoing)) {
      na_count -= 1;
    } else {
      window_sum -= outgoing;
    }

    if (is_na(incoming)) {
      na_count += 1;
    } else {
      window_sum += incoming;
    }

    if (na_count != 0) continue;

    if (old_na_count == 0) {
      weighted_sum = weighted_sum - prev_window_sum + static_cast<double>(n) * incoming;
    } else {
      weighted_sum = 0.0;
      for (size_t j = end + 1 - n; j <= end; ++j) {
        weighted_sum += static_cast<double>(j - (end + 1 - n) + 1) * in[j];
      }
    }

    out[end] = weighted_sum / denom;
  }

  return 0;
}

void rolling_quantiles_kernel(
    double* out,           // length = len * P
    const double* in,      // length = len
    size_t len,
    size_t n,
    const double* probs,   // length = P
    size_t P
) {

  std::fill(out, out + (len * P), STRATEGYR::kNaReal);

  if (P == 0 || n == 0 || n > len) return;
  if (in == nullptr || out == nullptr || probs == nullptr) return;

  // Validate probs in [0, 1]
  for (size_t j = 0; j < P; ++j) {
    double p = probs[j];
    if (is_na(p) || p < 0.0 || p > 1.0) return;
  }

  // 1) Coordinate compression: collect non-NA values
  std::vector<double> vals;
  vals.reserve(len);
  for (size_t i = 0; i < len; ++i) {
    double xi = in[i];
    if (!is_na(xi)) vals.push_back(xi);
  }
  if (vals.empty()) return; // all NA

  std::sort(vals.begin(), vals.end());
  vals.erase(std::unique(vals.begin(), vals.end()), vals.end());
  const int M = (int)vals.size();

  Fenwick fw(M);

  // 2) Precompute rank per element (1..M), 0 for NA
  std::vector<int> rank(len, 0);
  for (size_t i = 0; i < len; ++i) {
    double xi = in[i];
    if (is_na(xi)) {
      rank[i] = 0;
    } else {
      auto it = std::lower_bound(vals.begin(), vals.end(), xi);
      rank[i] = (int)(it - vals.begin()) + 1; // 1..M
    }
  }

  int count_non_na = 0;

  for (size_t i = 0; i < len; ++i) {
    // add incoming
    if (rank[i] != 0) {
      fw.add(rank[i], +1);
      count_non_na += 1;
    }

    // remove outgoing once window is past n
    if (i >= n) {
      int ro = rank[i - n];
      if (ro != 0) {
        fw.add(ro, -1);
        count_non_na -= 1;
      }
    }

    // output once window is complete
    if (i >= n - 1) {
      if (count_non_na == 0) continue;

      for (size_t j = 0; j < P; ++j) {
        double p = probs[j];

        // type-1-ish: k = ceil(p*m), with clamps so p=0 -> 1, p=1 -> m
        int k;
        if (p <= 0.0) k = 1;
        else if (p >= 1.0) k = count_non_na;
        else k = (int)std::ceil(p * (double)count_non_na);

        if (k < 1) k = 1;
        if (k > count_non_na) k = count_non_na;

        int r = fw.find_by_order(k);
        double q = vals[(size_t)r - 1];

        // column-major: out(i, j) = out[i + len*j]
        out[i + len * j] = q;
      }
    }
  }
}
