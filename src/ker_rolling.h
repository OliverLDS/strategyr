// ker_rolling.h

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