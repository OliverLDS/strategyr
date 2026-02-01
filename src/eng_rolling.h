// eng_rolling.h

struct rolling_1st_moment {
	double rolling_sum = 0;
	double inv_n; // multiplication is cheaper than division
	inline void update(double incoming, double leaving) {rolling_sum = rolling_sum + incoming - leaving;}
	inline double mean() const {return rolling_sum * inv_n;}
};

struct rolling_2nd_moment {
	double rolling_sum = 0;
	double rolling_sum_sq = 0;
	double inv_n;
	double inv_denom;
	inline void update(double incoming, double leaving) {
		rolling_sum = rolling_sum + incoming - leaving;
		rolling_sum_sq = rolling_sum_sq + incoming * incoming - leaving * leaving;
	}
	inline double sd() const {
		double var = (rolling_sum_sq - rolling_sum * rolling_sum * inv_n) * inv_denom;
		if (var < 0.0) {var = 0.0;}  // guard tiny negative due to rounding
		return std::sqrt(var);
	}
};

struct Fenwick {
  int n;
  std::vector<int> bit;

  explicit Fenwick(int n_) : n(n_), bit(n_ + 1, 0) {}

  inline void add(int idx, int delta) { // idx: 1..n
    for (; idx <= n; idx += idx & -idx) bit[idx] += delta;
  }

  inline int find_by_order(int k) const {
    int idx = 0;
    int bitMask = 1;
    while ((bitMask << 1) <= n) bitMask <<= 1;

    for (int step = bitMask; step != 0; step >>= 1) {
      int next = idx + step;
      if (next <= n && bit[next] < k) {
        idx = next;
        k -= bit[next];
      }
    }
    return idx + 1;
  }
};

