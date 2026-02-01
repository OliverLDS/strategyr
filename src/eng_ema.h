// eng_ema.h

struct ema_fixed_step {
	double ema;
	double alpha;
	inline void update(double in) {ema = (in - ema) * alpha + ema;}
};

struct ema_irregular {
	double ema;
	double tau;
	size_t t;
	inline int update(double in, size_t new_t) {
		if (new_t <= t) return 1;
		double alpha = tau_to_alpha(tau * (new_t - t));
		t = new_t;
		ema = (in - ema) * alpha + ema;
		return 0;
	}
};