// strat_buy_and_hold.h

// engine of strat_buy_and_hold
inline double buy_and_hold(double timestamp) {
	double tgt_pos = 1.0;
	return tgt_pos;
}

// kernel of strat_buy_and_hold
inline void strat_buy_and_hold(double* out_tgt_pos, const double* timestamp, size_t len) {
	for (size_t i = 0; i < len; ++i) {
		out_tgt_pos[i] = buy_and_hold(timestamp[i]);
	}
}

