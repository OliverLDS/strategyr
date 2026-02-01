// ker_ema.h

int ema_ttr_fixed_step_kernel(double* out, const double* in, size_t len, size_t n, bool wilder) {
	if (n == 0 || len == 0) return 1;
	double sum = 0;
	ema_fixed_step ema_engine;
	ema_engine.alpha = wilder ? n_to_alpha_wilder(n) : n_to_alpha(n);
	
	for (size_t i = 0; i < len; ++i) {
		if (i < n) {
			sum += in[i];
			ema_engine.ema = sum/(i+1); // initialize as sum/n when i == n-1
			out[i] = (i == n-1) ? ema_engine.ema : STRATEGYR::kNaReal;
		} else {
			ema_engine.update(in[i]);
			out[i] = ema_engine.ema;
		}
	}
	return 0;
}

int ema_fixed_step_kernel(double* out, const double* in, size_t len, double tau) {
	if (len == 0 || tau <= 0.0) return 1;
	ema_fixed_step ema_engine;
	ema_engine.alpha = tau_to_alpha(tau);
	
	// initialize
	ema_engine.ema = in[0];
	out[0] = ema_engine.ema;
	
  for (size_t i = 1; i < len; ++i) {
		ema_engine.update(in[i]);
		out[i] = ema_engine.ema;
	}
	return 0;
}

int ema_tau_irregular_kernel(double* out, const double* in, const size_t* t, size_t len, double tau) {
	if (len == 0 || tau <= 0.0) return 1;
	ema_irregular ema_engine;
	ema_engine.tau = tau;
	
	// initialize
	ema_engine.ema = in[0];
	ema_engine.t = t[0];
	out[0] = ema_engine.ema;
	
  for (size_t i = 1; i < len; ++i) {
		if (ema_engine.update(in[i], t[i])) return 1;
		out[i] = ema_engine.ema;
	}
	return 0;
}