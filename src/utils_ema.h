#ifndef UTILSEMA_H
#define UTILSEMA_H

#include <Rcpp.h>

// Exponential moving average utilities used by strategyr.
// All functions are also exported to R via Rcpp attributes
// in the corresponding .cpp implementation file.
		
// TTR-style EMA with optional Wilder smoothing, fixed time step.		
Rcpp::NumericVector ema_ttr_fixed_step(const Rcpp::NumericVector &x, int n, bool wilder);

// EMA defined via a continuous-time time constant tau, fixed time step.
Rcpp::NumericVector ema_tau_fixed_step(const Rcpp::NumericVector &x, double tau);

// EMA for irregular time intervals, given time vector t and tau.
Rcpp::NumericVector ema_tau_irregular(const Rcpp::NumericVector &t, const Rcpp::NumericVector &x, double tau);

#endif