// rcpp_strategies.cpp

#include <Rcpp.h>

#include "strat_buy_and_hold.h"

// [[Rcpp::export]]
Rcpp::NumericVector strat_buy_and_hold_rcpp(const Rcpp::NumericVector& timestamp) {
    size_t n = timestamp.size();
    Rcpp::NumericVector tgt_pos(n);

    strat_buy_and_hold(REAL(tgt_pos), REAL(timestamp), n);

    return tgt_pos;
}