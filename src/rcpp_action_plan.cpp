// rcpp_action_plan.cpp

#include <Rcpp.h>

#include "base_utils.h"
#include "base_ids.h"
#include "base_types.h"
#include "eng_tradestate.h"


// [[Rcpp::export]]
Rcpp::List gen_action_plan_rcpp(
		double ctr_size,
		double ctr_step,
		double lev,
    double last_px,
    double ctr_unit,
    double avg_price,
    double cash,
    double tgt_pos,
    double tol_pos,
    int strat_id,
    int pos_dir = 0    // -1 = SHORT, 0 = FLAT, 1 = LONG
) {
    TradeState s{};
		s.ctr_size = ctr_size;
		s.ctr_step = ctr_step;
		s.lev = lev;
    s.last_px  = last_px;
    s.ctr_unit = ctr_unit;
    s.avg_price = avg_price;
    s.cash = cash;
    s.pos_dir = static_cast<STRATEGYR::Dir>(pos_dir);

    Intent intent{};
    intent.strat   = strat_id;
    intent.tgt_pos = tgt_pos;
    intent.tol_pos = tol_pos;

    ActionPlan p = s.plan_action_mkt_ord(intent, 1);

    Rcpp::List out;
    out["n"] = static_cast<int>(p.n);

    Rcpp::List actions(p.n);
    for (std::size_t i = 0; i < p.n; ++i) {
        const ActionDecision &a = p.a[i];
        actions[i] = Rcpp::List::create(
            Rcpp::Named("action_id") = a.action_id,
            Rcpp::Named("strat")     = a.strat,
            Rcpp::Named("action")    = static_cast<int>(a.action),
            Rcpp::Named("dir")       = static_cast<int>(a.dir),
            Rcpp::Named("type")      = static_cast<int>(a.type),
            Rcpp::Named("ctr_qty")   = a.ctr_qty,
            Rcpp::Named("px")        = a.px
        );
    }

    out["actions"] = actions;
    return out;
}
