// base_types.h

#include <array>

struct ActionDecision { 
	
	int action_id;	// unique action id
	int strat;  // owner of this action decision
	// int reason; // strat reason will be useful, but right now let's ignore this
	
  STRATEGYR::ActionCode action = STRATEGYR::ActionCode::NONE;
  STRATEGYR::Dir dir = STRATEGYR::Dir::FLAT;
	STRATEGYR::OrderType type = STRATEGYR::OrderType::MARKET;
	double ctr_qty = STRATEGYR::kNaReal; // unit of contract to be placed in order;
	double px = STRATEGYR::kNaReal; // only for limit order - taking advantage of market inefficiency
 
};

static constexpr size_t MaxActions = 2; // right now we said one plan can at most have 2 actions in flip situation; if you want to make it flexible, you can use 'std::vector<ActionDecision> a;'

struct ActionPlan { 
  std::array<ActionDecision, MaxActions> a;
  size_t n = 0;
	bool append_action(const ActionDecision& action) {
	    if (n >= a.size()) return false;
	    a[n++] = action;
	    return true;
	}
};

struct Intent {
	int strat; // owner of intent
	// int reason; // strat reason will be useful, but right now let's ignore this
	
	double tgt_pos; // signed target exposure of the intent; pos here is dir * market value of position / (cash + unrealized pnl); it is for perps only right now; it doesn't consider startegies which will fix dir * entry value of position / (cash + entry value of position); we may extend it in the future
	double tol_pos = 0.0; // when abs(tgt_pos - cur_pos) < tol_pos, no action
	STRATEGYR::OrderType type = STRATEGYR::OrderType::MARKET; // entry type
	double px = STRATEGYR::kNaReal; // only for limit order - taking advantage of market inefficiency
	
	// some features
  bool has_tgt_pos() const noexcept { return (tgt_pos != 0.0 && !is_na(tgt_pos)); }
	STRATEGYR::Dir tgt_pos_dir() const noexcept {
	  return (tgt_pos > 0) ? STRATEGYR::Dir::LONG : ((tgt_pos < 0) ? STRATEGYR::Dir::SHORT : STRATEGYR::Dir::FLAT);
	}
	double tgt_pos_size() const noexcept { return std::abs(tgt_pos); }
};

static_assert(ActionPlan{}.n == 0, "ActionPlan must start empty"); // this sounds stupid here; it is just to make sure the default ActionPlan is not modified; or you just comment there that the defalt n can not be modified