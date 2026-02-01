// eng_tradestate.h

// tradestate is usded in both backtesting and live order; in backtesting, it is an struct stored in the loop over bars; between the close of this bar and open of next bar, there is pending ActionPlan; in live order, these variables are stored in agent's mind_state; pending ActionPlan will be usually placed immediately; but we can still maintain this structure; the additional step of write into and clear pending is basically harmless

struct TradeState {
  int strat = 0;      							// which strategy for this backtesting or live order; it is different from strat in recorder which is the owner of action; it could be same as or different from the stategy name exported to backtesting results
	
  int asset = 0;										// internal asset_id; we use asset instead of asset_id to make the naming concise; here we should stick to asset chosen in TradeState, otherwise we need a data table here to let each asset have their own pos and avg_price, which is too complicated for now
	// asset-level parameters
	double ctr_size = 1.0;       			// how many underlying units per contract
	double ctr_step = 0.01; 					// smallest tradable fraction of a contract
	double lev = 1.0;									// in reality, long and short can have different leverage; now we just use unified one
	double mmr = 0.02;								// unrealized_pnl is the leveraged exposure; in cross mode, if cash can not cover it, it will liquidate the account; mmr is an aditional requirement that cash + unrealized_pnl > mmr * notational (or initial notational?); how about isolate mode, is it preallocate some cash here as buffer?
	
	// account-level parameters
	double fee_rt = 0.0005;    				// trade fee rate on notional
	double fund_rt = 0.0004;   				// funding rate per 8 hours; also on notional

  // asset holding
	double ctr_unit = 0.0;						// unit of contract holded; std::round(pos_unit * pos_unit_per_contract / min_contract_unit) * min_contract_unit;
	STRATEGYR::Dir pos_dir = STRATEGYR::Dir::FLAT; // -1, 0, 1
	double avg_price = STRATEGYR::kNaReal;       // weighted entry price for realized PnL / TP/SL
	double cash = 10000.0; 								// the cash amount
	bool liquidated = false;
	
  // next-open scheduled actions
  ActionPlan pending;       				// pending.n == 0 means no scheduled action
	size_t action_id_now = 1;
	
	// for accounting
	double last_px = 0; // current mark/close price

  // features	
	inline double pos_units() const noexcept { return ctr_size * ctr_unit * (double)pos_dir; }  	// signed units of pos
	inline double notional() const noexcept { return pos_units() * last_px; }  										// signed notional
	inline double abs_notional() const noexcept { return std::abs(notional()); }
	inline double unrealized_pnl(double px) const noexcept {
	  if (!has_pos() || std::isnan(avg_price)) return 0.0;
	  return (px - avg_price) * pos_units();
	}
	inline double unrealized_pnl() const noexcept {
    return unrealized_pnl(last_px);
  }
	inline double eq() const noexcept { return cash + unrealized_pnl(); }
	
	inline double cur_pos() const noexcept { 
		double e = eq();
  	if (e <= 0.0) return STRATEGYR::kInfReal;
  	return notional() / e; 
	}                   				// signed exposure ratio
	inline double abs_cur_pos() const noexcept { return std::abs(cur_pos()); }							
	inline bool has_pos() const noexcept { 
		double pu = pos_units();
  	return (pu != 0.0 && !is_na(pu)); 
	}
	
	inline double imr() const noexcept { return 1/lev; }
	inline double mm() const noexcept { return abs_notional()*mmr; }
	
	// updates
	
	double delta_pos_to_ctr(double delta_pos) const noexcept;
	ActionPlan plan_action_mkt_ord(const Intent& intent, size_t action_id) const noexcept;
};

inline double TradeState::delta_pos_to_ctr(double delta_pos) const noexcept {
  double e = eq();
  if (e <= 0.0 || is_na(last_px)) return 0.0; // no need to consider delta_pos == 0 here; delta_pos is already greater than tol_pos

  double gap_notional = delta_pos * e;
  double raw_ctr = gap_notional / (ctr_size * last_px);

  double stepped_ctr = std::round(raw_ctr / ctr_step) * ctr_step;
  return stepped_ctr; // signed contracts
}

inline ActionPlan TradeState::plan_action_mkt_ord(const Intent& intent, size_t action_id) const noexcept {
  ActionPlan plan{}; // default n == 0

  // no opinion or tolerance
  if (is_na(intent.tgt_pos)) return plan;

  const double gap_pos = intent.tgt_pos - cur_pos();

  // within tolerance => no trade
  if (std::abs(gap_pos) < intent.tol_pos) return plan;

  // convert exposure gap to contracts
  double ctr_delta = delta_pos_to_ctr(gap_pos); // signed
  if (ctr_delta == 0.0) return plan;

	STRATEGYR::Dir cur_dir = pos_dir;
	STRATEGYR::Dir tgt_dir = intent.tgt_pos_dir();
	
  if (!has_pos()) {
    // simple OPEN
    ActionDecision o;
		o.action_id = action_id;
		o.strat   = intent.strat;
		
    o.action  = STRATEGYR::ActionCode::OPEN;
    o.dir     = tgt_dir;
		// o.type 		= STRATEGYR::OrderType::MARKET;
		o.ctr_qty = std::abs(ctr_delta);
    
    plan.a[0] = o;
    plan.n    = 1;
    return plan;
  }
	
	if (!intent.has_tgt_pos()) { // we already return na earlier; so here is only when tgt_pos is zero
    ActionDecision c;
		c.action_id = action_id;
		c.strat    = intent.strat;
		
    c.action   = STRATEGYR::ActionCode::CLOSE;
    c.dir      = STRATEGYR::Dir::FLAT;
    c.ctr_qty  = std::abs(ctr_unit);   // close full position in contracts
		
    plan.a[0] = c;
    plan.n    = 1;
		return plan;
	}

  // flip: current dir != target dir and both non-flat
  if (tgt_dir != STRATEGYR::Dir::FLAT && cur_dir != tgt_dir) { // both cur_dir and tgt_dir can not be FLAT if the program runs here, otherwise has_pos()==false

    // For simplicity: first CLOSE everything, then OPEN to target
    ActionDecision c;
		c.action_id = action_id;
		c.strat    = intent.strat;
		
    c.action   = STRATEGYR::ActionCode::CLOSE;
    c.dir      = STRATEGYR::Dir::FLAT;
    c.ctr_qty  = std::abs(ctr_unit);   // close full position in contracts
		
    plan.a[0] = c;
    plan.n    = 1;
    
    // contracts needed to go from 0 -> tgt
    double ctr_for_tgt = delta_pos_to_ctr(intent.tgt_pos);
		if (ctr_for_tgt == 0.0) return plan; // it becomes a CLOSE plan here
		
		// now OPEN the new one
    ActionDecision o;
		o.action_id = action_id + (size_t)1;
		o.strat    = intent.strat;
		
    o.action   = STRATEGYR::ActionCode::OPEN;
    o.dir      = tgt_dir;
		o.ctr_qty  = std::abs(ctr_for_tgt);
    
    plan.a[1] = o;
    plan.n    = 2;
    return plan;
  }

  // same direction: increase or reduce
  ActionDecision d;
	d.action_id = action_id;
	d.strat   = intent.strat;
	
  if (intent.tgt_pos_size() > abs_cur_pos()) {
    d.action = STRATEGYR::ActionCode::INCREASE;
  } else {
    d.action = STRATEGYR::ActionCode::REDUCE;
  }
	
  d.dir     = tgt_dir;
  d.ctr_qty = std::abs(ctr_delta);

  plan.a[0] = d;
  plan.n    = 1;
  return plan;
}
