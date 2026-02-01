// eng_exchange.h

// sub structs first

struct ExchangeMessage_on_funding {
	double timestamp;
	STRATEGYR::BarStage bar_stage = STRATEGYR::BarStage::CLOSE; // happened at the close of a bar
	double cash = STRATEGYR::kNaReal;
	bool liquidate = false;
};

struct ExchangeMessage_on_mark {
	double timestamp;
	STRATEGYR::BarStage bar_stage = STRATEGYR::BarStage::CLOSE; // happened at the close of a bar; but should calculated after funding fee
	double last_px = STRATEGYR::kNaReal;
	bool liquidate = false;
};

struct ExchangeMessage_on_trade { // recorder fields should be here
	int action_id; // it is actually the order id inherited from ActionDecision
	double timestamp;
	STRATEGYR::BarStage bar_stage; // we don't force it to be CLOSE, because for limit order or TP/SL order, it happens in INTRA
	int strat;	
	STRATEGYR::ActionCode action = STRATEGYR::ActionCode::NONE;
	STRATEGYR::Dir action_pos_dir = STRATEGYR::Dir::FLAT;
	double action_ctr_unit = STRATEGYR::kNaReal;
	double action_px = STRATEGYR::kNaReal;
	STRATEGYR::ActionStatus status = STRATEGYR::ActionStatus::PENDING;
	// above are all for recorder
	
	bool liquidate = false; // liquidate will be both recorded and also be used to modify tradestate
	
	// below are all for tradestate
	double cash = STRATEGYR::kNaReal;
	STRATEGYR::Dir pos_dir = STRATEGYR::Dir::FLAT;
	double ctr_unit = STRATEGYR::kNaReal;
	double avg_price = STRATEGYR::kNaReal;
};

// main struct of Exchange

struct Exchange {
	double old_timestamp = STRATEGYR::kNaReal; // time stamp of last bar
	double timestamp;
  double open;
	double high;
	double low;
  double close;
	void update_bar(double ts, double o, double h, double l, double c) { // a new bar arrives
		old_timestamp = timestamp;
		timestamp = ts;
		open = o;
		high = h;
		low = l;
		close = c;
	}
	bool limit_order_filled(double px) const noexcept {
		if (px > low && px < high) return true; // we don't include equal situation
		return false;
	};
	// bool is_tp(TradeState s) const noexcept;
	// bool is_sl(TradeState s) const noexcept;
	
	bool is_liquidated(const TradeState& s, double px) const noexcept;
	bool is_liquidated_intra_bar(const TradeState& s) const noexcept {
    return is_liquidated(s, high) || is_liquidated(s, low);
  }
	ExchangeMessage_on_funding update_on_funding(const TradeState& s) const noexcept;
  ExchangeMessage_on_mark    update_on_mark(const TradeState& s) const noexcept;
  ExchangeMessage_on_trade   update_on_trade(const TradeState& s,
                                             const ActionDecision& a,
                                             STRATEGYR::BarStage stage,
                                             double filled_price) const noexcept;
};

// helper

bool Exchange::is_liquidated(const TradeState& s, double px) const noexcept {
	double floating_pnl = s.unrealized_pnl(px);
	return s.cash + floating_pnl < s.mm();
}

// calculate funding fees

ExchangeMessage_on_funding Exchange::update_on_funding(const TradeState& s) const noexcept { // dt should be something new.timestamp - old.timestamp; return funding_fee, so we can deduct from cash here
	ExchangeMessage_on_funding out;
	out.timestamp = timestamp;
	
	if (is_na(old_timestamp)) {
		out.cash = s.cash;
		return out;
	}
	
	const double dt = timestamp - old_timestamp;
	out.cash = s.cash - s.fund_rt * dt * s.abs_notional() / (60*60*8.0); // timestamp's unit is second, so s.fund_rt should be the funding fee rate per 8 hours here (we use hard code for simplicity)
	if (out.cash + s.unrealized_pnl() < s.mm()) {
		out.cash = 0.0;
		out.liquidate = true; // so it is possible to record even without action id (triggered by liquidation event)
	}
	return out;
}

// update floating pnl

ExchangeMessage_on_mark Exchange::update_on_mark(const TradeState& s) const noexcept { // happended at the close of a bar
	ExchangeMessage_on_mark out;
	out.timestamp = timestamp;
	out.last_px = close;
	if (is_liquidated(s, close)) out.liquidate = true;
	return out;
}

// update after transaction executed

ExchangeMessage_on_trade Exchange::update_on_trade(const TradeState& s, const ActionDecision& a, STRATEGYR::BarStage stage, double filled_price) const noexcept { 
	ExchangeMessage_on_trade out;
	out.timestamp = timestamp; // executed time, not action submitted time
	out.bar_stage = stage;
	
	// receive fields of ActionDecision
	out.action_id = a.action_id;
	out.strat = a.strat;
	out.action = a.action;
	out.action_pos_dir = a.dir;
	out.action_ctr_unit = a.ctr_qty;
	out.action_px = filled_price; // so it is actually not necessarily px in action
	
	// first, you need to make sure it is not liquidated (at filled_price); this one may be better to be separated
	if (is_liquidated(s, filled_price)) { // tradestate will call recorder engine to record liquidation or successful transaction
		out.status = STRATEGYR::ActionStatus::FAILED;
		out.liquidate = true;
		return out;
	}
	
	const double trade_units   = a.ctr_qty * s.ctr_size;
  const double trade_notional = trade_units * filled_price;
  const double fee = s.fee_rt * trade_notional;
	
	// now check whether the order can be executed; open and increase, cash >= imr; 
	if (a.action == STRATEGYR::ActionCode::OPEN || a.action == STRATEGYR::ActionCode::INCREASE) {
		const double floating_pnl = s.unrealized_pnl(filled_price);
    const double equity_after_fee = s.cash + floating_pnl - fee;
    const double new_abs_notional =
      std::abs(s.abs_notional() + trade_notional * (a.dir == STRATEGYR::Dir::LONG ? 1.0 : -1.0));

    const double required_margin = new_abs_notional * s.imr();
				
		if (equity_after_fee < required_margin) {
			out.status = STRATEGYR::ActionStatus::FAILED;
			return out;
		}
		// if it is not liquidated
		out.status = STRATEGYR::ActionStatus::FILLED;
		out.cash = s.cash - fee;
		out.pos_dir = a.dir;
		if (a.action == STRATEGYR::ActionCode::OPEN) {
			out.ctr_unit = a.ctr_qty;
			out.avg_price = filled_price;
		} else { // INCREASE
			out.ctr_unit = s.ctr_unit + a.ctr_qty;
			out.avg_price = (s.ctr_unit*s.avg_price + a.ctr_qty*filled_price)/out.ctr_unit;
		}
		return out;
	}
	
	// CLOSE, no requirement because mmr > trading fee
	if (a.action == STRATEGYR::ActionCode::CLOSE) {
		out.status = STRATEGYR::ActionStatus::FILLED;
		
		const double trade_units    = a.ctr_qty * s.ctr_size;      // always positive
	  const double trade_notional = trade_units * filled_price;  // > 0
	  const double fee_close      = s.fee_rt * trade_notional;
		const double dir_sign = static_cast<double>(s.pos_dir);    // -1, 0, +1
		const double realized_pnl =
		    trade_units * (filled_price - s.avg_price) * dir_sign;
			
		out.cash = s.cash - fee_close + realized_pnl;
		out.pos_dir = STRATEGYR::Dir::FLAT;
		out.ctr_unit = 0.0;
		out.avg_price = STRATEGYR::kNaReal;
		
		return out;
	}
	
	// REDUCE, i think no requirement because mmr > trading fee;
	if (a.action == STRATEGYR::ActionCode::REDUCE) {
		out.status = STRATEGYR::ActionStatus::FILLED;
		
		const double trade_units    = a.ctr_qty * s.ctr_size;
	  const double trade_notional = trade_units * filled_price;
	  const double fee_reduce     = s.fee_rt * trade_notional;

	  const double dir_sign = static_cast<double>(s.pos_dir);
	  const double realized_pnl =
	    trade_units * (filled_price - s.avg_price) * dir_sign;
			
		out.cash     = s.cash - fee_reduce + realized_pnl;
		out.pos_dir = a.dir; // it is also s.pos_dir
		out.ctr_unit = s.ctr_unit - a.ctr_qty; // i think in action_from_intent we already make sure s.ctr_unit is greater than a.ctr_qty
		out.avg_price = s.avg_price; // no change in avg_price for REDUCE
		
		return out;
	}
	
	out.status = STRATEGYR::ActionStatus::FAILED;
	return out;
}

