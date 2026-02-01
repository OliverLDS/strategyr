// eng_recorder.h

#include <vector>

struct Recorder { // the reason we don't need two recorders is we can control it in backtest engine
  std::vector<double> ts;
	std::vector<STRATEGYR::BarStage> bar_stage;
  std::vector<int> strat_id_vec;
  size_t tx_id = 0; // open action -> tx_id++; so we can calculate win rate 
  std::vector<size_t> tx_id_vec;
	std::vector<STRATEGYR::ActionStatus> status;
	std::vector<bool> liquidation;
  std::vector<STRATEGYR::ActionCode> action;
  std::vector<STRATEGYR::Dir> dir;
  std::vector<double> ctr_qty;  
  std::vector<double> price;
	std::vector<double> eq_vec;

  inline void reserve(size_t n) {
    ts.reserve(n);
		bar_stage.reserve(n);
    strat_id_vec.reserve(n);
    tx_id_vec.reserve(n);
		status.reserve(n);
		liquidation.reserve(n);
    action.reserve(n);
    dir.reserve(n);
    ctr_qty.reserve(n);
    price.reserve(n);
		eq_vec.reserve(n);
  }

  inline void append_record(TradeState s, ExchangeMessage_on_trade m) {
    ts.push_back(m.timestamp);
		bar_stage.push_back(m.bar_stage);
    strat_id_vec.push_back(m.strat);

    if (m.action == STRATEGYR::ActionCode::OPEN) { ++tx_id; }
    tx_id_vec.push_back(tx_id);
		
		status.push_back(m.status);
		liquidation.push_back(m.liquidate);
    action.push_back(m.action);
    dir.push_back(m.action_pos_dir);
		ctr_qty.push_back(m.action_ctr_unit);
    price.push_back(m.action_px);
		eq_vec.push_back(s.eq());
  }
	
	inline void append_liquidation(double timestamp, STRATEGYR::BarStage stage) {
    ts.push_back(timestamp);
		bar_stage.push_back(stage);
		eq_vec.push_back(0.0);
	}
};

