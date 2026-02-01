#include <cassert>
#include <cmath>
#include <iostream>

// include your headers (adjust paths/names as in your project)
#include "../src/base_utils.h"
#include "../src/base_ids.h"
#include "../src/base_types.h"
#include "../src/eng_tradestate.h"
#include "../src/eng_exchange.h"
#include "../src/eng_recorder.h"
#include "../src/ker_backtest.h"

// simple assert helpers
inline void assert_true(const char* label, bool cond) {
    if (!cond) {
        std::cerr << label << " FAILED\n";
        std::exit(1);
    }
}

inline void assert_eq_int(const char* label, std::size_t actual, std::size_t expected) {
    if (actual != expected) {
        std::cerr << label << " FAILED:\n"
                  << "  actual   = " << actual << "\n"
                  << "  expected = " << expected << "\n";
        std::exit(1);
    }
}

inline void assert_near(const char* label,
                        double actual,
                        double expected,
                        double tol = 1e-8) {
    if (!std::isfinite(actual) || !std::isfinite(expected)) {
        std::cerr << label << " FAILED: non-finite value\n"
                  << "  actual   = " << actual << "\n"
                  << "  expected = " << expected << "\n";
        std::exit(1);
    }

    double diff = std::fabs(actual - expected);
    if (diff > tol) {
        std::cerr << label << " FAILED:\n"
                  << "  actual   = " << actual << "\n"
                  << "  expected = " << expected << "\n"
                  << "  diff     = " << diff << "\n"
                  << "  tol      = " << tol << "\n";
        std::exit(1);
    }
}

TradeState make_base_state() {
    TradeState s{};
    s.strat     = StratID::LOW_ATR5_CROSS;
    s.asset     = AssetID::BTC_USDT_SWAP;
    s.ctr_size  = 1.0;
    s.ctr_step  = 1.0;
    s.lev       = 10.0;
    s.mmr       = 0.0;
    s.fee_rt    = 0.0;
    s.fund_rt   = 0.0;
    s.ctr_unit  = 0.0;
    s.pos_dir   = STRATEGYR::Dir::FLAT;
    s.avg_price = STRATEGYR::kNaReal;
    s.cash      = 10000.0;
    s.liquidated = false;
    s.pending   = ActionPlan{};
    s.action_id_now = 1;
    s.last_px   = 100.0;  // important for delta_pos_to_ctr
    return s;
}

void test_open_from_flat() {
    TradeState s = make_base_state();

    Intent intent{};
    intent.strat   = StratID::LOW_ATR5_CROSS;
    intent.tgt_pos = 0.01;   // equity ~10000, price ~100 => 1 contract
    intent.tol_pos = 0.0;

    ActionPlan plan = s.plan_action_mkt_ord(intent, /*action_id=*/1);

    assert_eq_int("open_from_flat: plan.n", plan.n, 1);
    const ActionDecision& a = plan.a[0];

    assert_true("open_from_flat: action == OPEN",
                a.action == STRATEGYR::ActionCode::OPEN);
    assert_true("open_from_flat: dir == LONG",
                a.dir == STRATEGYR::Dir::LONG);
    assert_near("open_from_flat: ctr_qty == 1", a.ctr_qty, 1.0);

    std::cout << "test_open_from_flat passed\n";
}

void test_flatten_from_long() {
    TradeState s = make_base_state();

    // simulate holding 1 contract long at avg 100, current price 110
    s.ctr_unit  = 1.0;
    s.pos_dir   = STRATEGYR::Dir::LONG;
    s.avg_price = 100.0;
    s.last_px   = 110.0;

    Intent intent{};
    intent.strat   = StratID::LOW_ATR5_CROSS;
    intent.tgt_pos = 0.0;   // flatten
    intent.tol_pos = 0.0;

    ActionPlan plan = s.plan_action_mkt_ord(intent, /*action_id=*/1);

    assert_eq_int("flatten_from_long: plan.n", plan.n, 1);
    const ActionDecision& a = plan.a[0];

    assert_true("flatten_from_long: action == CLOSE",
                a.action == STRATEGYR::ActionCode::CLOSE);
    assert_true("flatten_from_long: dir == FLAT",
                a.dir == STRATEGYR::Dir::FLAT);
    assert_near("flatten_from_long: ctr_qty == full position",
                a.ctr_qty, std::abs(s.ctr_unit));

    std::cout << "test_flatten_from_long passed\n";
}

void test_increase_same_direction() {
    TradeState s = make_base_state();

    // 1 contract long at 100, last_px = 100 => eq == 10000, cur_pos ~0.01
    s.ctr_unit  = 1.0;
    s.pos_dir   = STRATEGYR::Dir::LONG;
    s.avg_price = 100.0;
    s.last_px   = 100.0;

    Intent intent{};
    intent.strat   = StratID::LOW_ATR5_CROSS;
    intent.tgt_pos = 0.02;   // want ~2 contracts
    intent.tol_pos = 0.0;

    ActionPlan plan = s.plan_action_mkt_ord(intent, /*action_id=*/1);

    assert_eq_int("increase_same_dir: plan.n", plan.n, 1);
    const ActionDecision& a = plan.a[0];

    assert_true("increase_same_dir: action == INCREASE",
                a.action == STRATEGYR::ActionCode::INCREASE);
    assert_true("increase_same_dir: dir == LONG",
                a.dir == STRATEGYR::Dir::LONG);
    assert_near("increase_same_dir: ctr_qty == 1", a.ctr_qty, 1.0);

    std::cout << "test_increase_same_direction passed\n";
}

void test_reduce_same_direction() {
    TradeState s = make_base_state();

    // 2 contracts long at 100, last_px = 100
    s.ctr_unit  = 2.0;
    s.pos_dir   = STRATEGYR::Dir::LONG;
    s.avg_price = 100.0;
    s.last_px   = 100.0;

    // cur_pos ~ 0.02. We want 0.01 => reduce by ~1 contract
    Intent intent{};
    intent.strat   = StratID::LOW_ATR5_CROSS;
    intent.tgt_pos = 0.01;
    intent.tol_pos = 0.0;

    ActionPlan plan = s.plan_action_mkt_ord(intent, /*action_id=*/1);

    assert_eq_int("reduce_same_dir: plan.n", plan.n, 1);
    const ActionDecision& a = plan.a[0];

    assert_true("reduce_same_dir: action == REDUCE",
                a.action == STRATEGYR::ActionCode::REDUCE);
    assert_true("reduce_same_dir: dir == LONG",
                a.dir == STRATEGYR::Dir::LONG);
    assert_near("reduce_same_dir: ctr_qty == 1", a.ctr_qty, 1.0);

    std::cout << "test_reduce_same_direction passed\n";
}

void test_flip_long_to_short() {
    TradeState s = make_base_state();

    // 1 contract long at 100, last_px = 110
    s.ctr_unit  = 1.0;
    s.pos_dir   = STRATEGYR::Dir::LONG;
    s.avg_price = 100.0;
    s.last_px   = 110.0;

    Intent intent{};
    intent.strat   = StratID::LOW_ATR5_CROSS;
    intent.tgt_pos = -0.01;   // want short
    intent.tol_pos = 0.0;

    ActionPlan plan = s.plan_action_mkt_ord(intent, /*action_id=*/10);

    assert_eq_int("flip_long_to_short: plan.n", plan.n, 2);

    const ActionDecision& c = plan.a[0];
    const ActionDecision& o = plan.a[1];

    // First: CLOSE
    assert_true("flip_long_to_short: first action CLOSE",
                c.action == STRATEGYR::ActionCode::CLOSE);
    assert_true("flip_long_to_short: first dir FLAT",
                c.dir == STRATEGYR::Dir::FLAT);
    assert_near("flip_long_to_short: close full position",
                c.ctr_qty, std::abs(s.ctr_unit));
    assert_true("flip_long_to_short: first action_id == 10",
                c.action_id == 10);

    // Second: OPEN in opposite direction
    assert_true("flip_long_to_short: second action OPEN",
                o.action == STRATEGYR::ActionCode::OPEN);
    assert_true("flip_long_to_short: second dir SHORT",
                o.dir == STRATEGYR::Dir::SHORT);
    assert_true("flip_long_to_short: second action_id == 11",
                o.action_id == 11);

    std::cout << "test_flip_long_to_short passed\n";
}

void test_flip_short_to_long() {
    TradeState s = make_base_state();

    // 1 contract short at 100, last_px = 90
    s.ctr_unit  = 1.0;
    s.pos_dir   = STRATEGYR::Dir::SHORT;
    s.avg_price = 100.0;
    s.last_px   = 90.0;

    Intent intent{};
    intent.strat   = StratID::LOW_ATR5_CROSS;
    intent.tgt_pos = 0.01;   // want long
    intent.tol_pos = 0.0;

    ActionPlan plan = s.plan_action_mkt_ord(intent, /*action_id=*/20);

    assert_eq_int("flip_short_to_long: plan.n", plan.n, 2);

    const ActionDecision& c = plan.a[0];
    const ActionDecision& o = plan.a[1];

    assert_true("flip_short_to_long: first action CLOSE",
                c.action == STRATEGYR::ActionCode::CLOSE);
    assert_true("flip_short_to_long: first dir FLAT",
                c.dir == STRATEGYR::Dir::FLAT);
    assert_near("flip_short_to_long: close full position",
                c.ctr_qty, std::abs(s.ctr_unit));

    assert_true("flip_short_to_long: second action OPEN",
                o.action == STRATEGYR::ActionCode::OPEN);
    assert_true("flip_short_to_long: second dir LONG",
                o.dir == STRATEGYR::Dir::LONG);

    std::cout << "test_flip_short_to_long passed\n";
}

void test_tolerance_no_trade() {
    TradeState s = make_base_state();

    // 1 contract long at 100, price 100 -> eq=10000, cur_pos ~0.01
    s.ctr_unit  = 1.0;
    s.pos_dir   = STRATEGYR::Dir::LONG;
    s.avg_price = 100.0;
    s.last_px   = 100.0;

    const double cur_pos = s.cur_pos();  // ~0.01

    Intent intent{};
    intent.strat   = StratID::LOW_ATR5_CROSS;
    intent.tgt_pos = cur_pos + 0.0001;
    intent.tol_pos = 0.001; // tolerance bigger than gap

    ActionPlan plan = s.plan_action_mkt_ord(intent, /*action_id=*/1);

    assert_eq_int("tolerance_no_trade: plan.n", plan.n, 0);

    std::cout << "test_tolerance_no_trade passed\n";
}

void test_na_target_no_trade() {
    TradeState s = make_base_state();

    Intent intent{};
    intent.strat   = StratID::LOW_ATR5_CROSS;
    intent.tgt_pos = STRATEGYR::kNaReal;
    intent.tol_pos = 0.0;

    ActionPlan plan = s.plan_action_mkt_ord(intent, /*action_id=*/1);

    assert_eq_int("na_target_no_trade: plan.n", plan.n, 0);

    std::cout << "test_na_target_no_trade passed\n";
}

void test_ctr_step_rounding() {
    TradeState s = make_base_state();

    // no position yet
    s.ctr_step = 1.0;   // whole contracts
    s.last_px  = 100.0;
    s.cash     = 10000.0;

    // Very small target: gap_notional = 0.0001 * 10000 = 1
    // raw_ctr = 1 / (1 * 100) = 0.01 -> rounded to 0 contracts
    Intent intent{};
    intent.strat   = StratID::LOW_ATR5_CROSS;
    intent.tgt_pos = 0.0001;
    intent.tol_pos = 0.0;

    ActionPlan plan = s.plan_action_mkt_ord(intent, /*action_id=*/1);

    assert_eq_int("ctr_step_rounding: plan.n", plan.n, 0);

    std::cout << "test_ctr_step_rounding passed\n";
}

int main() {
    test_open_from_flat();
    test_flatten_from_long();
    test_increase_same_direction();
    test_reduce_same_direction();
    test_flip_long_to_short();
    test_flip_short_to_long();
    test_tolerance_no_trade();
    test_na_target_no_trade();
    test_ctr_step_rounding();

    std::cout << "All TradeState::plan_action_mkt_ord tests passed.\n";
    return 0;
}
