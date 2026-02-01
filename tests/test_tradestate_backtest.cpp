#include <cassert>
#include <cmath>
#include <iostream>

// include your headers (adjust paths if needed)
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

// ----------------------------------------------------------------------
// 1) simple long open-and-hold
// ----------------------------------------------------------------------
void test_simple_long_open_and_hold() {
    constexpr std::size_t len = 3;

    double timestamp[len] = { 0.0, 3600.0, 7200.0 };

    double open[len]  = { 100.0, 100.0, 110.0 };
    double high[len]  = { 100.0, 110.0, 110.0 };
    double low[len]   = { 100.0, 100.0, 110.0 };
    double close[len] = { 100.0, 110.0, 110.0 };

    // Want exposure ~0.01 => 1 contract
    double tgt_pos[len] = { 0.01, 0.01, 0.01 };

    int pos_strat[len] = {
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS
    };

    double tol_pos[len] = { 0.0, 0.0, 0.0 };

    double eq[len] = {0.0, 0.0, 0.0};

    Recorder recorder{};

    backtest(
        eq,
        timestamp, open, high, low, close,
        tgt_pos, pos_strat, tol_pos,
        StratID::LOW_ATR5_CROSS,
        AssetID::BTC_USDT_SWAP,
        1.0,   // ctr_size
        1.0,   // ctr_step
        10.0,  // lev
        0.0,   // fee_rt
        0.0,   // fund_rt
        len,
        false, // rec
        &recorder
    );

    std::cout << "eq path (test_simple_long_open_and_hold):\n";
    for (std::size_t i = 0; i < len; ++i) {
        std::cout << "  eq[" << i << "] = " << eq[i] << "\n";
    }

    assert_near("test_simple_long_open_and_hold: eq[0]", eq[0], 10000.0);
    assert_near("test_simple_long_open_and_hold: eq[1]", eq[1], 10010.0);
    assert_near("test_simple_long_open_and_hold: eq[2]", eq[2], 10010.0);

    std::cout << "test_simple_long_open_and_hold passed\n";
}

// ----------------------------------------------------------------------
// 2) flatten to zero
// ----------------------------------------------------------------------
void test_flatten_to_zero() {
    constexpr std::size_t len = 3;

    double timestamp[len] = { 0.0, 3600.0, 7200.0 };

    double open[len]  = { 100.0, 100.0, 110.0 };
    double high[len]  = { 100.0, 110.0, 110.0 };
    double low[len]   = { 100.0, 100.0, 110.0 };
    double close[len] = { 100.0, 110.0, 110.0 };

    double tgt_pos[len] = { 0.01, 0.0, 0.0 };

    int pos_strat[len] = {
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS
    };

    double tol_pos[len] = { 0.0, 0.0, 0.0 };

    double eq[len] = {0.0, 0.0, 0.0};

    Recorder recorder{};

    backtest(
        eq,
        timestamp, open, high, low, close,
        tgt_pos, pos_strat, tol_pos,
        StratID::LOW_ATR5_CROSS,
        AssetID::BTC_USDT_SWAP,
        1.0,   // ctr_size
        1.0,   // ctr_step
        10.0,  // lev
        0.0,   // fee_rt
        0.0,   // fund_rt
        len,
        false,
        &recorder
    );

    std::cout << "eq path (test_flatten_to_zero):\n";
    for (std::size_t i = 0; i < len; ++i) {
        std::cout << "  eq[" << i << "] = " << eq[i] << "\n";
    }

    assert_near("test_flatten_to_zero: eq[0]", eq[0], 10000.0);
    assert_near("test_flatten_to_zero: eq[1]", eq[1], 10010.0);
    assert_near("test_flatten_to_zero: eq[2]", eq[2], 10010.0);

    std::cout << "test_flatten_to_zero passed\n";
}

// ----------------------------------------------------------------------
// 3) simple short open-and-hold
// ----------------------------------------------------------------------
void test_simple_short_open_and_hold() {
    constexpr std::size_t len = 3;

    double timestamp[len] = { 0.0, 3600.0, 7200.0 };

    double open[len]  = { 100.0, 100.0, 90.0 };
    double high[len]  = { 100.0, 100.0, 90.0 };
    double low[len]   = { 100.0, 90.0, 90.0 };
    double close[len] = { 100.0, 90.0, 90.0 };

    double tgt_pos[len] = { -0.01, -0.01, -0.01 };

    int pos_strat[len] = {
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS
    };

    double tol_pos[len] = { 0.0, 0.0, 0.0 };
    double eq[len]      = { 0.0, 0.0, 0.0 };

    Recorder recorder{};

    backtest(
        eq,
        timestamp, open, high, low, close,
        tgt_pos, pos_strat, tol_pos,
        StratID::LOW_ATR5_CROSS,
        AssetID::BTC_USDT_SWAP,
        1.0,   // ctr_size
        1.0,   // ctr_step
        10.0,  // lev
        0.0,   // fee_rt
        0.0,   // fund_rt
        len,
        false,
        &recorder
    );

    std::cout << "eq path (test_simple_short_open_and_hold):\n";
    for (std::size_t i = 0; i < len; ++i) {
        std::cout << "  eq[" << i << "] = " << eq[i] << "\n";
    }

    assert_near("test_simple_short_open_and_hold: eq[0]", eq[0], 10000.0);
    assert_near("test_simple_short_open_and_hold: eq[1]", eq[1], 10010.0);
    assert_near("test_simple_short_open_and_hold: eq[2]", eq[2], 10010.0);

    std::cout << "test_simple_short_open_and_hold passed\n";
}

// ----------------------------------------------------------------------
// 4) margin rejection on open
// ----------------------------------------------------------------------
void test_margin_rejection_on_open() {
    constexpr std::size_t len = 2;

    double timestamp[len] = { 0.0, 3600.0 };

    double open[len]  = { 100.0, 100.0 };
    double high[len]  = { 100.0, 100.0 };
    double low[len]   = { 100.0, 100.0 };
    double close[len] = { 100.0, 100.0 };

    double tgt_pos[len] = { 2.0, 2.0 };

    int pos_strat[len] = {
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS
    };

    double tol_pos[len] = { 0.0, 0.0 };
    double eq[len]      = { 0.0, 0.0 };

    Recorder recorder{};

    backtest(
        eq,
        timestamp, open, high, low, close,
        tgt_pos, pos_strat, tol_pos,
        StratID::LOW_ATR5_CROSS,
        AssetID::BTC_USDT_SWAP,
        1.0,   // ctr_size
        1.0,   // ctr_step
        1.0,   // lev
        0.0,   // fee_rt
        0.0,   // fund_rt
        len,
        false,
        &recorder
    );

    std::cout << "eq path (test_margin_rejection_on_open):\n";
    for (std::size_t i = 0; i < len; ++i) {
        std::cout << "  eq[" << i << "] = " << eq[i] << "\n";
    }

    assert_near("test_margin_rejection_on_open: eq[0]", eq[0], 10000.0);
    assert_near("test_margin_rejection_on_open: eq[1]", eq[1], 10000.0);

    std::cout << "test_margin_rejection_on_open passed\n";
}

// ----------------------------------------------------------------------
// 5) liquidation on big crash
// ----------------------------------------------------------------------
void test_liquidation_on_big_crash() {
    constexpr std::size_t len = 2;

    double timestamp[len] = { 0.0, 3600.0 };

    double open[len]  = { 100.0, 100.0 };
    double high[len]  = { 100.0, 100.0 };
    double low[len]   = { 100.0, 0.0   };
    double close[len] = { 100.0, 0.0   };

    double tgt_pos[len] = { 10.0, 10.0 };

    int pos_strat[len] = {
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS
    };

    double tol_pos[len] = { 0.0, 0.0 };
    double eq[len]      = { 0.0, 0.0 };

    Recorder recorder{};

    backtest(
        eq,
        timestamp, open, high, low, close,
        tgt_pos, pos_strat, tol_pos,
        StratID::LOW_ATR5_CROSS,
        AssetID::BTC_USDT_SWAP,
        1.0,   // ctr_size
        1.0,   // ctr_step
        10.0,  // lev
        0.0,   // fee_rt
        0.0,   // fund_rt
        len,
        false,
        &recorder
    );

    std::cout << "eq path (test_liquidation_on_big_crash):\n";
    for (std::size_t i = 0; i < len; ++i) {
        std::cout << "  eq[" << i << "] = " << eq[i] << "\n";
    }

    assert_near("test_liquidation_on_big_crash: eq[0]", eq[0], 10000.0);
    assert_near("test_liquidation_on_big_crash: eq[1]", eq[1], 0.0);

    std::cout << "test_liquidation_on_big_crash passed\n";
}

// ----------------------------------------------------------------------
// 6) fees on open and close
// ----------------------------------------------------------------------
void test_fees_open_and_close() {
    constexpr std::size_t len = 3;

    double timestamp[len] = { 0.0, 3600.0, 7200.0 };

    double open[len]  = { 100.0, 100.0, 100.0 };
    double high[len]  = { 100.0, 100.0, 100.0 };
    double low[len]   = { 100.0, 100.0, 100.0 };
    double close[len] = { 100.0, 100.0, 100.0 };

    double tgt_pos[len] = { 0.01, 0.0, 0.0 };

    int pos_strat[len] = {
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS
    };

    double tol_pos[len] = { 0.0, 0.0, 0.0 };
    double eq[len]      = { 0.0, 0.0, 0.0 };

    Recorder recorder{};

    backtest(
        eq,
        timestamp, open, high, low, close,
        tgt_pos, pos_strat, tol_pos,
        StratID::LOW_ATR5_CROSS,
        AssetID::BTC_USDT_SWAP,
        1.0,    // ctr_size
        1.0,    // ctr_step
        10.0,   // lev
        0.001,  // fee_rt
        0.0,    // fund_rt
        len,
        false,
        &recorder
    );

    std::cout << "eq path (test_fees_open_and_close):\n";
    for (std::size_t i = 0; i < len; ++i) {
        std::cout << "  eq[" << i << "] = " << eq[i] << "\n";
    }

    assert_near("test_fees_open_and_close: eq[0]", eq[0], 10000.0);
    assert_near("test_fees_open_and_close: eq[1]", eq[1], 9999.9, 1e-6);
    assert_near("test_fees_open_and_close: eq[2]", eq[2], 9999.8, 1e-6);

    std::cout << "test_fees_open_and_close passed\n";
}

// ----------------------------------------------------------------------
// 7) partial reduce
// ----------------------------------------------------------------------
void test_partial_reduce_backtest() {
    constexpr std::size_t len = 3;

    double timestamp[len] = { 0.0, 3600.0, 7200.0 };

    double open[len]  = { 100.0, 100.0, 110.0 };
    double high[len]  = { 100.0, 110.0, 110.0 };
    double low[len]   = { 100.0, 100.0, 110.0 };
    double close[len] = { 100.0, 110.0, 110.0 };

    double tgt_pos[len] = { 0.02, 0.02, 0.01 };

    int pos_strat[len] = {
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS
    };

    double tol_pos[len] = { 0.0, 0.0, 0.0 };
    double eq[len]      = { 0.0, 0.0, 0.0 };

    Recorder recorder{};

    backtest(
        eq,
        timestamp, open, high, low, close,
        tgt_pos, pos_strat, tol_pos,
        StratID::LOW_ATR5_CROSS,
        AssetID::BTC_USDT_SWAP,
        1.0,   // ctr_size
        1.0,   // ctr_step
        10.0,  // lev
        0.0,   // fee_rt
        0.0,   // fund_rt
        len,
        false,
        &recorder
    );

    std::cout << "eq path (test_partial_reduce_backtest):\n";
    for (std::size_t i = 0; i < len; ++i) {
        std::cout << "  eq[" << i << "] = " << eq[i] << "\n";
    }

    assert_near("test_partial_reduce_backtest: eq[0]", eq[0], 10000.0);
    assert_near("test_partial_reduce_backtest: eq[1]", eq[1], 10020.0);
    assert_near("test_partial_reduce_backtest: eq[2]", eq[2], 10020.0);

    std::cout << "test_partial_reduce_backtest passed\n";
}

// ----------------------------------------------------------------------
// 8) flip long to short
// ----------------------------------------------------------------------
void test_flip_long_to_short_backtest() {
    constexpr std::size_t len = 3;

    double timestamp[len] = { 0.0, 3600.0, 7200.0 };

    double open[len]  = { 100.0, 100.0, 110.0 };
    double high[len]  = { 100.0, 110.0, 110.0 };
    double low[len]   = { 100.0, 100.0, 90.0  };
    double close[len] = { 100.0, 110.0, 90.0  };

    double tgt_pos[len] = { 0.01, -0.01, -0.01 };

    int pos_strat[len] = {
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS
    };

    double tol_pos[len] = { 0.0, 0.0, 0.0 };
    double eq[len]      = { 0.0, 0.0, 0.0 };

    Recorder recorder{};

    backtest(
        eq,
        timestamp, open, high, low, close,
        tgt_pos, pos_strat, tol_pos,
        StratID::LOW_ATR5_CROSS,
        AssetID::BTC_USDT_SWAP,
        1.0,   // ctr_size
        1.0,   // ctr_step
        10.0,  // lev
        0.0,   // fee_rt
        0.0,   // fund_rt
        len,
        false,
        &recorder
    );

    std::cout << "eq path (test_flip_long_to_short_backtest):\n";
    for (std::size_t i = 0; i < len; ++i) {
        std::cout << "  eq[" << i << "] = " << eq[i] << "\n";
    }

    assert_near("test_flip_long_to_short_backtest: eq[0]", eq[0], 10000.0);
    assert_near("test_flip_long_to_short_backtest: eq[1]", eq[1], 10010.0);
    assert_near("test_flip_long_to_short_backtest: eq[2]", eq[2], 10030.0);

    std::cout << "test_flip_long_to_short_backtest passed\n";
}

// ----------------------------------------------------------------------
// 9) tolerance behavior
// ----------------------------------------------------------------------
void test_tolerance_backtest() {
    constexpr std::size_t len = 3;

    double timestamp[len] = { 0.0, 3600.0, 7200.0 };

    double open[len]  = { 100.0, 100.0, 100.0 };
    double high[len]  = { 100.0, 100.0, 100.0 };
    double low[len]   = { 100.0, 100.0, 100.0 };
    double close[len] = { 100.0, 100.0, 100.0 };

    double tgt_pos[len] = { 0.01, 0.0105, 0.0105 };

    int pos_strat[len] = {
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS
    };

    double tol_pos[len] = { 0.0, 0.001, 0.001 };
    double eq[len]      = { 0.0, 0.0, 0.0 };

    Recorder recorder{};

    backtest(
        eq,
        timestamp, open, high, low, close,
        tgt_pos, pos_strat, tol_pos,
        StratID::LOW_ATR5_CROSS,
        AssetID::BTC_USDT_SWAP,
        1.0,   // ctr_size
        1.0,   // ctr_step
        10.0,  // lev
        0.0,   // fee_rt
        0.0,   // fund_rt
        len,
        false,
        &recorder
    );

    std::cout << "eq path (test_tolerance_backtest):\n";
    for (std::size_t i = 0; i < len; ++i) {
        std::cout << "  eq[" << i << "] = " << eq[i] << "\n";
    }

    assert_near("test_tolerance_backtest: eq[0]", eq[0], 10000.0);
    assert_near("test_tolerance_backtest: eq[1]", eq[1], 10000.0);
    assert_near("test_tolerance_backtest: eq[2]", eq[2], 10000.0);

    std::cout << "test_tolerance_backtest passed\n";
}

// ----------------------------------------------------------------------
// 10) NaN target means “no new intent”
// ----------------------------------------------------------------------
void test_nan_target_backtest() {
    constexpr std::size_t len = 3;

    double timestamp[len] = { 0.0, 3600.0, 7200.0 };

    double open[len]  = { 100.0, 100.0, 110.0 };
    double high[len]  = { 100.0, 110.0, 110.0 };
    double low[len]   = { 100.0, 100.0, 110.0 };
    double close[len] = { 100.0, 110.0, 110.0 };

    double tgt_pos[len] = {
        0.01,
        STRATEGYR::kNaReal,
        STRATEGYR::kNaReal
    };

    int pos_strat[len] = {
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS
    };

    double tol_pos[len] = { 0.0, 0.0, 0.0 };
    double eq[len]      = { 0.0, 0.0, 0.0 };

    Recorder recorder{};

    backtest(
        eq,
        timestamp, open, high, low, close,
        tgt_pos, pos_strat, tol_pos,
        StratID::LOW_ATR5_CROSS,
        AssetID::BTC_USDT_SWAP,
        1.0,
        1.0,
        10.0,
        0.0,
        0.0,
        len,
        false,
        &recorder
    );

    std::cout << "eq path (test_nan_target_backtest):\n";
    for (std::size_t i = 0; i < len; ++i) {
        std::cout << "  eq[" << i << "] = " << eq[i] << "\n";
    }

    assert_near("test_nan_target_backtest: eq[0]", eq[0], 10000.0);
    assert_near("test_nan_target_backtest: eq[1]", eq[1], 10010.0);
    assert_near("test_nan_target_backtest: eq[2]", eq[2], 10010.0);

    std::cout << "test_nan_target_backtest passed\n";
}

// ----------------------------------------------------------------------
// 11) rec flag should not change equity
// ----------------------------------------------------------------------
void test_rec_flag_equity_invariant() {
    constexpr std::size_t len = 3;

    double timestamp[len] = { 0.0, 3600.0, 7200.0 };

    double open[len]  = { 100.0, 100.0, 110.0 };
    double high[len]  = { 100.0, 110.0, 110.0 };
    double low[len]   = { 100.0, 100.0, 110.0 };
    double close[len] = { 100.0, 110.0, 110.0 };

    double tgt_pos[len] = { 0.01, 0.01, 0.01 };
    int pos_strat[len]  = {
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS,
        StratID::LOW_ATR5_CROSS
    };
    double tol_pos[len] = { 0.0, 0.0, 0.0 };

    double eq_no_rec[len]   = {0.0, 0.0, 0.0};
    double eq_with_rec[len] = {0.0, 0.0, 0.0};

    Recorder recorder1{};
    Recorder recorder2{};

    backtest(
        eq_no_rec,
        timestamp, open, high, low, close,
        tgt_pos, pos_strat, tol_pos,
        StratID::LOW_ATR5_CROSS,
        AssetID::BTC_USDT_SWAP,
        1.0, 1.0,
        10.0,
        0.0, 0.0,
        len,
        false,
        &recorder1
    );

    backtest(
        eq_with_rec,
        timestamp, open, high, low, close,
        tgt_pos, pos_strat, tol_pos,
        StratID::LOW_ATR5_CROSS,
        AssetID::BTC_USDT_SWAP,
        1.0, 1.0,
        10.0,
        0.0, 0.0,
        len,
        true,
        &recorder2
    );

    for (std::size_t i = 0; i < len; ++i) {
        assert_near("test_rec_flag_equity_invariant", eq_no_rec[i], eq_with_rec[i]);
    }

    std::cout << "test_rec_flag_equity_invariant passed\n";
}

int main() {
    test_simple_long_open_and_hold();
    test_flatten_to_zero();

    test_simple_short_open_and_hold();
    test_margin_rejection_on_open();
    test_liquidation_on_big_crash();
    test_fees_open_and_close();

    test_partial_reduce_backtest();
    test_flip_long_to_short_backtest();
    test_tolerance_backtest();
    test_nan_target_backtest();

    test_rec_flag_equity_invariant();

    std::cout << "All tests passed.\n";
    return 0;
}
