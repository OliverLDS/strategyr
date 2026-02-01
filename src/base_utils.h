// base_utils.h

#include <limits>
#include <cmath>

#pragma once

namespace STRATEGYR {
	enum class ActionCode : int { REDUCE=-2, CLOSE=-1, NONE=0, OPEN=1, INCREASE=2 };
	enum class Dir : int { SHORT = -1, FLAT = 0, LONG = 1 };
	enum class OrderType : int { MARKET = 0, LIMIT = 1 };
	enum class ActionStatus : int { PENDING = 0, FILLED = 1, FAILED = -1 };
	enum class BarStage : int { OPEN = 1, INTRA = 2, CLOSE = 3};
	inline constexpr double kNaReal = std::numeric_limits<double>::quiet_NaN();
	inline constexpr double kInfReal = std::numeric_limits<double>::infinity();
}

inline double tau_to_alpha(const double tau) {return 1.0 - std::exp(-1.0 / tau);}
inline double n_to_alpha(const size_t n) {return 2.0 / (n+1);}
inline double n_to_alpha_wilder(const size_t n) {return 1.0 / n;}

inline double sigmoid(double z) {return 1.0 / (1.0 + std::exp(-z));}
inline double clamp(double x, double lo, double hi) {return (x < lo) ? lo : (x > hi ? hi : x);}

inline bool is_na(double x) { return std::isnan(x); }
