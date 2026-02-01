// base_ids.h
#pragma once

namespace AssetID { // we leave the ID spaces for future other asset types; maybe equity starts with 1, bond starts with 2; but we don't care about it now
	// crypto perps
  static constexpr int BTC_USDT_SWAP = 8001;
  static constexpr int ETH_USDT_SWAP = 8002;
  static constexpr int SOL_USDT_SWAP = 8003;
  // ...
}

namespace StratID {
  static constexpr int LOW_ATR5_CROSS = 101;
	static constexpr int LOW_ATR10_CROSS = 102;
	static constexpr int LOW_ATR20_CROSS = 103;
	static constexpr int LOW_ATR30_CROSS = 104;
  static constexpr int LADDER3_BREAKOUT = 201;
	static constexpr int LADDER4_BREAKOUT = 202;
	static constexpr int DUAL_PULSE = 301;
  // reserve ranges by category
}
