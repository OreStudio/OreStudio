/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_TRADING_API_BUILDERS_ENTRY_RESET_OPTION_BUILDERS_HPP
#define ORES_TRADING_API_BUILDERS_ENTRY_RESET_OPTION_BUILDERS_HPP

#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.trading.api/builders/equity_builders.hpp"
#include "ores.trading.api/builders/fx_builders.hpp"
#include "ores.trading.api/builders/option_position.hpp"
#include "ores.trading.api/builders/required.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <string>
#include <string_view>
#include <vector>

namespace ores::trading::builders {

/**
 * @brief An option whose entry level is the best the underlying reaches.
 *
 * When the underlying falls below the trigger level on an observation date, the
 * entry is the lowest level observed, floored at the reset minimum. The strike,
 * cap, trigger level and reset minimum are fractions of the level on the strike
 * date.
 */
struct best_entry_option_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<utility::decimal::decimal> notional;
    required<utility::decimal::decimal> multiplier;
    required<utility::decimal::decimal> strike;
    required<utility::decimal::decimal> cap;
    required<utility::decimal::decimal> trigger_level;
    required<utility::decimal::decimal> reset_minimum;
    required<utility::decimal::decimal> premium;
    required<std::chrono::year_month_day> premium_date;
    required<std::chrono::year_month_day> strike_date;
    required<std::vector<std::chrono::year_month_day>> strike_observation_dates;
    required<std::chrono::year_month_day> expiry;
    required<std::chrono::year_month_day> settlement;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT equity_best_entry_option_builder final {
public:
    static constexpr std::string_view trade_type = "EquityBestEntryOption";
    explicit equity_best_entry_option_builder(best_entry_option_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    best_entry_option_terms terms_;
};

class ORES_TRADING_API_EXPORT fx_best_entry_option_builder final {
public:
    static constexpr std::string_view trade_type = "FxBestEntryOption";
    explicit fx_best_entry_option_builder(best_entry_option_terms terms);
    [[nodiscard]] fx_result build() const;

private:
    best_entry_option_terms terms_;
};

/**
 * @brief An option whose strike resets when the underlying reaches a trigger.
 *
 * On the first observation date the underlying is at or past the trigger price
 * in the trigger's direction, the strike becomes the reset strike.
 */
struct strike_resettable_option_terms final {
    required<marketdata::datum::market_reference> underlying;
    required<std::string> currency;
    required<option_kind> kind;
    required<option_position> position;
    required<utility::decimal::decimal> quantity;
    required<utility::decimal::decimal> strike;
    required<utility::decimal::decimal> reset_strike;
    required<trigger_direction> trigger;
    required<utility::decimal::decimal> trigger_price;
    required<std::vector<std::chrono::year_month_day>> observation_dates;
    required<std::chrono::year_month_day> expiry;
    required<std::chrono::year_month_day> settlement;
    required<utility::decimal::decimal> premium;
    required<std::chrono::year_month_day> premium_date;
};

class ORES_TRADING_API_EXPORT equity_strike_resettable_option_builder final {
public:
    static constexpr std::string_view trade_type = "EquityStrikeResettableOption";
    explicit equity_strike_resettable_option_builder(strike_resettable_option_terms terms);
    [[nodiscard]] equity_result build() const;

private:
    strike_resettable_option_terms terms_;
};

class ORES_TRADING_API_EXPORT fx_strike_resettable_option_builder final {
public:
    static constexpr std::string_view trade_type = "FxStrikeResettableOption";
    explicit fx_strike_resettable_option_builder(strike_resettable_option_terms terms);
    [[nodiscard]] fx_result build() const;

private:
    strike_resettable_option_terms terms_;
};

}

#endif
