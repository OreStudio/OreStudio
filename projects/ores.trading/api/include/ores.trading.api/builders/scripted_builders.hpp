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
#ifndef ORES_TRADING_API_BUILDERS_SCRIPTED_BUILDERS_HPP
#define ORES_TRADING_API_BUILDERS_SCRIPTED_BUILDERS_HPP

#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.trading.api/builders/build_error.hpp"
#include "ores.trading.api/builders/option_position.hpp"
#include "ores.trading.api/builders/required.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/domain/script_input.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <expected>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace ores::trading::builders {

using scripted_result = std::expected<domain::scripted_instrument_data, build_error>;

/// An event stated as a schedule: one date a tenor from the start to the end.
struct scripted_schedule final {
    std::chrono::year_month_day start;
    std::chrono::year_month_day end;
    std::string tenor;
};

/**
 * @brief One variable a generic scripted trade states: its name, the type ORE
 * reads it as, and one value, a list of values, or a schedule of dates.
 */
struct scripted_variable final {
    std::string name;
    std::string type;
    std::variant<std::string, std::vector<std::string>, scripted_schedule> value;
};

/**
 * @brief A generic ScriptedTrade of a library script: the script it names, the
 * inputs the library holds for that script, and the variables the trade states.
 */
struct scripted_trade_terms final {
    required<std::string> script_name;
    std::vector<domain::script_input> inputs;
    std::vector<scripted_variable> variables;
};

/**
 * @brief Builds a generic ScriptedTrade, refusing one that does not state an
 * input of its script as the script reads it.
 */
class ORES_TRADING_API_EXPORT scripted_trade_builder final {
public:
    static constexpr std::string_view trade_type = "ScriptedTrade";
    explicit scripted_trade_builder(scripted_trade_terms terms);
    [[nodiscard]] scripted_result build() const;

private:
    scripted_trade_terms terms_;
};

/// The kind of each leg of a double digital option, as ORE reads Type1 and Type2.
enum class digital_kind { call, put, collar };

/// A double digital option, which ORE reads through a class of its own.
struct double_digital_option_terms final {
    required<std::chrono::year_month_day> expiry;
    required<std::chrono::year_month_day> settlement;
    required<utility::decimal::decimal> binary_payout;
    required<utility::decimal::decimal> binary_level_1;
    required<utility::decimal::decimal> binary_level_2;
    required<digital_kind> kind_1;
    required<digital_kind> kind_2;
    required<option_position> position;
    required<std::string> pay_currency;
    required<marketdata::datum::market_reference> underlying_1;
    required<marketdata::datum::market_reference> underlying_2;
};

class ORES_TRADING_API_EXPORT double_digital_option_builder final {
public:
    static constexpr std::string_view trade_type = "DoubleDigitalOption";
    explicit double_digital_option_builder(double_digital_option_terms terms);
    [[nodiscard]] scripted_result build() const;

private:
    double_digital_option_terms terms_;
};

/// A performance option on a basket, which ORE reads through a class of its own.
struct performance_option_terms final {
    required<utility::decimal::decimal> notional;
    required<utility::decimal::decimal> participation_rate;
    required<std::chrono::year_month_day> valuation_date;
    required<std::chrono::year_month_day> settlement_date;
    required<std::vector<utility::decimal::decimal>> strike_prices;
    required<utility::decimal::decimal> strike;
    required<option_position> position;
    required<std::string> pay_currency;
    required<std::vector<marketdata::datum::market_reference>> underlyings;
};

class ORES_TRADING_API_EXPORT performance_option_builder final {
public:
    static constexpr std::string_view trade_type = "PerformanceOption_01";
    explicit performance_option_builder(performance_option_terms terms);
    [[nodiscard]] scripted_result build() const;

private:
    performance_option_terms terms_;
};

/**
 * @brief An autocallable that redeems when the underlying fixes at or above its
 * trigger, which ORE reads through a class of its own.
 */
struct autocallable_terms final {
    required<utility::decimal::decimal> notional;
    required<utility::decimal::decimal> determination_level;
    required<utility::decimal::decimal> trigger_level;
    required<marketdata::datum::market_reference> underlying;
    required<option_position> position;
    required<std::string> pay_currency;
    required<scripted_schedule> fixing_dates;
    required<scripted_schedule> settlement_dates;
    required<std::vector<utility::decimal::decimal>> accumulation_factors;
    required<utility::decimal::decimal> cap;
};

class ORES_TRADING_API_EXPORT autocallable_builder final {
public:
    static constexpr std::string_view trade_type = "Autocallable_01";
    explicit autocallable_builder(autocallable_terms terms);
    [[nodiscard]] scripted_result build() const;

private:
    autocallable_terms terms_;
};

}

#endif
