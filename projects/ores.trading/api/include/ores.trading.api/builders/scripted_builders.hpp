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
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

namespace ores::trading::builders {

using scripted_result = std::expected<domain::scripted_instrument_data, build_error>;

/**
 * @brief An event stated as a schedule: one date a tenor from the start to the
 * end. A rule term left empty is not written, so ORE applies its own default.
 */
struct scripted_schedule final {
    std::chrono::year_month_day start;
    std::chrono::year_month_day end;
    std::string tenor;
    std::optional<std::string> calendar = "TARGET";
    std::optional<std::string> convention = "F";
    std::optional<std::string> term_convention;
    std::optional<std::string> rule = "Forward";
};

/// An event stated as a list of dates, which ORE reads as a schedule of those dates.
struct scripted_dates final {
    std::vector<std::chrono::year_month_day> dates;
    std::optional<std::string> calendar;
    std::optional<std::string> convention;
};

/// An event derived from another event of the trade, each of its dates moved by the shift.
struct scripted_derived final {
    std::string base;
    std::string shift;
    std::optional<std::string> calendar;
    std::optional<std::string> convention;
};

/**
 * @brief One variable a generic scripted trade states: its name, the type ORE
 * reads it as, and one value, a list of values, a schedule of dates by rules,
 * a schedule of listed dates, or a schedule derived from another.
 */
struct scripted_variable final {
    std::string name;
    std::string type;
    std::variant<std::string,
                 std::vector<std::string>,
                 scripted_schedule,
                 scripted_dates,
                 scripted_derived>
        value;
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

/// The families a single-underlying scripted product is read over, each through a class of its own.
enum class scripted_family { equity, fx };

/// One barrier: its type as ORE spells it, such as DownAndOut, and its level.
struct scripted_barrier final {
    std::string type;
    utility::decimal::decimal level;
};


}

#endif
