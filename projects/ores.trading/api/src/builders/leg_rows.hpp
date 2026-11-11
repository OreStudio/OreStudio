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
#ifndef ORES_TRADING_API_SRC_BUILDERS_LEG_ROWS_HPP
#define ORES_TRADING_API_SRC_BUILDERS_LEG_ROWS_HPP

#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/swap_leg_amount.hpp"
#include "ores.trading.api/domain/swap_leg_rate.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <utility>

/**
 * @brief The rows every builder states a leg with.
 *
 * Each sets the columns every leg shares; a builder then sets only what its
 * product adds, such as the conventions of a swap leg or the group of a credit
 * leg. The generated row types carry no constructors, so these are free
 * functions.
 */
namespace ores::trading::builders::rows {

[[nodiscard]] inline domain::swap_leg
swap_leg(int leg_number, bool payer, std::string type, std::string currency) {
    domain::swap_leg row;
    row.identity.leg_number = leg_number;
    row.payer = payer;
    row.leg_type_code = std::move(type);
    row.currency = std::move(currency);
    return row;
}

[[nodiscard]] inline domain::swap_leg_amount
leg_amount(int leg_number,
           utility::decimal::decimal amount,
           std::optional<std::chrono::year_month_day> start = {}) {
    domain::swap_leg_amount row;
    row.leg_number = leg_number;
    row.sequence_number = 1;
    row.amount = amount;
    row.start_date = start;
    return row;
}

[[nodiscard]] inline domain::swap_leg_rate
leg_rate(int leg_number, std::string role, utility::decimal::decimal value) {
    domain::swap_leg_rate row;
    row.leg_number = leg_number;
    row.rate_role = std::move(role);
    row.sequence_number = 1;
    row.value = value;
    return row;
}

}

#endif
