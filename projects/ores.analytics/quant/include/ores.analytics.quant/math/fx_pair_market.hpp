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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_ANALYTICS_QUANT_MATH_FX_PAIR_MARKET_HPP
#define ORES_ANALYTICS_QUANT_MATH_FX_PAIR_MARKET_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/fx_delta.hpp"
#include <chrono>
#include <expected>
#include <span>
#include <string>

namespace ores::analytics::quant::math {

/**
 * @brief One dated row of a pair's forward set: the two discount factors to that day.
 *
 * The base currency's discount is the foreign side and the quote currency's is the
 * domestic side, so a row carries no currency names and needs no lookup to be read as a
 * market. That mapping is the one every reader of a forward set shares, which is why it
 * lives here and not in each caller.
 */
struct fx_discount_point final {
    std::chrono::year_month_day date{};
    double base_discount{1.0};
    double quote_discount{1.0};
};

/**
 * @brief A pair's market at one value date, from the discounts its forward set holds.
 *
 * A day the set quotes takes that row's two discounts. A day between two rows takes both
 * discounts interpolated log-linearly in days from the spot date, because a discount
 * factor is an exponential of a continuously compounded rate: the exponent is what runs
 * between two points, not the factor.
 *
 * The spot date is where discounting starts, at exactly one on both sides, so a value
 * date on it is the spot itself with no differential.
 *
 * @param spot The pair's spot, in the direction the set was published.
 * @param spot_date The pair's spot date.
 * @param points The set's rows, ascending by date and not earlier than @p spot_date.
 * @param value_date The day the market is read at, normally an option's expiry.
 * @param context The caller, prefixed to the error message.
 */
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT std::expected<fx_market, std::string>
pair_market_at(double spot,
               std::chrono::year_month_day spot_date,
               std::span<const fx_discount_point> points,
               std::chrono::year_month_day value_date,
               std::string context);

}

#endif
