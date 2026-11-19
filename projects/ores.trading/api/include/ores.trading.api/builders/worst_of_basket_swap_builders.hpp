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
#ifndef ORES_TRADING_API_BUILDERS_WORST_OF_BASKET_SWAP_BUILDERS_HPP
#define ORES_TRADING_API_BUILDERS_WORST_OF_BASKET_SWAP_BUILDERS_HPP

#include "ores.marketdata.api/datum/market_reference.hpp"
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

/// One member of an FX worst-of basket: its pair's index and the rate it starts at.
struct worst_of_member final {
    marketdata::datum::market_reference underlying;
    utility::decimal::decimal initial_price;
};

/**
 * @brief A swap paying a floating leg against fixed coupons that depend on the
 * worst performing pair of a basket.
 *
 * The period dates bound each floating period; each period pays on its pay
 * date and states its own knock-out and fixed trigger level, so the pay dates
 * and both level lists hold one entry fewer than the period dates. The floating
 * leg fixes two business days before each period starts.
 */
struct fx_worst_of_basket_swap_terms final {
    required<std::vector<worst_of_member>> members;
    required<std::string> currency;
    required<utility::decimal::decimal> quantity;
    required<utility::decimal::decimal> initial_fixed_rate;
    required<std::vector<std::chrono::year_month_day>> period_dates;
    required<std::vector<std::chrono::year_month_day>> pay_dates;
    /// The floating index as ORE names it, such as USD-SOFR.
    required<std::string> floating_index;
    required<utility::decimal::decimal> floating_spread;
    /// The floating leg's day count as ORE spells it, such as A360.
    required<std::string> floating_day_count;
    required<std::vector<utility::decimal::decimal>> knock_out_levels;
    required<std::vector<utility::decimal::decimal>> fixed_trigger_levels;
    required<utility::decimal::decimal> fixed_rate;
    required<utility::decimal::decimal> knock_in_level;
    required<utility::decimal::decimal> strike;
    required<option_position> position;
};

class ORES_TRADING_API_EXPORT fx_worst_of_basket_swap_builder final {
public:
    static constexpr std::string_view trade_type = "FxWorstOfBasketSwap";
    explicit fx_worst_of_basket_swap_builder(fx_worst_of_basket_swap_terms terms);
    [[nodiscard]] fx_result build() const;

private:
    fx_worst_of_basket_swap_terms terms_;
};

}

#endif
