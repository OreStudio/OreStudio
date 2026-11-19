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
#ifndef ORES_TRADING_API_BUILDERS_BASKET_OPTION_TERMS_HPP
#define ORES_TRADING_API_BUILDERS_BASKET_OPTION_TERMS_HPP

#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.trading.api/builders/option_position.hpp"
#include "ores.trading.api/builders/required.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::builders {

/// One member of a basket: what it is, its quote currency where the document states one, and its
/// weight.
struct basket_option_member final {
    marketdata::datum::market_reference underlying;
    std::optional<std::string> currency;
    utility::decimal::decimal weight;
};

/**
 * @brief A European option on a weighted basket.
 *
 * The payoff type names the payoff ORE prices: Vanilla for a basket, and
 * BestOfAssetOrCash, WorstOfAssetOrCash, MaxRainbow or MinRainbow for a
 * rainbow. The option type is stated only for the payoffs that read it.
 */
struct basket_option_terms final {
    required<std::string> currency;
    required<utility::decimal::decimal> notional;
    required<utility::decimal::decimal> strike;
    required<std::vector<basket_option_member>> members;
    required<std::string> payoff_type;
    required<option_kind> kind;
    required<option_position> position;
    required<std::chrono::year_month_day> expiry;
    required<std::chrono::year_month_day> settlement;
};

}

#endif
