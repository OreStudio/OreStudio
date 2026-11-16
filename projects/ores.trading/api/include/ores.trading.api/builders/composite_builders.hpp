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
#ifndef ORES_TRADING_API_BUILDERS_COMPOSITE_BUILDERS_HPP
#define ORES_TRADING_API_BUILDERS_COMPOSITE_BUILDERS_HPP

#include "ores.marketdata.api/conventions/convention_lookup.hpp"
#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.trading.api/builders/build_error.hpp"
#include "ores.trading.api/builders/required.hpp"
#include "ores.trading.api/builders/vanilla_swap_builder.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <expected>
#include <string>
#include <string_view>

namespace ores::trading::builders {

/**
 * The containers state only their own terms. The trades they hold are trades
 * of their own, each built by its own builder and joined to the container by a
 * structure of the container's template.
 */
using composite_result = std::expected<domain::composite_instrument_data, build_error>;

/// The terms of a trade that is the sum of the trades it holds.
struct composite_trade_terms final {
    required<std::string> currency;
    /// How ORE derives the composite's notional from its components'.
    required<std::string> notional_calculation;
};

class ORES_TRADING_API_EXPORT composite_trade_builder final {
public:
    static constexpr std::string_view trade_type = "CompositeTrade";

    explicit composite_trade_builder(composite_trade_terms terms);

    [[nodiscard]] composite_result build() const;

private:
    composite_trade_terms terms_;
};

/**
 * @brief The return leg of a total return swap or a contract for difference.
 *
 * One side pays the return of the underlying trade from the start to the end,
 * measured against the initial price.
 */
struct total_return_terms final {
    required<side> return_side;
    required<std::string> currency;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> end;
    required<utility::decimal::decimal> initial_price;
};

/// The return on the underlying for a fee, with no funding leg.
class ORES_TRADING_API_EXPORT contract_for_difference_builder final {
public:
    static constexpr std::string_view trade_type = "ContractForDifference";

    explicit contract_for_difference_builder(total_return_terms terms);

    [[nodiscard]] composite_result build() const;

private:
    total_return_terms terms_;
};

/**
 * @brief The terms of a total return swap.
 *
 * The funding leg is the floating leg of a vanilla swap on the given terms, so
 * the index's conventions state it; the side that pays the return receives it.
 */
struct total_return_swap_terms final {
    total_return_terms return_leg;
    required<utility::decimal::decimal> funding_notional;
    required<marketdata::datum::market_reference> funding_index;
};

class ORES_TRADING_API_EXPORT total_return_swap_builder final {
public:
    static constexpr std::string_view trade_type = "TotalReturnSwap";

    total_return_swap_builder(const marketdata::conventions::convention_lookup& conventions,
                              total_return_swap_terms terms);
    total_return_swap_builder(marketdata::conventions::convention_lookup&&,
                              total_return_swap_terms) = delete;

    [[nodiscard]] composite_result build() const;

private:
    const marketdata::conventions::convention_lookup& conventions_;
    total_return_swap_terms terms_;
};

}

#endif
