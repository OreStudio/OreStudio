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
#ifndef ORES_TRADING_API_BUILDERS_PAIRWISE_VARIANCE_SWAP_TERMS_HPP
#define ORES_TRADING_API_BUILDERS_PAIRWISE_VARIANCE_SWAP_TERMS_HPP

#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.trading.api/builders/build_error.hpp"
#include "ores.trading.api/builders/option_position.hpp"
#include "ores.trading.api/builders/required.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <expected>
#include <string>
#include <string_view>

namespace ores::trading::builders {

/// One member of a pairwise variance swap: the underlying, its strike and its notional.
struct pairwise_member final {
    marketdata::datum::market_reference underlying;
    utility::decimal::decimal strike;
    utility::decimal::decimal notional;
};

/**
 * @brief A swap on the variance of two underlyings and of their sum, observed
 * daily from the start to the end.
 */
struct pairwise_variance_swap_terms final {
    required<pairwise_member> first;
    required<pairwise_member> second;
    required<utility::decimal::decimal> basket_notional;
    required<utility::decimal::decimal> basket_strike;
    required<std::string> pay_currency;
    required<std::chrono::year_month_day> start;
    required<std::chrono::year_month_day> end;
    required<std::chrono::year_month_day> settlement;
    required<option_position> position;
};

/// The row both families' pairwise tables share, or the term that refuses it.
template <typename Row>
std::expected<Row, build_error>
pairwise_variance_swap_row(std::string_view trade_type, const pairwise_variance_swap_terms& terms) {
    using utility::decimal::decimal;
    if (terms.pay_currency.get().empty())
        return std::unexpected(build_error{"pay_currency", "must name a currency"});
    const auto& first = terms.first.get();
    const auto& second = terms.second.get();
    if (first.underlying.uri() == second.underlying.uri())
        return std::unexpected(build_error{"second", "must name another underlying"});
    for (const auto& [term, value] : {std::pair{"strike", &first.strike},
                                      std::pair{"strike", &second.strike},
                                      std::pair{"notional", &first.notional},
                                      std::pair{"notional", &second.notional},
                                      std::pair{"basket_notional", &terms.basket_notional.get()},
                                      std::pair{"basket_strike", &terms.basket_strike.get()}})
        if (!(*value > decimal{}))
            return std::unexpected(build_error{term, "must be positive"});
    if (!(terms.start.get() < terms.end.get()))
        return std::unexpected(build_error{"end", "must be after the start"});
    if (terms.settlement.get() < terms.end.get())
        return std::unexpected(build_error{"settlement", "must not be before the end"});
    Row row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.first_underlying = first.underlying.uri();
    row.second_underlying = second.underlying.uri();
    row.first_strike = first.strike;
    row.second_strike = second.strike;
    row.first_notional = first.notional;
    row.second_notional = second.notional;
    row.basket_notional = terms.basket_notional.get();
    row.basket_strike = terms.basket_strike.get();
    row.currency = terms.pay_currency.get();
    row.settlement_date = terms.settlement.get();
    row.long_short = long_short_of(terms.position.get());
    return row;
}

}

#endif
