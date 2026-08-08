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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_SWAP_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_SWAP_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Equity Swap instrument.
 *
 * Routes ORE product types: EquitySwap, EquityWorstOfBasketSwap. For
 * basket swaps, underlying_name is NULL and basket_json holds the basket
 * definition; for single-name swaps, basket_json is NULL.
 */
struct equity_swap_instrument final {
    instrument_identity identity;

    /**
     * @brief Single underlying; empty for basket swaps.
     */
    std::string underlying_name;

    /**
     * @brief JSON array of underlyings for EquityWorstOfBasketSwap; empty for single-underlying.
     *
     * Empty generator value: the mapper maps empty strings to SQL NULL (see
     * equity_swap_instrument_mapper.cpp), so the generated instrument keeps basket_json NULL and
     * satisfies the SQL XOR check with trade_type_code = 'EquitySwap'.
     */
    std::string basket_json;

    /**
     * @brief ISO 4217 currency code.
     */
    std::string currency;

    /**
     * @brief Must be positive.
     */
    double notional = 0.0;

    /**
     * @brief TotalReturn or PriceReturn.
     */
    std::string return_type;

    /**
     * @brief ISO 8601 date.
     */
    std::string start_date;

    /**
     * @brief ISO 8601 date.
     */
    std::string maturity_date;

    /**
     * @brief Long or Short.
     */
    std::string long_short;

    /**
     * @brief e.g. 3M, 6M, 1Y.
     */
    std::string payment_frequency;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
};

/**
 * @brief Dispatch-key identifier for equity_swap_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const equity_swap_instrument&) {
    return "ores.trading.equity_swap_instrument";
}

}

#endif
