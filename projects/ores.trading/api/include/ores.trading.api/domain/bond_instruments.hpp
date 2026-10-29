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
#ifndef ORES_TRADING_API_DOMAIN_BOND_INSTRUMENTS_HPP
#define ORES_TRADING_API_DOMAIN_BOND_INSTRUMENTS_HPP

#include "ores.trading.api/domain/bond_instrument_data.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <stdexcept>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Refuses to construct a product from facts that do not state one.
 */
class bond_product_error final : public std::invalid_argument {
public:
    using std::invalid_argument::invalid_argument;
};

/**
 * @brief The facts a bond trade states about its security.
 */
struct bond_terms final {
    boost::uuids::uuid issue_id;
    ores::utility::decimal::decimal notional;
};

/**
 * @brief Constructs the one bond product the population draws.
 *
 * The security and its terms live in the issue, which the run writes before it
 * books the trade, so the instrument states the issue it holds and the amount
 * of that issue the trade is for.
 */
[[nodiscard]] inline bond_instrument_data construct_bond(const bond_terms& terms) {
    if (terms.issue_id == boost::uuids::nil_uuid())
        throw bond_product_error("A bond states the security issue it holds.");
    if (!(terms.notional > ores::utility::decimal::decimal{}))
        throw bond_product_error("A bond states a positive notional.");

    bond_instrument_data data;
    data.instrument.identity.trade_type_code = "Bond";
    data.instrument.issue_id = terms.issue_id;
    data.instrument.notional = terms.notional;
    return data;
}

}

#endif
