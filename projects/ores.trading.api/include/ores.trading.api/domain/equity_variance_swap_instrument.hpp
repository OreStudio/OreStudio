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
#ifndef ORES_TRADING_DOMAIN_EQUITY_VARIANCE_SWAP_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_EQUITY_VARIANCE_SWAP_INSTRUMENT_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief Equity Variance Swap instrument.
 *
 * Represents EquityVarianceSwap trades.
 */
struct equity_variance_swap_instrument final {
    int version = 0;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this equity variance swap instrument.
     */
    boost::uuids::uuid instrument_id;

    boost::uuids::uuid party_id;
    std::optional<boost::uuids::uuid> trade_id;

    /**
     * @brief ORE product type code: EquityVarianceSwap.
     */
    std::string trade_type_code;

    std::string underlying_name;
    std::string currency;

    /**
     * @brief Vega notional. Must be positive.
     */
    double notional = 0.0;

    /**
     * @brief Strike variance.
     */
    double variance_strike = 0.0;

    /**
     * @brief ISO 8601 date
     */
    std::string start_date;

    /**
     * @brief ISO 8601 date
     */
    std::string maturity_date;

    /**
     * @brief Long or Short
     */
    std::string long_short;

    std::string description;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
