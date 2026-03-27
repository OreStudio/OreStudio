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
#ifndef ORES_TRADING_DOMAIN_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_INSTRUMENT_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief Parent instrument record holding economic terms for a trade.
 *
 * Discriminated by trade_type_code (e.g. Swap, CrossCurrencySwap, CapFloor,
 * Swaption). Asset-class-specific leg details live in swap_leg records linked
 * by instrument_id.
 */
struct instrument final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this instrument.
     */
    boost::uuids::uuid id;

    /**
     * @brief ORE product type code discriminating the asset class.
     *
     * Examples: 'Swap', 'CrossCurrencySwap', 'CapFloor', 'Swaption'.
     * Soft FK to ores_trading_trade_types_tbl.
     */
    std::string trade_type_code;

    /**
     * @brief Principal notional amount.
     */
    double notional = 0.0;

    /**
     * @brief ISO 4217 currency code for the notional (e.g. USD, EUR).
     */
    std::string currency;

    /**
     * @brief Date from which the instrument is effective (ISO 8601).
     */
    std::string start_date;

    /**
     * @brief Maturity/termination date of the instrument (ISO 8601).
     */
    std::string maturity_date;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this record.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
