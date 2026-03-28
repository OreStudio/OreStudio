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
#ifndef ORES_TRADING_DOMAIN_CREDIT_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_CREDIT_INSTRUMENT_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief Credit instrument economics for CreditDefaultSwap, CDSIndex, and
 * SyntheticCDO trades.
 *
 * Discriminated by trade_type_code. Optional fields (index_name, index_series,
 * seniority, restructuring, description) are empty/zero for non-applicable
 * product types.
 */
struct credit_instrument final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this credit instrument.
     */
    boost::uuids::uuid id;

    /**
     * @brief ORE product type code (CreditDefaultSwap, CDSIndex, SyntheticCDO).
     */
    std::string trade_type_code;

    /**
     * @brief Name or identifier of the reference entity.
     */
    std::string reference_entity;

    /**
     * @brief ISO 4217 currency code (e.g. USD).
     */
    std::string currency;

    /**
     * @brief Notional amount of the credit instrument.
     */
    double notional = 0.0;

    /**
     * @brief Credit spread in basis points (e.g. 100.0 for 100 bps).
     */
    double spread = 0.0;

    /**
     * @brief Recovery rate as a decimal (e.g. 0.4 for 40%).
     */
    double recovery_rate = 0.0;

    /**
     * @brief Tenor of the instrument (e.g. "5Y", "3Y").
     */
    std::string tenor;

    /**
     * @brief Start date (ISO 8601 date string, e.g. 2026-01-15).
     */
    std::string start_date;

    /**
     * @brief Maturity date (ISO 8601 date string, e.g. 2031-01-15).
     */
    std::string maturity_date;

    /**
     * @brief Day count convention code (e.g. Actual365Fixed, Thirty360).
     */
    std::string day_count_code;

    /**
     * @brief Payment frequency code (e.g. Quarterly, SemiAnnual).
     */
    std::string payment_frequency_code;

    /**
     * @brief Optional index name for CDSIndex trades (e.g. "CDX.NA.IG").
     * Empty for non-index products.
     */
    std::string index_name;

    /**
     * @brief Optional index series number for CDSIndex trades. Zero means not
     * specified.
     */
    int index_series = 0;

    /**
     * @brief Optional seniority (e.g. "Senior", "Subordinated"). Empty if not
     * applicable.
     */
    std::string seniority;

    /**
     * @brief Optional restructuring clause (e.g. "MM", "MR", "CR", "XR").
     * Empty if not applicable.
     */
    std::string restructuring;

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
