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
#ifndef ORES_MARKETDATA_API_DOMAIN_MARKET_FIXING_HPP
#define ORES_MARKETDATA_API_DOMAIN_MARKET_FIXING_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>

namespace ores::marketdata::domain {

/**
 * @brief Historical index realisation (fixing) for a given series and date; TimescaleDB hypertable
 * partitioned by fixing_date.
 *
 * A historical index fixing: the official value of a fixing index (identified via
 * series_id) on a given fixing_date. Separate from observations: fixings are
 * historical facts (e.g. EUR-EURIBOR-3M fixed at X on date Y) rather than
 * forward-looking curve points.
 *
 * TimescaleDB hypertable partitioned by fixing_date with 30-day chunks.
 * Corrections use the soft-update/soft-delete pattern (no GIST, no DELETE RULEs).
 * No audit trail columns — volume makes them impractical.
 */
struct market_fixing final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this fixing row.
     */
    boost::uuids::uuid id;

    /**
     * @brief Party that owns this fixing.
     *
     * Set server-side from the authenticated session. Enforced by RLS.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Reference to ores_marketdata_market_series_tbl(id) — identifies the fixing index.
     */
    boost::uuids::uuid series_id;

    /**
     * @brief Calendar date on which the index was fixed. Also the hypertable partition column.
     */
    std::chrono::year_month_day fixing_date;

    /**
     * @brief Serialised fixing value (numeric string).
     */
    std::string value;

    /**
     * @brief Source tag identifying the feed or authority that published this fixing.
     */
    std::string source;

    /**
     * @brief Username of the person who last modified this market fixing.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
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
