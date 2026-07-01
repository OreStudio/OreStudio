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
#ifndef ORES_MARKETDATA_API_DOMAIN_MARKET_OBSERVATION_HPP
#define ORES_MARKETDATA_API_DOMAIN_MARKET_OBSERVATION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>

namespace ores::marketdata::domain {

/**
 * @brief One observed value for a (series, observation_datetime, point_id) triple; TimescaleDB
 * hypertable partitioned by observation_datetime.
 *
 * A single market data observation: the value of a series at a given
 * observation_datetime and optional point_id (tenor/surface coordinate).
 * observation_datetime is the financial valid-time (UTC); valid_from/valid_to
 * is the transaction time. Corrections replace the previous value via the
 * soft-update trigger.
 *
 * TimescaleDB hypertable partitioned by observation_datetime with 30-day chunks;
 * GIST exclusion and DELETE RULEs are incompatible with hypertables — uniqueness
 * is enforced via partial unique index and the soft-update/soft-delete trigger pair.
 *
 * No audit trail columns (version, modified_by, performed_by, change_reason_code,
 * change_commentary) — tick-level data volumes make these impractical.
 */
struct market_observation final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this observation row.
     */
    boost::uuids::uuid id;

    /**
     * @brief Party that owns this observation.
     *
     * Set server-side from the authenticated session. Enforced by RLS.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Reference to ores_marketdata_market_series_tbl(id) — identifies what was observed.
     */
    boost::uuids::uuid series_id;

    /**
     * @brief Financial valid-time: when the market value was observed (UTC). Also the hypertable
     * partition column.
     */
    std::chrono::system_clock::time_point observation_datetime;

    /**
     * @brief Tenor or compound surface identifier (e.g. 1Y, 5Y/2Y/ATM, 0.03/10Y/2Y). Null for
     * scalar series such as FX spot rates.
     */
    std::string point_id;

    /**
     * @brief Serialised market value (numeric string; format is series-type-specific).
     */
    std::string value;

    /**
     * @brief Source tag identifying the producer channel that published this observation (e.g.
     * synthetic.v1.tick.EUR-USD).
     */
    std::string source;

    /**
     * @brief Username of the person who last modified this market observation.
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
