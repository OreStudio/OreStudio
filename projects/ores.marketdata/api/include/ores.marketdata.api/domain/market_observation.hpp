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

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::marketdata::domain {

/**
 * @brief A single value on a market series at a specific date and tenor point.
 *
 * Observations are the time-series data points for a market_series. Each row
 * records the value of the series at a given observation_date and (for term
 * structures) a specific point_id (tenor, surface coordinate).
 *
 * Stored in a TimescaleDB hypertable partitioned by observation_date.
 * Corrections are handled by the insert trigger: inserting a new value for the
 * same (series, date, point) closes the previous row and creates a fresh one,
 * preserving full transaction-time history.
 */
struct market_observation final {
    /**
     * @brief Unique identifier for this observation row.
     */
    boost::uuids::uuid id{};

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Foreign key to the parent market_series.
     */
    boost::uuids::uuid series_id{};

    /**
     * @brief Financial valid-time: the date the market was observed.
     */
    std::chrono::year_month_day observation_date;

    /**
     * @brief Tenor or surface coordinate for term structure series.
     *
     * Null for scalar series (e.g. FX spot, equity spot price).
     * For curves: "1Y", "5Y", "6M".
     * For 2-D surfaces: "1Y/ATM", "5Y/2Y/ATM", "6M/25RR".
     */
    std::optional<std::string> point_id;

    /**
     * @brief The observed market value as a string (e.g. "0.034567").
     */
    std::string value;

    /**
     * @brief Optional data source identifier (e.g. "BLOOMBERG", "REUTERS").
     */
    std::optional<std::string> source;

    /**
     * @brief Transaction time: when this row was inserted into the system.
     *
     * Maps to valid_from in the database; valid_to is managed internally.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
