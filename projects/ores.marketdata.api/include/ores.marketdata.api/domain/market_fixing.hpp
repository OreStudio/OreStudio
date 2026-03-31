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

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::marketdata::domain {

/**
 * @brief Historical realisation of an index on a specific fixing date.
 *
 * Fixings are the published historical values of floating rate indices
 * (e.g. EURIBOR 3M on 2024-03-20 = 3.894%). They are modelled separately
 * from forward-looking observations but share the same series catalog.
 *
 * Stored in a TimescaleDB hypertable partitioned by fixing_date.
 * Corrections are handled via the same soft-update insert trigger pattern as
 * market_observation.
 */
struct market_fixing final {
    /**
     * @brief Unique identifier for this fixing row.
     */
    boost::uuids::uuid id{};

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Foreign key to the parent market_series (the index).
     */
    boost::uuids::uuid series_id{};

    /**
     * @brief The date on which the index fixing was published.
     */
    std::chrono::year_month_day fixing_date;

    /**
     * @brief The published fixing value as a string (e.g. "0.038940").
     */
    std::string value;

    /**
     * @brief Optional data source identifier (e.g. "BLOOMBERG", "ECB").
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
