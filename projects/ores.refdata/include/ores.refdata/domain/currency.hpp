/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_REFDATA_DOMAIN_CURRENCY_HPP
#define ORES_REFDATA_DOMAIN_CURRENCY_HPP

#include <chrono>
#include <string>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::domain {

/**
 * @brief Represents a currency with its metadata and formatting rules.
 */
struct currency final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief ISO 4217 alphabetic code (e.g., "USD").
     */
    std::string iso_code;

    /**
     * @brief Full name of the currency (e.g., "United States Dollar").
     */
    std::string name;

    /**
     * @brief ISO 4217 numeric code (e.g., "840").
     */
    std::string numeric_code;
    /**
     * @brief Currency symbol (e.g., "$").
     */
    std::string symbol;
    /**
     * @brief Symbol for fractional unit (e.g., "cent").
     */
    std::string fraction_symbol;

    /**
     * @brief Number of fractional units per whole unit (e.g., 100 for cents).
     */
    int fractions_per_unit;

    /**
     * @brief Rounding method for fractional amounts.
     */
    std::string rounding_type;

    /**
     * @brief Decimal places to round to during formatting.
     */
    int rounding_precision;

    /**
     * @brief Format string for display.
     */
    std::string format;

    /**
     * @brief Asset class classification (e.g., fiat, commodity, synthetic, supranational).
     */
    std::string asset_class;

    /**
     * @brief Market tier classification (e.g., g10, emerging, exotic, frontier, historical).
     */
    std::string market_tier;

    /**
     * @brief Optional reference to a flag image in the images table.
     */
    std::optional<boost::uuids::uuid> image_id;

    /**
     * @brief Username of the person who recorded this version in the system.
     */
    std::string modified_by;

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
     * @brief Username of the account that performed this operation.
     */
    std::string performed_by;

    /**
     * @brief Timestamp when this version of the record was recorded in the system.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
