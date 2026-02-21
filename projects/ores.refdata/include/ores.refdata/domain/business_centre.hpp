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
#ifndef ORES_REFDATA_DOMAIN_BUSINESS_CENTRE_HPP
#define ORES_REFDATA_DOMAIN_BUSINESS_CENTRE_HPP

#include <chrono>
#include <string>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::domain {

/**
 * @brief Represents a business centre using FpML-style codes.
 *
 * Business centres identify financial trading locations used for holiday
 * calendar determination (e.g., "USNY" for New York, "GBLO" for London).
 */
struct business_centre final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief FpML-style business centre code (e.g., "USNY", "GBLO").
     */
    std::string code;

    /**
     * @brief Data source identifier (e.g., "FpML", "ISDA").
     */
    std::string source;

    /**
     * @brief Human-readable description of the business centre.
     */
    std::string description;

    /**
     * @brief City name derived from description.
     */
    std::string city_name;

    /**
     * @brief Code of the coding scheme this business centre belongs to.
     */
    std::string coding_scheme_code;

    /**
     * @brief ISO 3166-1 alpha-2 code of the country (e.g., "US", "GB").
     */
    std::string country_alpha2_code;

    /**
     * @brief Optional reference to an image in the images table.
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
