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
#ifndef ORES_REFDATA_API_DOMAIN_COUNTRY_HPP
#define ORES_REFDATA_API_DOMAIN_COUNTRY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>

namespace ores::refdata::domain {

/**
 * @brief ISO 3166-1 country definitions.
 *
 * ISO 3166-1 country definitions used for reference data.
 * Countries use alpha-2, alpha-3, and numeric codes per the ISO standard.
 */
struct country final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief ISO 3166-1 alpha-2 code (e.g., "US", "GB").
     *
     * No :name generator block is defined for this primary key. The synthetic
     * generate_synthetic_country function will produce an empty prefix; this is intentional — the
     * fictional generator (generate_fictional_countries, injected via paste) is the authoritative
     * test-data source for country and does not rely on the template-generated synthetic path.
     */
    std::string alpha2_code;

    /**
     * @brief ISO 3166-1 alpha-3 code (e.g., "USA", "GBR").
     */
    std::string alpha3_code;

    /**
     * @brief ISO 3166-1 numeric code (e.g., "840", "826").
     */
    std::string numeric_code;

    /**
     * @brief Short name of the country (e.g., "United States").
     */
    std::string name;

    /**
     * @brief Official name of the country (e.g., "United States of America").
     */
    std::string official_name;

    /**
     * @brief Optional reference to a flag image in the images table.
     */
    std::optional<boost::uuids::uuid> image_id;

    /**
     * @brief Optional coding scheme code for the country.
     */
    std::optional<std::string> coding_scheme_code;

    /**
     * @brief Username of the person who last modified this country.
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
