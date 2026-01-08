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
#ifndef ORES_RISK_DOMAIN_COUNTRY_HPP
#define ORES_RISK_DOMAIN_COUNTRY_HPP

#include <chrono>
#include <string>
#include <optional>
#include <boost/uuid/uuid.hpp>

namespace ores::risk::domain {

/**
 * @brief Represents a country using ISO 3166-1 standard codes.
 */
struct country final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief ISO 3166-1 alpha-2 code (e.g., "US", "GB").
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
     * @brief Username of the person who recorded this version in the system.
     */
    std::string recorded_by;

    /**
     * @brief Timestamp when this version of the record was recorded in the system.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
