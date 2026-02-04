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
#ifndef ORES_ASSETS_DOMAIN_IMAGE_HPP
#define ORES_ASSETS_DOMAIN_IMAGE_HPP

#include <chrono>
#include <string>
#include <optional>
#include <boost/uuid/uuid.hpp>

namespace ores::assets::domain {

/**
 * @brief Represents a dynamically loaded image (typically SVG).
 */
struct image final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    boost::uuids::uuid tenant_id;

    /**
     * @brief Unique identifier for the image (UUID).
     */
    boost::uuids::uuid image_id;

    /**
     * @brief Unique key used by the application (e.g., 'ro', 'us', 'gb').
     */
    std::string key;

    /**
     * @brief Human-readable description of the image.
     *
     * Example: "Flag for country code RO (Romania)"
     */
    std::string description;

    /**
     * @brief The entire SVG content stored as plain text.
     */
    std::string svg_data;

    /**
     * @brief Username of the person who recorded this version in the system.
     */
    std::string recorded_by;

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
