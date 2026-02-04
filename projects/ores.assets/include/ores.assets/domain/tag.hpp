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
#ifndef ORES_ASSETS_DOMAIN_TAG_HPP
#define ORES_ASSETS_DOMAIN_TAG_HPP

#include <string>
#include <optional>

namespace ores::assets::domain {

/**
 * @brief Represents a category tag for images.
 *
 * Tags are used to categorize images (e.g., 'flag', 'currency', 'commodity').
 */
struct tag final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief Unique identifier for the tag (UUID).
     */
    std::string tag_id;

    /**
     * @brief Unique name of the tag (e.g., 'flag', 'currency', 'commodity').
     */
    std::string name;

    /**
     * @brief Human-readable description of the tag.
     */
    std::string description;

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
     * @brief ID of the service account that performed this operation.
     *
     * Null when operation was performed directly by a user.
     * Set when operation was triggered by a service, algorithm, or LLM.
     * Contains the UUID as a string for serialization compatibility.
     */
    std::string performed_by;

    /**
     * @brief Timestamp when this version of the record was recorded in the system.
     */
    std::string recorded_at;
};

}

#endif
