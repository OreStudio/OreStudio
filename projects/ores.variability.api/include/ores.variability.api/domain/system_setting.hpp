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
#ifndef ORES_VARIABILITY_DOMAIN_SYSTEM_SETTING_HPP
#define ORES_VARIABILITY_DOMAIN_SYSTEM_SETTING_HPP

#include <chrono>
#include <string>

namespace ores::variability::domain {

/**
 * @brief Represents a typed system setting in the domain layer.
 *
 * Supports boolean, integer, string, and json value types. System-wide
 * settings use the system tenant; per-tenant
 * settings use the tenant's own tenant_id.
 */
struct system_setting final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     *
     * Use the system tenant UUID for system-wide settings.
     */
    std::string tenant_id;

    /**
     * @brief Name of the setting, serves as the unique identifier.
     */
    std::string name;

    /**
     * @brief Value stored as text regardless of data_type.
     */
    std::string value;

    /**
     * @brief Type of the value: "boolean", "integer", "string", or "json".
     */
    std::string data_type;

    /**
     * @brief Description of what this setting controls.
     */
    std::string description;

    /**
     * @brief Username of the person who recorded this version.
     */
    std::string modified_by;

    /**
     * @brief Code identifying the reason for the change.
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
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
