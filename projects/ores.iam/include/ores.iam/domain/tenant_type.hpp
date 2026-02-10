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
#ifndef ORES_IAM_DOMAIN_TENANT_TYPE_HPP
#define ORES_IAM_DOMAIN_TENANT_TYPE_HPP

#include <chrono>
#include <optional>
#include <string>

namespace ores::iam::domain {

/**
 * @brief Classification of tenant types.
 *
 * Reference data table defining valid tenant type classifications.
 * Examples: 'system', 'production', 'evaluation', 'automation'.
 *
 * Tenant types are managed by the system tenant and are used to
 * categorise tenants for different purposes.
 */
struct tenant_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Unique type code.
     *
     * Examples: 'system', 'production', 'evaluation'.
     */
    std::string type;

    /**
     * @brief Human-readable name for the type.
     */
    std::string name;

    /**
     * @brief Detailed description of the tenant type.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order;

    /**
     * @brief Username of the person who last modified this tenant type.
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
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
