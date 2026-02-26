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
#ifndef ORES_REFDATA_DOMAIN_BUSINESS_UNIT_TYPE_HPP
#define ORES_REFDATA_DOMAIN_BUSINESS_UNIT_TYPE_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::refdata::domain {

/**
 * @brief Classification of organisational unit roles within a hierarchy.
 *
 * Defines the type and level of a business unit (e.g. Division, Desk).
 * The `level` field enforces hierarchy ordering: 0 = top-level (e.g.
 * Division or Branch), higher values = lower in the hierarchy.
 * Business units referencing a type must satisfy: child.level > parent.level.
 */
struct business_unit_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief UUID uniquely identifying this business unit type.
     *
     * Surrogate key for the record.
     */
    boost::uuids::uuid id;

    /**
     * @brief Coding scheme this type belongs to.
     *
     * Soft FK to ores_dq_coding_schemes_tbl (e.g. "ORES-ORG").
     */
    std::string coding_scheme_code;

    /**
     * @brief Short code identifying this type within the coding scheme.
     *
     * e.g. "DIVISION", "DESK".
     */
    std::string code;

    /**
     * @brief Human-readable name for this type.
     *
     * e.g. "Division", "Trading Desk".
     */
    std::string name;

    /**
     * @brief Hierarchy level: 0 = top, higher = lower in hierarchy.
     *
     * Used to enforce parent-child level constraints on business units.
     */
    int level = 0;

    /**
     * @brief Optional description of this business unit type.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this record.
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
