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
#ifndef ORES_REFDATA_API_DOMAIN_TENOR_RESOLUTION_ALGORITHM_HPP
#define ORES_REFDATA_API_DOMAIN_TENOR_RESOLUTION_ALGORITHM_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief How a tenor convention resolves a tenor's anchor/offset — fixed anchor offset vs
 * IMM-quarter roll.
 *
 * Reference data table defining the valid [[id:C4D8A2E6-3B7F-4A1D-9C5E-8F2A6D3B1E90][tenor
 * convention]] resolution algorithm values: ANCHOR_OFFSET (the common case —
 * a tenor resolves from the convention's anchor plus a fixed or per-row
 * offset) or IMM_ROLL (the convention resolves by rolling forward a count of
 * IMM quarters instead of a calendar offset). Managed by the system tenant,
 * like other tenor code tables.
 */
struct tenor_resolution_algorithm final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique resolution algorithm code.
     *
     * Examples: 'ANCHOR_OFFSET', 'IMM_ROLL'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the resolution algorithm.
     */
    std::string name;

    /**
     * @brief Detailed description of the resolution algorithm.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this tenor resolution algorithm.
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

/**
 * @brief Dispatch-key identifier for tenor_resolution_algorithm, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const tenor_resolution_algorithm&) {
    return "ores.refdata.tenor_resolution_algorithm";
}

}

#endif
