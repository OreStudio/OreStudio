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
#ifndef ORES_REFDATA_API_DOMAIN_TENOR_UNIT_HPP
#define ORES_REFDATA_API_DOMAIN_TENOR_UNIT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Period unit for a PERIOD tenor's nD/nW/nM/nY duration, or NONE for a SPECIAL tenor with no
 * fixed offset.
 *
 * Reference data table defining the valid [[id:0AC88EB3-DB7F-4135-9DA6-0ED4583FEC29][tenor]]
 * period unit values: DAY, WEEK, MONTH, YEAR for PERIOD tenors, and the
 * sentinel NONE for SPECIAL tenors (which carry no fixed unit/multiplier of
 * their own). NONE exists so tenor.unit can stay a required, FK-validated
 * column rather than nullable free text. Managed by the system tenant, like
 * other tenor code tables.
 */
struct tenor_unit final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique tenor unit code.
     *
     * Examples: 'DAY', 'WEEK', 'MONTH', 'YEAR', 'NONE'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the tenor unit.
     */
    std::string name;

    /**
     * @brief Detailed description of the tenor unit.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this tenor unit.
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
 * @brief Dispatch-key identifier for tenor_unit, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const tenor_unit&) {
    return "ores.refdata.tenor_unit";
}

}

#endif
