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
#ifndef ORES_REFDATA_API_DOMAIN_OVERNIGHT_INDEX_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_OVERNIGHT_INDEX_CONVENTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Conventions for an overnight index (e.g. EONIA, SONIA, SOFR).
 *
 * Defines the fixing calendar, day count, and settlement lag for a risk-free
 * overnight rate index. Corresponds to the <OvernightIndex> element in
 * ORE conventions.xml.
 */
struct overnight_index_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Workspace this record belongs to.
     *
     * Defaults to the Live workspace sentinel.
     */
    boost::uuids::uuid workspace_id = utility::uuid::live_workspace_id();

    /**
     * @brief Unique index identifier.
     *
     * Examples: 'EUR-EONIA', 'USD-SOFR', 'GBP-SONIA'.
     */
    std::string id;

    /**
     * @brief Calendar used to determine valid fixing dates (e.g. 'TARGET').
     */
    std::string fixing_calendar;

    /**
     * @brief Day count fraction for accrual (canonical FpML, e.g. 'ACT/360').
     */
    std::string day_count_fraction;

    /**
     * @brief Number of business days from fixing to value date (usually 0 or 1).
     */
    int settlement_days = 0;

    /**
     * @brief Username of the person who last modified this overnight index convention.
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
 * @brief Dispatch-key identifier for overnight_index_convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const overnight_index_convention&) {
    return "ores.refdata.overnight_index_convention";
}

}

#endif
