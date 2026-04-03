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
#ifndef ORES_REFDATA_API_DOMAIN_BUSINESS_DAY_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_BUSINESS_DAY_CONVENTION_HPP

#include <chrono>
#include <string>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::domain {

/**
 * @brief A business day convention code from the FpML BusinessDayConventionEnum.
 *
 * Business day conventions describe how a date that falls on a non-business
 * day should be adjusted. The canonical codes follow FpML 5.x naming:
 *
 * - "Following"                  — next business day
 * - "ModifiedFollowing"          — next business day unless it crosses a month
 *                                  boundary, in which case the preceding one
 * - "Preceding"                  — preceding business day
 * - "ModifiedPreceding"          — preceding unless it crosses a month boundary
 * - "Unadjusted"                 — no adjustment
 * - "HalfMonthModifiedFollowing" — as modified following but also adjusted at
 *                                  the 15th of the month
 * - "Nearest"                    — closest business day (forward preferred)
 *
 * ORE XML allows many aliases for the same convention (e.g. "F", "Following",
 * "FOLLOWING"). The mapper normalises all aliases to the canonical FpML code.
 */
struct business_day_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Canonical FpML code (e.g. "ModifiedFollowing", "Unadjusted").
     */
    std::string code;

    /**
     * @brief Human-readable name (e.g. "Modified Following").
     */
    std::string name;

    /**
     * @brief Description of the adjustment rule.
     */
    std::string description;

    /**
     * @brief Suggested display order in UI lists.
     */
    int display_order = 0;

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
