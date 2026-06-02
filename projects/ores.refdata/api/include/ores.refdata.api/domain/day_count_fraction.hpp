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
#ifndef ORES_REFDATA_API_DOMAIN_DAY_COUNT_FRACTION_HPP
#define ORES_REFDATA_API_DOMAIN_DAY_COUNT_FRACTION_HPP

#include <chrono>
#include <string>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::domain {

/**
 * @brief A day count fraction code from the FpML DayCountFractionEnum.
 *
 * Day count fractions define how interest accrues between dates. The canonical
 * codes follow FpML 5.x / ISDA naming conventions:
 *
 * - "ACT/360"        — Actual days / 360
 * - "ACT/365.FIXED"  — Actual days / 365 (fixed denominator)
 * - "ACT/ACT.ISDA"   — Actual/Actual (ISDA method)
 * - "ACT/ACT.ISMA"   — Actual/Actual (ISMA/ICMA method)
 * - "ACT/ACT.AFB"    — Actual/Actual (AFB method)
 * - "30/360"         — 30 days per month, 360 per year (US/NASD)
 * - "30E/360"        — 30E/360 (Eurobond basis)
 * - "30E/360.ISDA"   — 30E/360 (ISDA variant)
 * - "BUS/252"        — Business days / 252 (Brazilian market)
 * - "1/1"            — Unit day count (always 1.0)
 *
 * ORE XML allows many aliases (e.g. "A360", "Actual/360", "ACT/360").
 * The mapper normalises all aliases to the canonical FpML code.
 */
struct day_count_fraction final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Canonical FpML code (e.g. "ACT/360", "30E/360", "ACT/ACT.ISDA").
     */
    std::string code;

    /**
     * @brief Human-readable name (e.g. "Actual/360").
     */
    std::string name;

    /**
     * @brief Description of the calculation method.
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
