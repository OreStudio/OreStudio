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

#include <chrono>
#include <string>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::domain {

/**
 * @brief Conventions for an overnight index (e.g. EONIA, SONIA, SOFR).
 *
 * Defines the fixing calendar, day count, and settlement lag for a risk-free
 * overnight rate index. Overnight indices differ from IBOR indices in that
 * they are published daily and have no term structure of their own — the OIS
 * convention governs how they are used in swap products.
 *
 * Corresponds to the @c <OvernightIndex> element in @c conventions.xml.
 * Aligns with the FpML @c OvernightIndex concept.
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
     * @brief Unique index identifier (e.g. "EUR-EONIA", "USD-SOFR", "GBP-SONIA").
     */
    std::string id;

    /**
     * @brief Calendar used to determine valid fixing dates (e.g. "TARGET").
     */
    std::string fixing_calendar;

    /**
     * @brief Day count fraction for accrual (canonical FpML, e.g. "ACT/360").
     */
    std::string day_count_fraction;

    /**
     * @brief Number of business days from fixing to value date (usually 0 or 1).
     */
    int settlement_days = 0;

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
