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
#ifndef ORES_REFDATA_API_DOMAIN_IBOR_INDEX_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_IBOR_INDEX_CONVENTION_HPP

#include <chrono>
#include <string>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::domain {

/**
 * @brief Conventions for an interbank offered rate (IBOR) index.
 *
 * Defines the fixing calendar, day count, settlement lag, and business day
 * convention for a term IBOR index such as EURIBOR or USD LIBOR.
 * Corresponds to the @c <IborIndex> element in @c conventions.xml.
 *
 * The @c id typically takes the form "@c CCY-NAME" (e.g. "EUR-EURIBOR",
 * "USD-LIBOR"). The per-tenor variant (e.g. "EUR-EURIBOR-6M") is resolved
 * by ORE from the base index id and the instrument tenor.
 *
 * Aligns with the FpML @c FloatingRateIndex concept.
 */
struct ibor_index_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique index identifier (e.g. "EUR-EURIBOR", "USD-LIBOR").
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
     * @brief Number of business days from fixing to settlement.
     */
    int settlement_days = 0;

    /**
     * @brief Business day convention for maturity dates (canonical FpML).
     */
    std::string business_day_convention;

    /**
     * @brief Whether end-of-month convention applies.
     */
    bool end_of_month = false;

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
