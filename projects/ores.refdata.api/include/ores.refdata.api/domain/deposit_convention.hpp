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
#ifndef ORES_REFDATA_API_DOMAIN_DEPOSIT_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_DEPOSIT_CONVENTION_HPP

#include <chrono>
#include <string>
#include <optional>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::domain {

/**
 * @brief Conventions for a money-market deposit or IBOR index fixing.
 *
 * Specifies the settlement lag, calendar, and day count for short-term
 * deposits used as the near-end instruments when bootstrapping a yield curve.
 * Corresponds to the @c <Deposit> element in @c conventions.xml.
 *
 * When @c index_based is @c true the remaining fields may be omitted and are
 * instead inherited from the referenced IBOR index convention.
 */
struct deposit_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique convention identifier (e.g. "USD-LIBOR-CONVENTIONS").
     */
    std::string id;

    /**
     * @brief Whether the deposit conventions are derived from an IBOR index.
     *
     * When @c true, @c index must be set and the remaining fields are optional.
     */
    bool index_based = false;

    /**
     * @brief IBOR index identifier (e.g. "USD-LIBOR") used when @c index_based.
     */
    std::optional<std::string> index;

    /**
     * @brief Settlement calendar (e.g. "TARGET", "US-FED").
     */
    std::optional<std::string> calendar;

    /**
     * @brief Business day convention applied to settlement dates (canonical FpML).
     */
    std::optional<std::string> convention;

    /**
     * @brief Whether end-of-month convention applies.
     */
    std::optional<bool> end_of_month;

    /**
     * @brief Day count fraction code (canonical FpML, e.g. "ACT/360").
     */
    std::optional<std::string> day_count_fraction;

    /**
     * @brief Number of business days from trade date to settlement.
     */
    std::optional<int> settlement_days;

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
