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
#ifndef ORES_REFDATA_API_DOMAIN_SWAP_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_SWAP_CONVENTION_HPP

#include <chrono>
#include <string>
#include <optional>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::domain {

/**
 * @brief Conventions for a vanilla interest rate swap.
 *
 * Defines the fixed-leg schedule and the floating-leg index for a standard
 * interest rate swap. Used by ORE to bootstrap par swap rates off a yield
 * curve. Corresponds to the @c <Swap> element in @c conventions.xml.
 *
 * The @c index field references an IBOR or overnight index convention by its
 * ORE identifier (e.g. "EUR-EURIBOR-6M").
 */
struct swap_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique convention identifier (e.g. "EUR-6M-SWAP-CONVENTIONS").
     */
    std::string id;

    /**
     * @brief Fixed-leg payment calendar (e.g. "TARGET").
     */
    std::optional<std::string> fixed_calendar;

    /**
     * @brief Fixed-leg payment frequency (canonical CDM, e.g. "Annual").
     */
    std::string fixed_frequency;

    /**
     * @brief Business day convention for the fixed leg (canonical FpML).
     */
    std::optional<std::string> fixed_convention;

    /**
     * @brief Day count fraction for the fixed leg (canonical FpML, e.g. "30/360").
     */
    std::string fixed_day_count_fraction;

    /**
     * @brief Floating-leg index identifier (e.g. "EUR-EURIBOR-6M").
     */
    std::string index;

    /**
     * @brief Floating-leg payment frequency override (canonical CDM).
     * When absent the frequency is derived from the index tenor.
     */
    std::optional<std::string> float_frequency;

    /**
     * @brief Sub-period coupon type when the float leg uses sub-periods.
     * Either "Compounding" or "Averaging".
     */
    std::optional<std::string> sub_periods_coupon_type;

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
