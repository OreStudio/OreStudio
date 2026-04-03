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
#ifndef ORES_REFDATA_API_DOMAIN_OIS_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_OIS_CONVENTION_HPP

#include <chrono>
#include <string>
#include <optional>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::domain {

/**
 * @brief Conventions for an overnight index swap (OIS).
 *
 * Defines the fixed-leg and overnight floating-leg parameters for an OIS.
 * OIS are used to bootstrap risk-free overnight discount curves (e.g. EONIA,
 * SOFR, SONIA). Corresponds to the @c <OIS> element in @c conventions.xml.
 */
struct ois_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique convention identifier (e.g. "EUR-OIS-CONVENTIONS").
     */
    std::string id;

    /**
     * @brief Number of business days from trade date to spot date.
     */
    int spot_lag = 0;

    /**
     * @brief Overnight index identifier (e.g. "EUR-EONIA", "USD-SOFR").
     */
    std::string index;

    /**
     * @brief Day count fraction for the fixed leg (canonical FpML).
     */
    std::string fixed_day_count_fraction;

    /**
     * @brief Fixed-leg payment calendar (e.g. "TARGET").
     */
    std::optional<std::string> fixed_calendar;

    /**
     * @brief Number of business days between period end and payment.
     */
    std::optional<int> payment_lag;

    /**
     * @brief Whether end-of-month convention applies.
     */
    std::optional<bool> end_of_month;

    /**
     * @brief Fixed-leg payment frequency (canonical CDM, e.g. "Annual").
     */
    std::optional<std::string> fixed_frequency;

    /**
     * @brief Business day convention for fixed coupon dates (canonical FpML).
     */
    std::optional<std::string> fixed_convention;

    /**
     * @brief Business day convention for fixed payment dates (canonical FpML).
     */
    std::optional<std::string> fixed_payment_convention;

    /**
     * @brief Date generation rule for the schedule (e.g. "Backward", "CDS2015").
     */
    std::optional<std::string> rule;

    /**
     * @brief Payment calendar override (e.g. "TARGET").
     */
    std::optional<std::string> payment_calendar;

    /**
     * @brief Number of fixing days before period end that rate is locked.
     */
    std::optional<int> rate_cutoff;

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
