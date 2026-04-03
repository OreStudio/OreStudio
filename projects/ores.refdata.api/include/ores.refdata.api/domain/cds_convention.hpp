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
#ifndef ORES_REFDATA_API_DOMAIN_CDS_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_CDS_CONVENTION_HPP

#include <chrono>
#include <string>
#include <optional>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::domain {

/**
 * @brief Conventions for a credit default swap (CDS).
 *
 * Defines the settlement lag, premium payment schedule, and accrual rules for
 * a vanilla CDS. Corresponds to the @c <CDS> element in @c conventions.xml.
 *
 * The @c rule field governs how IMM dates and standard CDS roll dates are
 * generated (e.g. "CDS2015" for the 2015 ISDA CDS Standard Model).
 */
struct cds_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique convention identifier (e.g. "CDS-STANDARD-CONVENTIONS").
     */
    std::string id;

    /**
     * @brief Number of business days from trade date to cash settlement.
     */
    int settlement_days = 0;

    /**
     * @brief Calendar used for premium payment date adjustment (e.g. "WeekendsOnly").
     */
    std::string calendar;

    /**
     * @brief Premium payment frequency (canonical CDM, e.g. "Quarterly").
     */
    std::string frequency;

    /**
     * @brief Business day convention for premium payments (canonical FpML).
     */
    std::string payment_convention;

    /**
     * @brief Date generation rule (e.g. "CDS2015", "Backward", "TwentiethIMM").
     */
    std::string rule;

    /**
     * @brief Day count fraction for premium accrual (canonical FpML, e.g. "ACT/360").
     */
    std::string day_count_fraction;

    /**
     * @brief Override for the upfront cash settlement lag (business days).
     */
    std::optional<int> upfront_settlement_days;

    /**
     * @brief Whether accrued interest is settled on default.
     */
    bool settles_accrual = true;

    /**
     * @brief Whether the protection payment is made at the time of default.
     * When @c false payment occurs at the scheduled payment date.
     */
    bool pays_at_default_time = true;

    /**
     * @brief Day count fraction for the last coupon period (canonical FpML).
     * When absent the same fraction as @c day_count_fraction is used.
     */
    std::optional<std::string> last_period_day_count_fraction;

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
