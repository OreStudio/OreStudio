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

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Conventions for a credit default swap (CDS).
 *
 * Defines the settlement lag, premium payment schedule, and accrual rules
 * for a vanilla CDS. Corresponds to the <CDS> element in ORE conventions.xml.
 * The rule field governs IMM dates and standard CDS roll dates.
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
     * @brief Workspace this record belongs to.
     *
     * Defaults to the Live workspace sentinel.
     */
    boost::uuids::uuid workspace_id = utility::uuid::live_workspace_id();

    /**
     * @brief Unique convention identifier.
     *
     * Examples: 'CDS-STANDARD-CONVENTIONS', 'EUR-CDS-CONVENTIONS'.
     */
    std::string id;

    /**
     * @brief Number of business days from trade date to cash settlement.
     */
    int settlement_days = 0;

    /**
     * @brief Calendar used for premium payment date adjustment (e.g. 'WeekendsOnly').
     */
    std::string calendar;

    /**
     * @brief Premium payment frequency (canonical CDM, e.g. 'Quarterly').
     */
    std::string frequency;

    /**
     * @brief Business day convention for premium payments (canonical FpML).
     */
    std::string payment_convention;

    /**
     * @brief Date generation rule (e.g. 'CDS2015', 'Backward', 'TwentiethIMM').
     */
    std::string rule;

    /**
     * @brief Day count fraction for premium accrual (canonical FpML, e.g. 'ACT/360').
     */
    std::string day_count_fraction;

    /**
     * @brief Whether accrued interest is settled on default.
     */
    bool settles_accrual = false;

    /**
     * @brief Whether the protection payment is made at the time of default.
     */
    bool pays_at_default_time = false;

    /**
     * @brief Override for the upfront cash settlement lag (business days).
     */
    std::optional<int> upfront_settlement_days;

    /**
     * @brief Day count fraction for the last coupon period (canonical FpML). When absent the same
     * fraction as day_count_fraction is used.
     */
    std::optional<std::string> last_period_day_count_fraction;

    /**
     * @brief Username of the person who last modified this CDS convention.
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
 * @brief Dispatch-key identifier for cds_convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const cds_convention&) {
    return "ores.refdata.cds_convention";
}

}

#endif
