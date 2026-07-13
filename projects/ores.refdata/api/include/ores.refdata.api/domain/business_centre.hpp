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
#ifndef ORES_REFDATA_API_DOMAIN_BUSINESS_CENTRE_HPP
#define ORES_REFDATA_API_DOMAIN_BUSINESS_CENTRE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Financial trading location used for holiday calendar determination (FpML-style code).
 *
 * Business centres identify financial trading locations used for holiday
 * calendar determination (e.g. "USNY" for New York, "GBLO" for London),
 * using FpML-style codes. Coding-scheme scoped (a code is only unique
 * within its scheme, e.g. FpML vs ISDA) and optionally linked to a country
 * for flag-icon display in the Qt UI. Referenced by business_unit as a
 * soft FK.
 */
struct business_centre final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief FpML-style business centre code.
     *
     * Examples: 'USNY' (New York), 'GBLO' (London).
     */
    std::string code;

    /**
     * @brief Data source identifier (e.g. "FpML", "ISDA").
     */
    std::string source;

    /**
     * @brief Human-readable description of the business centre.
     */
    std::string description;

    /**
     * @brief City name derived from description.
     */
    std::string city_name;

    /**
     * @brief ISO 3166-1 alpha-2 code of the country. Soft FK to country.alpha2_code.
     */
    std::string country_alpha2_code;

    /**
     * @brief Code of the coding scheme this business centre belongs to (e.g. FpML vs ISDA). Soft FK
     * to ores_dq_coding_schemes_tbl, validated by
     * ores_refdata_validate_business_centre_coding_scheme_fn.
     */
    std::string coding_scheme_code;

    /**
     * @brief Username of the person who last modified this business centre.
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
 * @brief Dispatch-key identifier for business_centre, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const business_centre&) {
    return "ores.refdata.business_centre";
}

}

#endif
