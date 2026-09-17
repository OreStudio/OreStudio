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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_API_DOMAIN_SERIES_SUBCLASS_CODE_HPP
#define ORES_REFDATA_API_DOMAIN_SERIES_SUBCLASS_CODE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Fine-grained market series subclass taxonomy (spot, forward, volatility, yield, basis,
 * fra, xccy, spread, index_credit, recovery, swap, capfloor, seasonality, price, correlation).
 *
 * Fine-grained classification of a market series within its asset class.
 * market_series.series_subclass carries one of these codes, so a query
 * can slice a tenant's series by shape ("all FX vol surfaces", "all
 * discount curves") without parsing the ORE key.
 *
 * Most codes are shared across asset classes: spot covers both FX spot
 * and equity spot, volatility covers FX options, swaptions and
 * commodity options alike. The table therefore does not partition by
 * asset_class_code; the pairing a producer actually emits is declared
 * where the series is written.
 *
 * This table is the single source of truth for the taxonomy. Code carries
 * no parallel enumeration, because the list is runtime-managed and no
 * compiled list can be exhaustive over it. market_series and
 * ir_curve_tick FK-validate against this table. Managed by the system
 * tenant, like other shared code tables.
 */
struct series_subclass_code final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique series subclass code.
     *
     * Examples: 'spot', 'forward', 'volatility', 'yield', 'basis', 'fra', 'xccy', 'spread',
     * 'index_credit', 'recovery', 'swap', 'capfloor', 'seasonality', 'price', 'correlation'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the series subclass.
     */
    std::string name;

    /**
     * @brief Detailed description of the series subclass.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this series subclass code.
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
 * @brief Dispatch-key identifier for series_subclass_code, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const series_subclass_code&) {
    return "ores.refdata.series_subclass_code";
}

}

#endif
