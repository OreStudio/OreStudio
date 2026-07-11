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
#ifndef ORES_REFDATA_API_DOMAIN_CURRENCY_GROUP_HPP
#define ORES_REFDATA_API_DOMAIN_CURRENCY_GROUP_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Desk grouping of currencies (G11, Scandies, Antipodeans, ...).
 *
 * Reference data defining desk-style currency groupings. Unlike
 * currency.market_tier (single-valued primary liquidity tier), a
 * currency can belong to any number of groups simultaneously via
 * [[id:579032D2-3637-4188-859E-6C17C1D144F7][ores.refdata.currency_currency_group_junction]] (e.g.
 * NOK: G11 *and* SCANDIES *and* COMMODITY). Seeded with G11, SCANDIES, ANTIPODEANS, COMMODITY,
 * ASIANS, LATAMS — extensible by inserting a row, no schema change needed for a new group.
 */
struct currency_group final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique currency group code.
     *
     * Examples: 'G11', 'SCANDIES', 'ANTIPODEANS', 'COMMODITY', 'ASIANS', 'LATAMS'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the currency group.
     */
    std::string name;

    /**
     * @brief Detailed description of the currency group.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this currency group.
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
 * @brief Dispatch-key identifier for currency_group, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const currency_group&) {
    return "ores.refdata.currency_group";
}

}

#endif
