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
#ifndef ORES_REFDATA_API_DOMAIN_FLOATING_INDEX_TYPE_HPP
#define ORES_REFDATA_API_DOMAIN_FLOATING_INDEX_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief ORE floating rate index code (e.g. EUR-EURIBOR-6M, USD-SOFR, GBP-SONIA).
 *
 * Reference data table defining valid floating rate indices used in
 * instrument floating leg definitions. Values are sourced from ORE
 * ore_types.xsd.
 *
 * /2026-07-18 moved from ores.trading to ores.refdata/: the Qt UI
 * already lived in ores.qt/refdata's RefdataPlugin, hand-wired
 * outside codegen; the backend followed to match, same direction as
 * Book/BusinessCentre's own moves to ores.refdata.
 */
struct floating_index_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique floating index type code in ISDA/ORE convention.
     *
     * Examples: 'EUR-EURIBOR-6M', 'USD-SOFR', 'GBP-SONIA', 'JPY-TONAR'.
     */
    std::string code;

    /**
     * @brief Human-readable description of the floating rate index.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this floating index type.
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
 * @brief Dispatch-key identifier for floating_index_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const floating_index_type&) {
    return "ores.refdata.floating_index_type";
}

}

#endif
