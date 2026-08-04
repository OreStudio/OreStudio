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
#ifndef ORES_REFDATA_API_DOMAIN_DERIVATION_KIND_HPP
#define ORES_REFDATA_API_DOMAIN_DERIVATION_KIND_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Classifies a market_series as directly observed or derived by a named mechanism.
 *
 * Reference data table defining the valid market_series.derivation_kind
 * values: the sentinel OBSERVED (the default case -- a directly
 * published tick, no derivation involved) plus a named value per
 * derivation mechanism (IR_CURVE_BOOTSTRAP, CRM_DERIVATION, ...).
 * OBSERVED exists so market_series.derivation_kind can stay a
 * required, FK-validated column rather than nullable free text, the same
 * pattern [[id:D9A9B2C5-2C22-456A-851F-B458E0568CD3][curve_role.NONE]] already establishes for
 * instrument_code.curve_role. Deliberately generic and not
 * curve-specific: a market_series row whose derivation_kind <>
 * 'OBSERVED' carries a derivation_config_id/derivation_config_version
 * pointing at the recipe that produced it (the bootstrap config for
 * IR_CURVE_BOOTSTRAP, the CRM topology config for CRM_DERIVATION),
 * letting any published point answer "was this observed or computed, and
 * by what" without guessing from the source string. Managed by the
 * system tenant, like other refdata code tables.
 */
struct derivation_kind final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique derivation kind code.
     *
     * Examples: 'OBSERVED', 'IR_CURVE_BOOTSTRAP', 'CRM_DERIVATION'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the derivation kind.
     */
    std::string name;

    /**
     * @brief Detailed description of the derivation kind.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this derivation kind.
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
 * @brief Dispatch-key identifier for derivation_kind, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const derivation_kind&) {
    return "ores.refdata.derivation_kind";
}

}

#endif
