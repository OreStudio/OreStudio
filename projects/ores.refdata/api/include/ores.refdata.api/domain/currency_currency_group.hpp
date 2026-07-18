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
#ifndef ORES_REFDATA_DOMAIN_CURRENCY_CURRENCY_GROUP_HPP
#define ORES_REFDATA_DOMAIN_CURRENCY_CURRENCY_GROUP_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Links a currency to a desk grouping it belongs to (G11, Scandies, ...).
 *
 * Many-to-many junction between currency and currency_group, mirroring
 * the existing party_currencies pattern. A currency can belong to any
 * number of groups simultaneously (e.g. NOK: G11 *and* SCANDIES *and*
 * COMMODITY) — see
 * [[id:04A121FA-00D6-43EB-9B21-04EDC1FA493D][Currency pair support in reference data]] for the
 * design rationale (replacing the original boolean is_g11 flag).
 */
struct currency_currency_group final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief ISO 4217 code of the currency.
     *
     * References ores_refdata_currencies_tbl.iso_code (soft FK).
     */
    std::string currency_iso_code;

    /**
     * @brief Code of the currency group this currency belongs to.
     *
     * References ores_refdata_currency_groups_tbl.code (soft FK).
     */
    std::string currency_group_code;

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
 * @brief Dispatch-key identifier for currency_currency_group, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const currency_currency_group&) {
    return "ores.refdata.currency_currency_group";
}

}

#endif
