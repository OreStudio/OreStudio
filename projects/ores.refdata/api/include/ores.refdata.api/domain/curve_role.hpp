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
#ifndef ORES_REFDATA_API_DOMAIN_CURVE_ROLE_HPP
#define ORES_REFDATA_API_DOMAIN_CURVE_ROLE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Instrument classification for curve-role derivation: deposit, fra, swap, or none.
 *
 * Reference data table defining the valid
 * [[id:4A02B9BB-B4B5-45DE-9036-C81EAEE3E921][instrument_code]] curve-role values: DEPOSIT, FRA,
 * SWAP -- the three pricing derivations ores.analytics.quant's curve_instrument_pricer implements
 * (simple rate, implied forward, par-rate solve respectively) -- and the sentinel NONE for the
 * great majority of instrument_code rows that are not curve instruments at all (options, credit,
 * equity, and so on). NONE exists so instrument_code.curve_role can stay a required, FK-validated
 * column rather than nullable free text, the same pattern
 * [[id:01E76440-B9A5-4D0A-A32C-B0C4A7484B26][tenor_unit.NONE]] already
 * establishes for tenor.unit. Managed by the system tenant, like other
 * refdata code tables.
 */
struct curve_role final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique curve role code.
     *
     * Examples: 'DEPOSIT', 'FRA', 'SWAP', 'NONE'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the curve role.
     */
    std::string name;

    /**
     * @brief Detailed description of the curve role.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this curve role.
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
 * @brief Dispatch-key identifier for curve_role, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const curve_role&) {
    return "ores.refdata.curve_role";
}

}

#endif
