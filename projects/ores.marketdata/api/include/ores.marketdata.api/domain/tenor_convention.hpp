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
#ifndef ORES_MARKETDATA_API_DOMAIN_TENOR_CONVENTION_HPP
#define ORES_MARKETDATA_API_DOMAIN_TENOR_CONVENTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief Per-asset-class scheme governing how a tenor label resolves to a date (default anchor,
 * e.g. spot vs today vs near leg).
 *
 * Persisted catalog of the resolution schemes described in
 * [[id:0AC88EB3-DB7F-4135-9DA6-0ED4583FEC29][Tenor]]'s "Tenor conventions by
 * curve type" section — spot/forward curves, FX swap curves (near-leg
 * quoting), credit/CDS curves (IMM-anchored), and volatility surfaces
 * (measured from today rather than spot). Each row names the *default*
 * [[id:3F8B6C2A-1D4E-4A7F-9B3C-6E2D8F1A5C90][tenor anchor]] a convention's
 * regular PERIOD tenors resolve from; which [[id:0AC88EB3-DB7F-4135-9DA6-0ED4583FEC29][tenor]]
 * labels actually belong to a given convention, and any per-tenor anchor
 * override (needed for SPECIAL tenors such as O/N, which resolve
 * differently under the spot/forward convention than under the swap
 * convention), is recorded in
 * [[id:E1F5A9C3-6D2B-4E8A-B7F1-3C9D5A2E6B48][Tenor Convention Resolution]],
 * not here.
 */
struct tenor_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique convention code.
     *
     * Examples: 'RATES_SPOT_FORWARD', 'FX_SWAP_NEAR_LEG', 'CREDIT_CDS_IMM', 'VOL_SURFACE_TODAY'.
     */
    std::string code;

    /**
     * @brief Human-readable description of the resolution scheme and where it applies.
     */
    std::string description;

    /**
     * @brief Default [[id:3F8B6C2A-1D4E-4A7F-9B3C-6E2D8F1A5C90][tenor anchor]] code (references
     * tenor_anchor.code) this convention's regular PERIOD tenors resolve from.
     */
    std::string measured_from;

    /**
     * @brief Username of the person who last modified this tenor convention.
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
 * @brief Dispatch-key identifier for tenor_convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const tenor_convention&) {
    return "ores.marketdata.tenor_convention";
}

}

#endif
