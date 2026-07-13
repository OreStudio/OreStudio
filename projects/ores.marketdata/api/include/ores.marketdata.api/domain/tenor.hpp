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
#ifndef ORES_MARKETDATA_API_DOMAIN_TENOR_HPP
#define ORES_MARKETDATA_API_DOMAIN_TENOR_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief Catalog of standard tenor labels (O/N, 1W, 1Y, SPOT, ...), independent of any curve-type
 * resolution convention.
 *
 * Persisted catalog of the standard [[id:0AC88EB3-DB7F-4135-9DA6-0ED4583FEC29][tenor]]
 * labels, so a user can see, manage, and order the set of tenors the system
 * knows about rather than relying on labels implied by parsing logic alone.
 * This table carries convention-independent identity only — how a given
 * tenor resolves under a particular curve type is
 * [[id:C4D8A2E6-3B7F-4A1D-9C5E-8F2A6D3B1E90][Tenor Convention]]'s concern,
 * recorded via [[id:E1F5A9C3-6D2B-4E8A-B7F1-3C9D5A2E6B48][Tenor Convention
 * Resolution]]. [[id:4DF1F49A-9AB4-4B56-B170-808490071799][Broken dates]]
 * are deliberately out of scope — they are per-instance ladder data, not
 * master data, and reference a row here only when they happen to coincide
 * with a standard label.
 */
struct tenor final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique tenor code.
     *
     * Examples: 'O/N', 'T/N', 'S/N', 'S/W', '1W', '1M', '1Y', 'SPOT'.
     */
    std::string code;

    /**
     * @brief Human-readable common name (e.g. 'Overnight', 'One Month', 'Spot').
     */
    std::string display_name;

    /**
     * @brief Longer-form description of the tenor and where it typically appears.
     */
    std::string description;

    /**
     * @brief Explicit display/ladder ordering. Not derived from parsing the code, so labels with no
     * natural lexicographic order relative to periods (e.g. SPOT among O/N/T/N/S/N) sort correctly.
     */
    int sort_order = 0;

    /**
     * @brief Either SPECIAL (O/N, T/N, S/N, SPOT, TODAY, TOMORROW — no fixed offset; resolved by
     * rule per convention) or PERIOD (regular nD/nW/nM/nY labels with a fixed offset from the
     * convention's anchor).
     */
    std::string kind;

    /**
     * @brief Period unit for PERIOD tenors: DAY, WEEK, MONTH, or YEAR. Null for SPECIAL tenors.
     */
    std::string unit;

    /**
     * @brief Period count for PERIOD tenors (the n in nD/nW/nM/nY). Null for SPECIAL tenors.
     */
    std::optional<int> multiplier;

    /**
     * @brief Username of the person who last modified this tenor.
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
 * @brief Dispatch-key identifier for tenor, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const tenor&) {
    return "ores.marketdata.tenor";
}

}

#endif
