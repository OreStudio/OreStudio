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
#ifndef ORES_REFDATA_DOMAIN_TENOR_CONVENTION_RESOLUTION_HPP
#define ORES_REFDATA_DOMAIN_TENOR_CONVENTION_RESOLUTION_HPP

#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Joins a tenor convention to the tenors valid under it, with an optional per-tenor anchor
 * override.
 *
 * A [[id:0AC88EB3-DB7F-4135-9DA6-0ED4583FEC29][tenor]] does not resolve the
 * same way under every [[id:C4D8A2E6-3B7F-4A1D-9C5E-8F2A6D3B1E90][tenor
 * convention]] — O/N resolves from horizon to tomorrow under the
 * spot/forward convention, but means "today" (zero offset) under the FX
 * swap convention — and not every tenor belongs to every convention's set
 * at all (the swap convention's tenor set stops at S/N; the credit/CDS
 * convention only uses IMM-quarter labels). This junction records all of
 * that as one row per valid (convention, tenor) pair: the row's mere
 * presence is set membership; its optional anchor_override column carries
 * the per-row exception to the convention's own default anchor; and its
 * optional offset_unit/offset_multiplier columns carry the
 * convention-specific duration for a SPECIAL tenor, which — unlike a
 * PERIOD tenor — has no unit/multiplier of its own to fall back on
 * (see [[id:9A2E4D6B-7C1F-4B8A-A5D3-2F6E9B1C4A87][Tenor]]'s kind column).
 * A PERIOD tenor's resolution never needs an offset override — its
 * duration is fixed by its own unit/multiplier regardless of
 * convention, so only the anchor itself (the convention's measured_from,
 * or this row's anchor_override) can vary for those rows. For a
 * convention whose resolution_algorithm is IMM_ROLL rather than
 * ANCHOR_OFFSET, offset_unit is ROLL_QUARTER and offset_multiplier
 * is the roll-quarter count (e.g. 1Y 1RQ, per
 * [[id:0AC88EB3-DB7F-4135-9DA6-0ED4583FEC29][Tenor]]'s CDS disambiguation
 * convention) — the same two columns, reused rather than requiring a
 * separate schema for the different algorithm. The runtime resolver for
 * IMM_ROLL rows is not implemented yet; see the capture referenced below.
 */
struct tenor_convention_resolution final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief References [[id:C4D8A2E6-3B7F-4A1D-9C5E-8F2A6D3B1E90][tenor_convention.code]].
     */
    std::string convention_code;

    /**
     * @brief References [[id:9A2E4D6B-7C1F-4B8A-A5D3-2F6E9B1C4A87][tenor.code]].
     */
    std::string tenor_code;

    /**
     * @brief References [[id:3F8B6C2A-1D4E-4A7F-9B3C-6E2D8F1A5C90][tenor_anchor.code]]. Null for
     * the common case (the tenor resolves from the convention's own measured_from); set only when
     * this specific tenor resolves from a different anchor under this specific convention.
     */
    std::optional<std::string> anchor_override;

    /**
     * @brief One of DAY, WEEK, MONTH, YEAR, or ROLL_QUARTER. Null when the referenced tenor is
     * PERIOD (its own unit applies instead); required when the referenced tenor is SPECIAL, since a
     * SPECIAL tenor has no duration outside a convention's say-so.
     */
    std::optional<std::string> offset_unit;

    /**
     * @brief The count paired with offset_unit (e.g. 1 day for O/N under spot/forward, 0 days for
     * O/N under FX swap, 1 roll-quarter for a CDS 1Y 1RQ tenor). Same nullability rule as
     * offset_unit.
     */
    std::optional<int> offset_multiplier;

    /**
     * @brief Username of the person who last modified this tenor convention resolution.
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
 * @brief Dispatch-key identifier for tenor_convention_resolution, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const tenor_convention_resolution&) {
    return "ores.refdata.tenor_convention_resolution";
}

}

#endif
