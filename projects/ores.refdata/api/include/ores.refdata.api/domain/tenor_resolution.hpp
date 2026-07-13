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
#ifndef ORES_REFDATA_API_DOMAIN_TENOR_RESOLUTION_HPP
#define ORES_REFDATA_API_DOMAIN_TENOR_RESOLUTION_HPP

#include "ores.refdata.api/domain/tenor.hpp"
#include "ores.refdata.api/domain/tenor_convention.hpp"
#include "ores.refdata.api/export.hpp"
#include <chrono>
#include <optional>
#include <string>

namespace ores::refdata::domain {

/**
 * @brief One row of the ores_refdata_tenor_convention_resolutions_tbl join table (see
 * ores.refdata.tenor_convention_resolution.org). Hand-authored: codegen's junction profile
 * generates SQL only, no C++ struct, since a junction has no domain-object identity of its own —
 * this is the resolver's own reading of that row's meaningful columns, not a generated artefact.
 */
struct tenor_convention_resolution final {
    std::string convention_code;
    std::string tenor_code;

    /**
     * @brief References tenor_anchor.code. Empty when the tenor resolves from the convention's own
     * measured_from.
     */
    std::optional<std::string> anchor_override;

    /**
     * @brief One of DAY, WEEK, MONTH, YEAR, or ROLL_QUARTER. Required when the referenced tenor is
     * SPECIAL; unused when it is PERIOD (the tenor's own unit applies instead).
     */
    std::optional<std::string> offset_unit;

    /**
     * @brief The count paired with offset_unit. Same nullability rule as offset_unit.
     */
    std::optional<int> offset_multiplier;
};

/**
 * @brief A half-open date window [start, end) anchored to a horizon date and a resolved tenor.
 */
struct tenor_window final {
    std::chrono::year_month_day start;
    std::chrono::year_month_day end;
};

/**
 * @brief Resolves a tenor to a concrete end date under a specific convention, reading only the
 * persisted catalog rows — no per-label knowledge is hardcoded here. The same tenor resolves to
 * different dates under different conventions purely because the input rows differ (see
 * doc/knowledge/domain/tenor.org's "Tenor conventions by curve type").
 *
 * @param t The tenor being resolved. Must be a row from the same tenant's tenor catalog.
 * @param convention The convention governing resolution.
 * @param resolution The (convention, tenor) membership/override row, if one exists. A tenor not
 * belonging to a convention's set (no such row) is a caller error, reported by throwing.
 * @param horizon The reference "as of" date every anchor is ultimately relative to.
 * @param spot The spot date (horizon + spot days), supplied by the caller since spot-day
 * resolution is currency-pair-specific and out of scope here (see doc/knowledge/domain/
 * fx_spot_date_and_settlement.org).
 *
 * @throws std::invalid_argument if the tenor does not belong to the convention's set, if a
 * SPECIAL tenor's resolution row is missing its offset, or if an anchor code is not one this
 * function resolves to a concrete date (SPOT, TODAY, TOMORROW).
 * @throws std::logic_error if convention.resolution_algorithm is IMM_ROLL — that algorithm is not
 * yet implemented (see the capture for adding it).
 */
ORES_REFDATA_API_EXPORT std::chrono::year_month_day
resolve_end_date(const tenor& t, const tenor_convention& convention,
    const std::optional<tenor_convention_resolution>& resolution,
    std::chrono::year_month_day horizon, std::chrono::year_month_day spot);

/**
 * @brief Resolves a (horizon, tenor, convention) triple into its [start, end) date window, where
 * start is the horizon date itself. See resolve_end_date() for the parameters and error modes.
 */
ORES_REFDATA_API_EXPORT tenor_window
resolve_window(const tenor& t, const tenor_convention& convention,
    const std::optional<tenor_convention_resolution>& resolution,
    std::chrono::year_month_day horizon, std::chrono::year_month_day spot);

/**
 * @brief Whether two half-open date windows [a.start, a.end) and [b.start, b.end) overlap.
 */
ORES_REFDATA_API_EXPORT bool windows_overlap(const tenor_window& a, const tenor_window& b);

}

#endif
