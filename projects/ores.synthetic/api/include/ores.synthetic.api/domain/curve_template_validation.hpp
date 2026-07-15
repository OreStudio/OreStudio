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
#ifndef ORES_SYNTHETIC_API_DOMAIN_CURVE_TEMPLATE_VALIDATION_HPP
#define ORES_SYNTHETIC_API_DOMAIN_CURVE_TEMPLATE_VALIDATION_HPP

#include "ores.refdata.api/domain/tenor.hpp"
#include "ores.refdata.api/domain/tenor_convention.hpp"
#include "ores.refdata.api/domain/tenor_resolution.hpp"
#include "ores.synthetic.api/export.hpp"
#include <string>
#include <vector>

namespace ores::synthetic::domain {

/**
 * @brief Result of validating a Curve Template's tenor periods for
 * collisions. No UI or service dependency -- callable from both the Qt
 * client and server-side save handlers, same shape as
 * ores::analytics::quant::domain::process_parameter_validation_result.
 */
struct curve_template_validation_result final {
    bool valid = true;
    std::string message; // empty when valid
};

/**
 * @brief One ir_curve_template_entry's tenor references, reduced to just
 * what this validator needs (no uuid/party/config-id fields) so it stays a
 * pure function over plain data rather than requiring the full domain
 * entity.
 */
struct curve_template_entry_ref final {
    int sequence_index = 0;
    std::string start_tenor_code;
    std::string end_tenor_code;
    std::string instrument_code;
};

/**
 * @brief Rejects a Curve Template if any two entries' resolved tenor
 * periods genuinely collide -- per Ballabio's production-bootstrapping
 * principles, instrument periods that cross over cause the
 * multi-dimensional iterative solver to fail to converge or produce
 * erratic forward curves.
 *
 * Each entry's start_tenor_code/end_tenor_code are resolved to concrete
 * dates via ores::refdata::domain::resolve_end_date(), using the same
 * tenor catalog/convention/resolution rows the entity itself was
 * validated against. An entry whose start_tenor_code is "SPOT" (the
 * convention adopted by ir_curve_generation_config for point instruments
 * -- deposits, swaps) contributes only its maturity date to the
 * collision check, not the whole [SPOT, maturity) span every such
 * instrument shares -- two point instruments never collide with each
 * other by construction, since placing multiple maturity nodes "from
 * now" is exactly how a curve is built, not a collision. An entry whose
 * start_tenor_code is anything else (an interval instrument, e.g. an
 * FRA's own front tenor) contributes its genuine [start, end) period,
 * which must not overlap another interval's period, and must not have
 * another entry's maturity fall strictly inside it (the "swap tenor
 * landing inside a futures instrument's active delivery window" case).
 * Touching endpoints are not a collision (adjacent, back-to-back periods
 * are exactly how a bootstrappable strip is built).
 *
 * @param entries The Curve Template's entries, in any order (sequence_index
 * is carried through only for error messages, not relied on for adjacency).
 * @param tenors The tenant's tenor catalog (must include every tenor code
 * referenced by entries).
 * @param convention The tenor convention governing resolution (e.g.
 * RATES_SPOT_FORWARD).
 * @param resolutions The convention's (tenor, resolution) membership rows.
 * @param horizon The reference "as of" date every anchor is ultimately
 * relative to.
 * @param spot The spot date (horizon + spot days).
 *
 * @return An ok result if no collision was found, or a result carrying a
 * human-readable message identifying the colliding entries (or an
 * unresolvable tenor reference) otherwise. Never throws -- exceptions
 * from the underlying tenor resolution are caught and reported through
 * the result instead, so callers get a friendly ok/error-message pair.
 */
ORES_SYNTHETIC_API_EXPORT curve_template_validation_result
validate_curve_template(const std::vector<curve_template_entry_ref>& entries,
                        const std::vector<ores::refdata::domain::tenor>& tenors,
                        const ores::refdata::domain::tenor_convention& convention,
                        const std::vector<ores::refdata::domain::tenor_convention_resolution>&
                            resolutions,
                        std::chrono::year_month_day horizon,
                        std::chrono::year_month_day spot);

}

#endif
