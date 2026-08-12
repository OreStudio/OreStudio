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
#ifndef ORES_MARKETDATA_API_DOMAIN_TICK_SUBJECTS_HPP
#define ORES_MARKETDATA_API_DOMAIN_TICK_SUBJECTS_HPP

#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief The unified synthetic tick subject scheme: synthetic.v1.tick.<kind>.<source_name>.
 *
 * The kind token is the third subject segment and equals the factory kind
 * string of the producing feed (fx_spot_feed_kind / ir_curve_feed_kind in
 * ores.synthetic.api) — producers reuse the factory vocabulary as the
 * subject kind, with no per-kind subject family. The ingest loop dispatches
 * on this token, with one ingest branch per kind. The payloads
 * (fx_spot_tick / ir_curve_tick) are unchanged — the kind rides in the
 * subject, not in the wire type.
 *
 * Sandboxed feeds keep their own prefix (synthetic.v1.sandbox.tick.) — it is
 * structurally unreachable from the ingest loop's subscription and from any
 * bound-feed resolution path. The republished stream keeps its own prefix
 * (marketdata.v1.tick.) — the consumer-facing scheme is unchanged.
 */
inline constexpr std::string_view synthetic_tick_subject_prefix = "synthetic.v1.tick.";
inline constexpr std::string_view synthetic_sandbox_tick_subject_prefix =
    "synthetic.v1.sandbox.tick.";
inline constexpr std::string_view marketdata_tick_subject_prefix = "marketdata.v1.tick.";

/**
 * @brief Wire kind tokens; must match the factory kind strings (fx_spot_feed_kind,
 * ir_curve_feed_kind in ores.synthetic.api).
 */
inline constexpr std::string_view fx_spot_kind_token = "fx_spot";
inline constexpr std::string_view ir_curve_kind_token = "ir_curve";

/**
 * @brief Build the producer subject for a source_name: synthetic.v1.tick.<kind>.<source_name>.
 */
inline std::string synthetic_tick_subject(std::string_view kind, std::string_view source_name) {
    std::string subject;
    subject.reserve(synthetic_tick_subject_prefix.size() + kind.size() + 1 + source_name.size());
    subject.append(synthetic_tick_subject_prefix);
    subject.append(kind);
    subject.push_back('.');
    subject.append(source_name);
    return subject;
}

}

#endif
