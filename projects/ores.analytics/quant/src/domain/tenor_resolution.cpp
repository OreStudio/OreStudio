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
#include "ores.analytics.quant/domain/tenor_resolution.hpp"
#include <array>
#include <stdexcept>

namespace ores::analytics::quant::domain {

namespace {

struct kind_entry final {
    std::string_view code;
    tenor_kind kind;
};

constexpr std::array<kind_entry, 2> kinds{{
    {"PERIOD", tenor_kind::period},
    {"SPECIAL", tenor_kind::special},
}};

struct anchor_entry final {
    std::string_view code;
    tenor_anchor anchor;
};

/// The order is @c ores_refdata_tenor_anchors_tbl's, by code.
constexpr std::array<anchor_entry, 6> anchors{{
    {"IMM_ROLL", tenor_anchor::imm_roll},
    {"NEAR_LEG", tenor_anchor::near_leg},
    {"NONE", tenor_anchor::none},
    {"SPOT", tenor_anchor::spot},
    {"TODAY", tenor_anchor::today},
    {"TOMORROW", tenor_anchor::tomorrow},
}};

struct algorithm_entry final {
    std::string_view code;
    resolution_algorithm algorithm;
};

constexpr std::array<algorithm_entry, 2> algorithms{{
    {"ANCHOR_OFFSET", resolution_algorithm::anchor_offset},
    {"SCHEDULE_STEP", resolution_algorithm::schedule_step},
}};

} // namespace

std::string_view to_string(resolution_status status) {
    switch (status) {
        case resolution_status::ok:
            return "ok";
        case resolution_status::tenor_not_in_convention:
            return "tenor_not_in_convention";
        case resolution_status::calendar_index_out_of_range:
            return "calendar_index_out_of_range";
        case resolution_status::unsupported_anchor:
            return "unsupported_anchor";
        case resolution_status::missing_spot_date:
            return "missing_spot_date";
        case resolution_status::period_tenor_without_period:
            return "period_tenor_without_period";
        case resolution_status::special_tenor_without_offset:
            return "special_tenor_without_offset";
        case resolution_status::inconsistent_schedule_declaration:
            return "inconsistent_schedule_declaration";
        case resolution_status::schedule_without_dates:
            return "schedule_without_dates";
        case resolution_status::schedule_dates_not_ordered:
            return "schedule_dates_not_ordered";
        case resolution_status::schedule_exhausted:
            return "schedule_exhausted";
    }
    return "unknown";
}

tenor_kind parse_tenor_kind_code(std::string_view code) {
    for (const auto& entry : kinds) {
        if (entry.code == code)
            return entry.kind;
    }
    throw std::invalid_argument("Unrecognised tenor kind code: " + std::string(code));
}

std::string_view to_tenor_kind_code(tenor_kind kind) {
    for (const auto& entry : kinds) {
        if (entry.kind == kind)
            return entry.code;
    }
    throw std::invalid_argument("Tenor kind has no persisted code");
}

tenor_anchor parse_tenor_anchor_code(std::string_view code) {
    for (const auto& entry : anchors) {
        if (entry.code == code)
            return entry.anchor;
    }
    throw std::invalid_argument("Unrecognised tenor anchor code: " + std::string(code));
}

std::string_view to_tenor_anchor_code(tenor_anchor anchor) {
    for (const auto& entry : anchors) {
        if (entry.anchor == anchor)
            return entry.code;
    }
    throw std::invalid_argument("Tenor anchor has no persisted code");
}

resolution_algorithm parse_resolution_algorithm_code(std::string_view code) {
    for (const auto& entry : algorithms) {
        if (entry.code == code)
            return entry.algorithm;
    }
    throw std::invalid_argument("Unrecognised tenor resolution algorithm code: " +
                                std::string(code));
}

std::string_view to_resolution_algorithm_code(resolution_algorithm algorithm) {
    for (const auto& entry : algorithms) {
        if (entry.algorithm == algorithm)
            return entry.code;
    }
    throw std::invalid_argument("Tenor resolution algorithm has no persisted code");
}

} // namespace ores::analytics::quant::domain
