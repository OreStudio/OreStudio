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
#include "ores.reporting.core/service/cut_state.hpp"
#include "ores.platform/time/datetime.hpp"
#include <chrono>
#include <format>
#include <string>

namespace ores::reporting::service {

namespace {

/// The family whose instant the run supplies, and so the family whose label
/// carries an instant instead of a take ordinal.
constexpr std::string_view consumer_family = "consumer";

/// The instant, to the second, in the shape a label states it.
std::string label_instant(const std::chrono::system_clock::time_point& timepoint) {
    const auto seconds = std::chrono::floor<std::chrono::seconds>(timepoint);
    const auto day = std::chrono::floor<std::chrono::days>(seconds);
    const std::chrono::year_month_day date(day);
    const std::chrono::hh_mm_ss<std::chrono::seconds> time(seconds - day);
    return std::format("{}T{:02}:{:02}:{:02}Z",
                       ores::platform::time::datetime::to_iso8601_date(date),
                       time.hours().count(),
                       time.minutes().count(),
                       time.seconds().count());
}

/// The date of the instant, in the shape a label states it.
std::string label_date(const std::chrono::system_clock::time_point& timepoint) {
    const std::chrono::year_month_day date(
        std::chrono::floor<std::chrono::days>(timepoint));
    return ores::platform::time::datetime::to_iso8601_date(date);
}

}

cut_state derive_state(const domain::cut& cut, int open_gap_count) {
    if (!cut.taken_at)
        return open_gap_count > 0 ? cut_state::incomplete : cut_state::draft;

    if (cut.refused_at)
        return cut_state::refused;

    if (cut.requires_authorisation && !cut.approved_at)
        return cut_state::taken;

    return cut.requires_authorisation ? cut_state::authorised : cut_state::in_use;
}

cut_state derive_state(const domain::cut& cut,
                       int open_gap_count,
                       bool later_cut_holds_its_place) {
    const auto state = derive_state(cut, open_gap_count);
    if (later_cut_holds_its_place && is_usable(state))
        return cut_state::superseded;
    return state;
}

bool is_usable(cut_state state) {
    switch (state) {
    case cut_state::in_use:
    case cut_state::authorised:
    case cut_state::superseded:
        return true;
    case cut_state::draft:
    case cut_state::incomplete:
    case cut_state::taken:
    case cut_state::refused:
        return false;
    }
    return false;
}

std::string_view to_string(cut_state state) {
    switch (state) {
    case cut_state::draft:
        return "draft";
    case cut_state::incomplete:
        return "incomplete";
    case cut_state::taken:
        return "taken";
    case cut_state::in_use:
        return "in_use";
    case cut_state::authorised:
        return "authorised";
    case cut_state::refused:
        return "refused";
    case cut_state::superseded:
        return "superseded";
    }
    return "unknown";
}

std::string cut_label_of(const domain::cut& cut, const domain::cut_kind& kind) {
    std::string label = cut.kind_code + "." + cut.scope_code + ".";

    if (kind.family_code == consumer_family)
        return label + label_instant(cut.timepoint);

    label += label_date(cut.timepoint);
    if (cut.take_ordinal > 1)
        label += "." + std::to_string(cut.take_ordinal);
    return label;
}

}
