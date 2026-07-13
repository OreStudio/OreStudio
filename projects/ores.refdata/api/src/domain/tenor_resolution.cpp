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
#include "ores.refdata.api/domain/tenor_resolution.hpp"
#include <stdexcept>

namespace ores::refdata::domain {

namespace {

std::chrono::year_month_day
add_offset(std::chrono::year_month_day anchor, const std::string& unit, int multiplier) {
    using namespace std::chrono;

    if (unit == "DAY")
        return year_month_day(sys_days(anchor) + days(multiplier));
    if (unit == "WEEK")
        return year_month_day(sys_days(anchor) + weeks(multiplier));
    if (unit == "MONTH") {
        auto shifted = anchor + months(multiplier);
        if (!shifted.ok())
            shifted = year_month_day_last(shifted.year() / shifted.month() / last);
        return shifted;
    }
    if (unit == "YEAR") {
        auto shifted = anchor + years(multiplier);
        if (!shifted.ok())
            shifted = year_month_day_last(shifted.year() / shifted.month() / last);
        return shifted;
    }
    if (unit == "ROLL_QUARTER")
        throw std::logic_error(
            "ROLL_QUARTER offsets require the IMM_ROLL algorithm, not add_offset");

    throw std::invalid_argument("Unrecognised offset unit: " + unit);
}

std::chrono::year_month_day
resolve_anchor_date(const std::string& anchor_code, std::chrono::year_month_day horizon,
    std::chrono::year_month_day spot) {
    using namespace std::chrono;

    if (anchor_code == "SPOT") return spot;
    if (anchor_code == "TODAY") return horizon;
    if (anchor_code == "TOMORROW") return year_month_day(sys_days(horizon) + days(1));

    throw std::invalid_argument(
        "Anchor '" + anchor_code + "' does not resolve to a concrete date under the "
        "ANCHOR_OFFSET algorithm (NEAR_LEG and IMM_ROLL require convention-specific handling)");
}

}

std::chrono::year_month_day
resolve_end_date(const tenor& t, const tenor_convention& convention,
    const std::optional<tenor_convention_resolution>& resolution,
    std::chrono::year_month_day horizon, std::chrono::year_month_day spot) {

    if (!resolution)
        throw std::invalid_argument(
            "Tenor '" + t.code + "' does not belong to convention '" + convention.code +
            "'s set (no tenor_convention_resolution row)");

    if (convention.resolution_algorithm == "IMM_ROLL")
        throw std::logic_error(
            "IMM_ROLL resolution is not yet implemented for convention '" + convention.code + "'");

    if (convention.resolution_algorithm != "ANCHOR_OFFSET")
        throw std::invalid_argument(
            "Unrecognised resolution_algorithm: " + convention.resolution_algorithm);

    const auto& anchor_code =
        resolution->anchor_override ? *resolution->anchor_override : convention.measured_from;
    const auto anchor_date = resolve_anchor_date(anchor_code, horizon, spot);

    if (t.kind == "SPECIAL") {
        if (!resolution->offset_unit || !resolution->offset_multiplier)
            throw std::invalid_argument(
                "SPECIAL tenor '" + t.code + "' under convention '" + convention.code +
                "' has no offset_unit/offset_multiplier in its resolution row");
        return add_offset(anchor_date, *resolution->offset_unit, *resolution->offset_multiplier);
    }

    if (t.kind == "PERIOD") {
        if (!t.multiplier)
            throw std::invalid_argument("PERIOD tenor '" + t.code + "' has no multiplier");
        return add_offset(anchor_date, t.unit, *t.multiplier);
    }

    throw std::invalid_argument("Unrecognised tenor kind: " + t.kind);
}

tenor_window resolve_window(const tenor& t, const tenor_convention& convention,
    const std::optional<tenor_convention_resolution>& resolution,
    std::chrono::year_month_day horizon, std::chrono::year_month_day spot) {
    return tenor_window{horizon, resolve_end_date(t, convention, resolution, horizon, spot)};
}

bool windows_overlap(const tenor_window& a, const tenor_window& b) {
    return a.start < b.end && b.start < a.end;
}

}
