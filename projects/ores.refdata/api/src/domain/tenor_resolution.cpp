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
#include <algorithm>
#include <iterator>
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

    throw std::invalid_argument("Unrecognised offset unit: " + unit);
}

std::chrono::year_month_day resolve_anchor_date(const std::string& anchor_code,
                                                std::chrono::year_month_day horizon,
                                                std::chrono::year_month_day spot) {
    using namespace std::chrono;

    if (anchor_code == "SPOT")
        return spot;
    if (anchor_code == "TODAY")
        return horizon;
    if (anchor_code == "TOMORROW")
        return year_month_day(sys_days(horizon) + days(1));

    // The other anchors (NONE, NEAR_LEG, IMM_ROLL) never reach this function
    // with correct data: the resolution row's anchor_override supplies one of
    // the three resolvable codes above.
    throw std::invalid_argument("Anchor '" + anchor_code +
                                "' does not resolve to a concrete date (only SPOT, TODAY and "
                                "TOMORROW do)");
}

// The closed-form IMM roll rule: the first business day on-or-after the 20th
// of the quarter month (March, June, September, December). The 20th itself
// when it is a weekday; the next weekday otherwise (doc/knowledge/domain/
// imm_dates.org).
std::chrono::year_month_day roll_quarter_date(std::chrono::year y, std::chrono::month m) {
    using namespace std::chrono;

    auto d = year_month_day(y / m / day(20));
    while (weekday(sys_days(d)) == Saturday || weekday(sys_days(d)) == Sunday)
        d = year_month_day(sys_days(d) + days(1));
    return d;
}

// The n-th ROLL_QUARTER date on-or-after the anchor, walking the closed form
// forward year by year. The set is unbounded, so the walk always terminates.
std::chrono::year_month_day walk_roll_quarter(std::chrono::year_month_day anchor, int steps) {
    using namespace std::chrono;

    auto y = anchor.year();
    int found = 0;
    for (;;) {
        for (auto m : {month(3), month(6), month(9), month(12)}) {
            const auto d = roll_quarter_date(y, m);
            if (d >= anchor && ++found == steps)
                return d;
        }
        y += years(1);
    }
}

// The n-th date of an event-lookup schedule's set on-or-after the anchor, or
// nullopt when the set is exhausted (fewer than n dates qualify).
std::optional<std::chrono::year_month_day> nth_on_or_after(
    const std::vector<std::chrono::year_month_day>& dates,
    std::chrono::year_month_day anchor,
    int n) {
    using namespace std::chrono;

    if (n < 1)
        return std::nullopt;
    auto it = std::find_if(dates.begin(), dates.end(),
                           [anchor](year_month_day d) { return d >= anchor; });
    if (std::distance(it, dates.end()) < n)
        return std::nullopt;
    return *std::next(it, n - 1);
}

// The date a tenor's duration lands on from the anchor: the tenor's own
// period for a PERIOD tenor, the resolution row's offset for a SPECIAL tenor.
std::chrono::year_month_day resolve_offset_date(
    const tenor& t,
    const std::optional<tenor_convention_resolution>& resolution,
    const tenor_convention& convention,
    std::chrono::year_month_day anchor_date) {
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

}

std::chrono::year_month_day
resolve_end_date(const tenor& t,
                 const tenor_convention& convention,
                 const std::optional<tenor_convention_resolution>& resolution,
                 std::chrono::year_month_day horizon,
                 std::chrono::year_month_day spot,
                 const std::optional<std::vector<std::chrono::year_month_day>>& schedule_dates) {

    if (!resolution)
        throw std::invalid_argument("Tenor '" + t.code + "' does not belong to convention '" +
                                    convention.code +
                                    "'s set (no tenor_convention_resolution row)");

    const auto& anchor_code =
        resolution->anchor_override ? *resolution->anchor_override : convention.measured_from;
    const auto anchor_date = resolve_anchor_date(anchor_code, horizon, spot);

    if (convention.resolution_algorithm == "SCHEDULE_STEP") {
        if (!resolution->schedule_code)
            throw std::logic_error("SCHEDULE_STEP resolution for tenor '" + t.code +
                                   "' under convention '" + convention.code +
                                   "' has no schedule_code in its resolution row");
        if (!resolution->schedule_step_count)
            throw std::logic_error("SCHEDULE_STEP resolution for tenor '" + t.code +
                                   "' under convention '" + convention.code +
                                   "' has no schedule_step_count in its resolution row");

        // Anchor + calendar offset, resolved exactly as ANCHOR_OFFSET resolves
        // them; then the walk starts from that date.
        const auto walk_start = resolve_offset_date(t, resolution, convention, anchor_date);

        // The closed-form IMM rule (ROLL_QUARTER) needs no event store: the
        // dates are computed code-side.
        if (*resolution->schedule_code == "ROLL_QUARTER")
            return walk_roll_quarter(walk_start, *resolution->schedule_step_count);

        // Event-lookup schedule (FOMC_MEETING is the only instance): walk the
        // caller-supplied dates. Missing dates or exhaustion are
        // configuration/data errors.
        if (!schedule_dates || schedule_dates->empty())
            throw std::logic_error("SCHEDULE_STEP resolution for tenor '" + t.code +
                                   "' under convention '" + convention.code + "' has schedule '" +
                                   *resolution->schedule_code +
                                   "' but no event-lookup dates were supplied");
        const auto walked =
            nth_on_or_after(*schedule_dates, walk_start, *resolution->schedule_step_count);
        if (!walked)
            throw std::logic_error("SCHEDULE_STEP resolution for tenor '" + t.code +
                                   "' under convention '" + convention.code + "' has schedule '" +
                                   *resolution->schedule_code + "' which is exhausted: fewer than " +
                                   std::to_string(*resolution->schedule_step_count) +
                                   " dates on-or-after the walk start");
        return *walked;
    }

    if (convention.resolution_algorithm != "ANCHOR_OFFSET")
        throw std::invalid_argument("Unrecognised resolution_algorithm: " +
                                    convention.resolution_algorithm);

    return resolve_offset_date(t, resolution, convention, anchor_date);
}

tenor_window resolve_window(const tenor& t,
                            const tenor_convention& convention,
                            const std::optional<tenor_convention_resolution>& resolution,
                            std::chrono::year_month_day horizon,
                            std::chrono::year_month_day spot) {
    return tenor_window{horizon, resolve_end_date(t, convention, resolution, horizon, spot)};
}

bool windows_overlap(const tenor_window& a, const tenor_window& b) {
    return a.start < b.end && b.start < a.end;
}

}
