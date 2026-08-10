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
// Test obligations propagated from doc/llm/specs/fomc-dated-ois-short-end.allium.
//
// The SCHEDULE_STEP branch of the resolver, added by story Decision D2 (task
// unified-tenor-resolution): a tenor resolves as anchor + calendar offset +
// n steps along the FOMC_MEETING schedule axis, walking the meeting events
// supplied by the caller as data (the same dates task D4 seeds into
// calendar_event from federalreserve.gov).
//
// Obligations covered here:
//   - contract-signature.ScheduleWalk.nth       (the walk, made executable)
//   - rule-success.ResolveScheduleStep          (1F, 8F resolve on-or-after spot)
//   - rule-success.ResolutionRejectedWhenScheduleExhausted (9F > 8 meetings)
//   - rule-failure.ResolveScheduleStep.* (the three resolution-row/data
//     guards: missing schedule_code, missing schedule_step_count, and
//     missing/empty event-lookup dates all reject as std::logic_error)
//   - rule-failure.ResolveScheduleStep.1        (algorithm != schedule_step:
//     the rule does not fire -- covered by the ANCHOR_OFFSET tests in
//     domain_tenor_resolution_tests.cpp, referenced, not duplicated)
//   - rule-success.ResolveAnchorOffset and rule-failure.ResolveAnchorOffset.1:
//     already covered by domain_tenor_resolution_tests.cpp.
//
// The meeting-date set below is the same data task D4 (calendar-seeding)
// transcribes into calendar_event from federalreserve.gov.
#include "ores.refdata.api/domain/tenor_resolution.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <iterator>
#include <string>
#include <vector>

namespace {

using ores::refdata::domain::resolve_end_date;
using ores::refdata::domain::tenor;
using ores::refdata::domain::tenor_convention;
using ores::refdata::domain::tenor_convention_resolution;

const std::string tags("[tenor_resolution][fomc]");

// The 2026 FOMC meeting dates, transcribed from federalreserve.gov. Each
// date is the *second day* of the Fed's published two-day range -- the
// statement/decision day, usually a Wednesday (story decision, 2026-08-10).
// Task D4 seeds the same 2025-2027 window into calendar_event; the D2
// FOMC_MEETING schedule lookup walks this same data.
std::vector<std::chrono::year_month_day> fomc_2026_meeting_dates() {
    using namespace std::chrono;
    return {year(2026) / month(1) / day(28),   year(2026) / month(3) / day(18),
            year(2026) / month(4) / day(29),   year(2026) / month(6) / day(17),
            year(2026) / month(7) / day(29),   year(2026) / month(9) / day(16),
            year(2026) / month(10) / day(28),  year(2026) / month(12) / day(9)};
}

// The spec's ScheduleWalk.nth contract, made executable: the n-th date of
// the schedule's set on-or-after the anchor (anchor + calendar offset; the
// FOMC convention's offset is zero, measured from SPOT).
std::chrono::year_month_day nth_on_or_after(
    const std::vector<std::chrono::year_month_day>& dates,
    std::chrono::year_month_day anchor,
    int n) {
    using namespace std::chrono;
    auto it = std::find_if(dates.begin(), dates.end(),
                           [anchor](year_month_day d) { return d >= anchor; });
    std::advance(it, n - 1);
    return *it;
}

tenor make_fomc_tenor(int n) {
    tenor t;
    t.code = std::to_string(n) + "F";
    t.kind = "SPECIAL";
    t.unit = "NONE";
    t.multiplier = n;
    return t;
}

tenor_convention make_fomc_convention() {
    tenor_convention c;
    c.code = "RATES_SPOT_FOMC";
    c.measured_from = "SPOT";
    c.resolution_algorithm = "SCHEDULE_STEP";
    return c;
}

tenor_convention_resolution make_fomc_resolution(int n) {
    tenor_convention_resolution r;
    r.convention_code = "RATES_SPOT_FOMC";
    r.tenor_code = std::to_string(n) + "F";
    // SPECIAL tenors carry the zero calendar-axis offset in the row; the
    // walk reads the meeting events off US.FOMC (seeded by task D4) and
    // takes n steps along them.
    r.offset_unit = "DAY";
    r.offset_multiplier = 0;
    r.schedule_code = "FOMC_MEETING";
    r.schedule_step_count = n;
    return r;
}

}

TEST_CASE("SCHEDULE_STEP resolves 1F to the first FOMC meeting on-or-after spot", tags) {
    using namespace std::chrono;

    const auto horizon = year(2026) / month(1) / day(1);
    const auto spot = year(2026) / month(1) / day(2);
    const auto meetings = fomc_2026_meeting_dates();

    const auto end = resolve_end_date(make_fomc_tenor(1),
                                      make_fomc_convention(),
                                      make_fomc_resolution(1),
                                      horizon,
                                      spot,
                                      meetings);

    CHECK(end == nth_on_or_after(meetings, spot, 1));
}

TEST_CASE("SCHEDULE_STEP resolves 8F to the eighth FOMC meeting on-or-after spot", tags) {
    using namespace std::chrono;

    const auto horizon = year(2026) / month(1) / day(1);
    const auto spot = year(2026) / month(1) / day(2);
    const auto meetings = fomc_2026_meeting_dates();

    // Eight meetings span roughly one year -- the pillar count that takes
    // the segment to the 1Y split tenor (settled 2026-08-10).
    const auto end = resolve_end_date(make_fomc_tenor(8),
                                      make_fomc_convention(),
                                      make_fomc_resolution(8),
                                      horizon,
                                      spot,
                                      meetings);

    CHECK(end == nth_on_or_after(meetings, spot, 8));
}

TEST_CASE("SCHEDULE_STEP rejects resolution when the schedule is exhausted", tags) {
    using namespace std::chrono;

    const auto horizon = year(2026) / month(1) / day(1);
    const auto spot = year(2026) / month(1) / day(2);
    const auto meetings = fomc_2026_meeting_dates();

    // Nine steps but only eight meetings in the window: the resolution must
    // fail rather than walk past the end of the schedule. Exhaustion is a
    // config/data error, so it reports as std::logic_error -- the same
    // error family the resolver uses for its other unwalkable states.
    CHECK_THROWS_AS(resolve_end_date(make_fomc_tenor(9),
                                     make_fomc_convention(),
                                     make_fomc_resolution(9),
                                     horizon,
                                     spot,
                                     meetings),
                    std::logic_error);
}

TEST_CASE("SCHEDULE_STEP rejects resolution with no schedule_code in the row", tags) {
    using namespace std::chrono;

    const auto horizon = year(2026) / month(1) / day(1);
    const auto spot = year(2026) / month(1) / day(2);

    // A resolution row that names the walk but not the schedule axis it
    // walks: the step count alone cannot drive a walk, so the resolution
    // rejects -- a configuration error, reported as std::logic_error.
    auto resolution = make_fomc_resolution(1);
    resolution.schedule_code = std::nullopt;

    CHECK_THROWS_AS(resolve_end_date(make_fomc_tenor(1),
                                     make_fomc_convention(),
                                     resolution,
                                     horizon,
                                     spot,
                                     fomc_2026_meeting_dates()),
                    std::logic_error);
}

TEST_CASE("SCHEDULE_STEP rejects resolution with no schedule_step_count in the row", tags) {
    using namespace std::chrono;

    const auto horizon = year(2026) / month(1) / day(1);
    const auto spot = year(2026) / month(1) / day(2);

    // A resolution row that names the schedule axis but not how many steps
    // to take along it: the walk has no length, so the resolution rejects
    // -- a configuration error, reported as std::logic_error.
    auto resolution = make_fomc_resolution(1);
    resolution.schedule_step_count = std::nullopt;

    CHECK_THROWS_AS(resolve_end_date(make_fomc_tenor(1),
                                     make_fomc_convention(),
                                     resolution,
                                     horizon,
                                     spot,
                                     fomc_2026_meeting_dates()),
                    std::logic_error);
}

TEST_CASE("SCHEDULE_STEP rejects resolution with no event-lookup dates supplied", tags) {
    using namespace std::chrono;

    const auto horizon = year(2026) / month(1) / day(1);
    const auto spot = year(2026) / month(1) / day(2);

    // An event-lookup walk with no dates to walk: neither the defaulted
    // nullopt nor an explicitly empty set is walkable. The dates are caller
    // data, so this is a data error, reported as std::logic_error.
    CHECK_THROWS_AS(resolve_end_date(make_fomc_tenor(1),
                                     make_fomc_convention(),
                                     make_fomc_resolution(1),
                                     horizon,
                                     spot),
                    std::logic_error);

    const std::vector<std::chrono::year_month_day> no_meetings{};
    CHECK_THROWS_AS(resolve_end_date(make_fomc_tenor(1),
                                     make_fomc_convention(),
                                     make_fomc_resolution(1),
                                     horizon,
                                     spot,
                                     no_meetings),
                    std::logic_error);
}
