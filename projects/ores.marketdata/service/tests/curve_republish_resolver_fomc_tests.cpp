/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 3 of the License, or (at your option)
 * any later version.
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
// The FOMC bootstrap segment, shaped as story Decision D3 (pillars SPOT->1F,
// 1F->2F, ..., 8F->1Y under one config; split_tenor_code = 1Y), walked
// through the existing republish resolver. These tests pass today -- the
// chain is existing behaviour; only the refdata rows (tenor_schedules,
// event seeds, the config itself) are missing in production.
//
// Obligations covered here:
//   - surface-exposure.CurveRepublish      (the pillar loop: codes and dates)
//   - surface-provides.CurveRepublish      (resolution available per pillar)
//   - rule-success.ResolvePillarDates      (chained: pillar dates = resolved dates)
//   - invariant.PillarDatesFollowSequence  (end dates ascending in sequence order)
//   - guarantee PointIdsHaveQuotes         (point id = end tenor code; a missing
//     quote fails loudly)
//   - derived.Tenor.is_fomc                (SPECIAL/NONE, multiplier >= 1 pillars,
//     as data)
//   - Story acceptance: the FOMC-to-FOMC fixing schedule falls out of the
//     pillar order.
//
// The resolution rows here use ANCHOR_OFFSET day offsets that land on the
// meeting dates. Task D2 re-expresses them as FOMC_MEETING schedule steps;
// the expected dates are the same meeting dates either way. The SCHEDULE_STEP
// walk itself is the contract of domain_tenor_resolution_fomc_tests.cpp
// (ores.refdata.api).
#include "../src/curve_republish_resolver.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

namespace {

const std::string tags("[curve_republish_resolver][fomc]");

using namespace std::chrono;
using ores::marketdata::service::curve_republish_refdata_context;
using ores::marketdata::service::resolve_bootstrap_pillars;
using ores::marketdata::service::resolve_tenor_date;
using ores::refdata::domain::ir_curve_bootstrap_pillar;
using ores::refdata::domain::tenor;
using ores::refdata::domain::tenor_convention_resolution;

constexpr auto meeting_count = 8;
const std::string split_code = "1Y";

// 2026 FOMC meeting dates -- the second day of each published two-day range
// (statement/decision day), transcribed from federalreserve.gov; the same
// window task D4 seeds into calendar_event.
std::vector<year_month_day> fomc_2026_meeting_dates() {
    return {2026y / January / 28d, 2026y / March / 18d, 2026y / April / 29d,
            2026y / June / 17d,    2026y / July / 29d,  2026y / September / 16d,
            2026y / October / 28d, 2026y / December / 9d};
}

tenor make_tenor(const std::string& code, const std::string& kind, const std::string& unit,
                 int multiplier) {
    tenor t;
    t.code = code;
    t.kind = kind;
    t.unit = unit;
    t.multiplier = multiplier;
    return t;
}

tenor_convention_resolution make_resolution(const std::string& tenor_code, int offset_days) {
    tenor_convention_resolution r;
    r.convention_code = "RATES_SPOT_FOMC";
    r.tenor_code = tenor_code;
    r.offset_unit = "DAY";
    r.offset_multiplier = offset_days;
    return r;
}

ir_curve_bootstrap_pillar
make_pillar(int seq, const std::string& start, const std::string& end, const std::string& role) {
    ir_curve_bootstrap_pillar p;
    p.sequence_index = seq;
    p.start_tenor_code = start;
    p.end_tenor_code = end;
    p.curve_role_code = role;
    return p;
}

// The D3 pillar list: SPOT->1F, 1F->2F, ..., 8F->1Y under one config. The
// FOMC pillars carry the spec's is_fomc shape (SPECIAL/NONE, multiplier
// >= 1); the 1Y split pillar is a plain period tenor.
std::vector<ir_curve_bootstrap_pillar> make_fomc_pillars() {
    std::vector<ir_curve_bootstrap_pillar> out;
    out.push_back(make_pillar(0, "SPOT", "1F", "DEPOSIT"));
    for (int n = 1; n < meeting_count; ++n)
        out.push_back(
            make_pillar(n, std::to_string(n) + "F", std::to_string(n + 1) + "F", "SWAP"));
    out.push_back(make_pillar(meeting_count, "8F", split_code, "SWAP"));
    return out;
}

curve_republish_refdata_context make_fomc_context() {
    curve_republish_refdata_context ctx;
    // horizon == value_date == spot (T+0), the resolver's simplification.
    ctx.horizon = 2026y / January / 2d;

    const auto meetings = fomc_2026_meeting_dates();
    ctx.tenors_by_code.emplace("SPOT", make_tenor("SPOT", "PERIOD", "DAY", 0));
    ctx.tenors_by_code.emplace(split_code, make_tenor(split_code, "PERIOD", "YEAR", 1));
    for (int n = 1; n <= meeting_count; ++n) {
        const auto code = std::to_string(n) + "F";
        ctx.tenors_by_code.emplace(code, make_tenor(code, "SPECIAL", "NONE", n));
        // ANCHOR_OFFSET day offsets landing on the meeting dates (D2 will
        // re-express these as FOMC_MEETING schedule steps).
        ctx.resolutions_by_tenor.emplace(
            code, make_resolution(code, (sys_days(meetings[n - 1]) - sys_days(ctx.horizon)).count()));
    }
    ctx.resolutions_by_tenor.emplace("SPOT", make_resolution("SPOT", 0));
    ctx.resolutions_by_tenor.emplace(split_code, make_resolution(split_code, 0));

    ctx.convention.code = "RATES_SPOT_FOMC";
    ctx.convention.measured_from = "SPOT";
    ctx.convention.resolution_algorithm = "ANCHOR_OFFSET";
    return ctx;
}

std::unordered_map<std::string, double> make_fomc_raw_rates() {
    std::unordered_map<std::string, double> raw_rates;
    for (int n = 1; n <= meeting_count; ++n)
        raw_rates[std::to_string(n) + "F"] = 0.03 + 0.001 * n;
    // The 8F->1Y split pillar's point id is its end tenor code: the raw grid
    // must carry a 1Y quote too (the boundary where the swap grid begins).
    raw_rates[split_code] = 0.04;
    return raw_rates;
}

}

TEST_CASE("resolve_bootstrap_pillars resolves the FOMC pillar chain onto the meeting dates",
          tags) {
    const auto ctx = make_fomc_context();
    const auto meetings = fomc_2026_meeting_dates();

    const auto resolved = resolve_bootstrap_pillars(make_fomc_pillars(), ctx,
                                                    make_fomc_raw_rates());

    REQUIRE(resolved.size() == meeting_count + 1);
    // SPOT->1F: the deposit pillar ends at the first meeting.
    CHECK(resolved[0].point_id == "1F");
    CHECK(resolved[0].end_date == meetings[0]);
    // 1F->2F, ..., 7F->8F: each swap pillar ends at the next meeting.
    for (int n = 2; n <= meeting_count; ++n) {
        CHECK(resolved[n - 1].point_id == std::to_string(n) + "F");
        CHECK(resolved[n - 1].end_date == meetings[n - 1]);
    }
    // 8F->1Y: the final pillar ends at the split tenor (spot + 1 year).
    CHECK(resolved[meeting_count].point_id == split_code);
    CHECK(resolved[meeting_count].end_date == resolve_tenor_date(ctx, split_code));
}

TEST_CASE("resolve_bootstrap_pillars chains each FOMC pillar's start date onto the previous "
          "pillar's end date",
          tags) {
    const auto ctx = make_fomc_context();

    const auto resolved = resolve_bootstrap_pillars(make_fomc_pillars(), ctx,
                                                    make_fomc_raw_rates());

    REQUIRE(resolved.size() == meeting_count + 1);
    for (int i = 1; i <= meeting_count; ++i)
        CHECK(resolved[i].start_date == resolved[i - 1].end_date);
}

TEST_CASE("resolve_bootstrap_pillars keeps FOMC pillar end dates strictly ascending in sequence "
          "order",
          tags) {
    const auto ctx = make_fomc_context();

    const auto resolved = resolve_bootstrap_pillars(make_fomc_pillars(), ctx,
                                                    make_fomc_raw_rates());

    REQUIRE(resolved.size() == meeting_count + 1);
    for (int i = 1; i <= meeting_count; ++i)
        CHECK(resolved[i].end_date > resolved[i - 1].end_date);
}

TEST_CASE("resolve_bootstrap_pillars looks up every FOMC point id in the raw grid", tags) {
    const auto ctx = make_fomc_context();

    const auto resolved = resolve_bootstrap_pillars(make_fomc_pillars(), ctx,
                                                    make_fomc_raw_rates());

    REQUIRE(resolved.size() == meeting_count + 1);
    // point id == end tenor code, and the observed rate comes through.
    for (int n = 1; n <= meeting_count; ++n)
        CHECK(resolved[n - 1].observed_rate == 0.03 + 0.001 * n);
    CHECK(resolved[meeting_count].observed_rate == 0.04); // the 1Y split pillar
}

TEST_CASE("resolve_bootstrap_pillars fails loudly when a FOMC point id has no raw-grid quote",
          tags) {
    const auto ctx = make_fomc_context();
    auto raw_rates = make_fomc_raw_rates();
    raw_rates.erase("8F"); // the last FOMC point id is missing

    CHECK_THROWS_AS(resolve_bootstrap_pillars(make_fomc_pillars(), ctx, raw_rates),
                    std::invalid_argument);
}

TEST_CASE("the FOMC-to-FOMC fixing schedule falls out of the pillar order", tags) {
    const auto ctx = make_fomc_context();

    const auto resolved = resolve_bootstrap_pillars(make_fomc_pillars(), ctx,
                                                    make_fomc_raw_rates());

    REQUIRE(resolved.size() == meeting_count + 1);
    // Each SWAP pillar's fixed-leg dates are every prior pillar's end date
    // plus its own: the meeting dates in order, plus the split date.
    for (int i = 1; i <= meeting_count; ++i) {
        const auto& r = resolved[i];
        REQUIRE(r.fixed_leg_dates.size() == static_cast<std::size_t>(i) + 1);
        for (int j = 0; j <= i; ++j)
            CHECK(r.fixed_leg_dates[j] == resolved[j].end_date);
    }
}
