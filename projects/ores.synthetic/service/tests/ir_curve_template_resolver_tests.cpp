/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
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
#include "../src/ir_curve_template_resolver.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>

namespace {

const std::string tags("[ir_curve_template_resolver]");

using ores::refdata::domain::instrument_code;
using ores::refdata::domain::payment_frequency;
using ores::refdata::domain::tenor;
using ores::refdata::domain::tenor_convention;
using ores::refdata::domain::tenor_convention_resolution;
using ores::synthetic::domain::ir_curve_template_entry;
using ores::synthetic::service::ir_curve_refdata_context;
using ores::synthetic::service::resolve;

tenor make_tenor(const std::string& code, const std::string& unit, int multiplier) {
    tenor t;
    t.code = code;
    t.kind = "PERIOD";
    t.unit = unit;
    t.multiplier = multiplier;
    return t;
}

tenor_convention_resolution make_resolution(const std::string& tenor_code) {
    tenor_convention_resolution r;
    r.convention_code = "RATES_SPOT_FORWARD";
    r.tenor_code = tenor_code;
    return r;
}

instrument_code make_instrument_code(const std::string& code, const std::string& curve_role) {
    instrument_code ic;
    ic.code = code;
    ic.curve_role = curve_role;
    return ic;
}

payment_frequency make_quarterly() {
    payment_frequency pf;
    pf.code = "Quarterly";
    pf.period_unit = "MONTH";
    pf.period_multiplier = 3;
    return pf;
}

ir_curve_template_entry
make_entry(int seq, const std::string& start, const std::string& end, const std::string& instr) {
    ir_curve_template_entry e;
    e.sequence_index = seq;
    e.start_tenor_code = start;
    e.end_tenor_code = end;
    e.instrument_code = instr;
    return e;
}

// SPOT (zero-duration), 3M, 6M, 2Y catalog + RATES_SPOT_FORWARD convention/resolutions,
// horizon == spot == 2026-01-01 (T+0, per the resolver's own documented simplification).
ir_curve_refdata_context make_context() {
    ir_curve_refdata_context ctx;

    ctx.tenors_by_code.emplace("SPOT", make_tenor("SPOT", "DAY", 0));
    ctx.tenors_by_code.emplace("3M", make_tenor("3M", "MONTH", 3));
    ctx.tenors_by_code.emplace("6M", make_tenor("6M", "MONTH", 6));
    ctx.tenors_by_code.emplace("2Y", make_tenor("2Y", "YEAR", 2));

    ctx.convention.code = "RATES_SPOT_FORWARD";
    ctx.convention.measured_from = "SPOT";
    ctx.convention.resolution_algorithm = "ANCHOR_OFFSET";

    for (const auto& code : {"SPOT", "3M", "6M", "2Y"})
        ctx.resolutions_by_tenor.emplace(code, make_resolution(code));

    ctx.instrument_codes_by_code.emplace("DEPO", make_instrument_code("DEPO", "DEPOSIT"));
    ctx.instrument_codes_by_code.emplace("FRA", make_instrument_code("FRA", "FRA"));
    ctx.instrument_codes_by_code.emplace("SWAP", make_instrument_code("SWAP", "SWAP"));

    ctx.payment_frequencies_by_code.emplace("Quarterly", make_quarterly());

    ctx.horizon = std::chrono::year{2026} / std::chrono::January / 1;
    ctx.spot = ctx.horizon;

    return ctx;
}

}

TEST_CASE("resolve derives ticks and year_fraction for a Deposit (point) entry", tags) {
    const auto ctx = make_context();
    const auto out = resolve({make_entry(0, "SPOT", "3M", "DEPO")}, ctx, "Quarterly");

    REQUIRE(out.size() == 1);
    CHECK(out[0].curve_role == "DEPOSIT");
    CHECK(out[0].point_id == "3M");
    CHECK(out[0].ticks_ahead_start == 0);
    CHECK(out[0].ticks_ahead_end == 90); // 2026-01-01 -> 2026-04-01
    CHECK(out[0].year_fraction == Catch::Approx(90.0 / 365.0));
    CHECK(out[0].fixed_leg_schedule.empty());
}

TEST_CASE("resolve derives a genuine [start, end) window for an FRA (interval) entry", tags) {
    const auto ctx = make_context();
    const auto out = resolve({make_entry(0, "3M", "6M", "FRA")}, ctx, "Quarterly");

    REQUIRE(out.size() == 1);
    CHECK(out[0].curve_role == "FRA");
    CHECK(out[0].ticks_ahead_start == 90); // 2026-01-01 -> 2026-04-01
    CHECK(out[0].ticks_ahead_end == 181);  // 2026-01-01 -> 2026-07-01
    CHECK(out[0].year_fraction == Catch::Approx(91.0 / 365.0)); // 2026-04-01 -> 2026-07-01
    CHECK(out[0].fixed_leg_schedule.empty());
}

TEST_CASE("resolve builds a quarterly fixed-leg schedule for a 2Y Swap entry", tags) {
    const auto ctx = make_context();
    const auto out = resolve({make_entry(0, "SPOT", "2Y", "SWAP")}, ctx, "Quarterly");

    REQUIRE(out.size() == 1);
    CHECK(out[0].curve_role == "SWAP");
    CHECK(out[0].ticks_ahead_start == 0);
    REQUIRE(out[0].fixed_leg_schedule.size() == 8); // 2Y / 3M

    // The schedule steps forward monotonically and lands exactly on the entry's own maturity.
    std::size_t previous = 0;
    double total_accrual = 0.0;
    for (const auto& step : out[0].fixed_leg_schedule) {
        CHECK(step.ticks_ahead > previous);
        previous = step.ticks_ahead;
        total_accrual += step.accrual_fraction;
    }
    CHECK(out[0].fixed_leg_schedule.back().ticks_ahead == out[0].ticks_ahead_end);
    CHECK(total_accrual == Catch::Approx(out[0].year_fraction));
}

TEST_CASE("resolve sorts output by sequence_index regardless of input order", tags) {
    const auto ctx = make_context();
    const auto out = resolve(
        {make_entry(2, "SPOT", "2Y", "SWAP"),
         make_entry(0, "SPOT", "3M", "DEPO"),
         make_entry(1, "3M", "6M", "FRA")},
        ctx,
        "Quarterly");

    REQUIRE(out.size() == 3);
    CHECK(out[0].sequence_index == 0);
    CHECK(out[1].sequence_index == 1);
    CHECK(out[2].sequence_index == 2);
}

TEST_CASE("resolve throws on an unknown tenor code", tags) {
    const auto ctx = make_context();
    CHECK_THROWS_AS(resolve({make_entry(0, "SPOT", "9M", "DEPO")}, ctx, "Quarterly"),
                    std::invalid_argument);
}

TEST_CASE("resolve throws on an unknown instrument code", tags) {
    const auto ctx = make_context();
    CHECK_THROWS_AS(resolve({make_entry(0, "SPOT", "3M", "NOPE")}, ctx, "Quarterly"),
                    std::invalid_argument);
}

TEST_CASE("resolve throws on an unknown fixed-leg payment frequency for a Swap entry", tags) {
    const auto ctx = make_context();
    CHECK_THROWS_AS(resolve({make_entry(0, "SPOT", "2Y", "SWAP")}, ctx, "Monthly"),
                    std::invalid_argument);
}
