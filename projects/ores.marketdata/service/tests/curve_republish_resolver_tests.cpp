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
#include "../src/curve_republish_resolver.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>

namespace {

const std::string tags("[curve_republish_resolver]");

using namespace std::chrono;
using ores::analytics::quant::service::bootstrap_curve_role_code;
using ores::marketdata::service::curve_republish_refdata_context;
using ores::marketdata::service::resolve_bootstrap_pillars;
using ores::marketdata::service::resolve_tenor_date;
using ores::refdata::domain::ir_curve_bootstrap_pillar;
using ores::refdata::domain::tenor;
using ores::refdata::domain::tenor_convention_resolution;

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

ir_curve_bootstrap_pillar
make_pillar(int seq, const std::string& start, const std::string& end, const std::string& role) {
    ir_curve_bootstrap_pillar p;
    p.sequence_index = seq;
    p.start_tenor_code = start;
    p.end_tenor_code = end;
    p.curve_role_code = role;
    return p;
}

// horizon == spot == 2026-01-01 (T+0), same simplification the generation-side resolver uses.
curve_republish_refdata_context make_context() {
    curve_republish_refdata_context ctx;

    ctx.tenors_by_code.emplace("SPOT", make_tenor("SPOT", "DAY", 0));
    ctx.tenors_by_code.emplace("3M", make_tenor("3M", "MONTH", 3));
    ctx.tenors_by_code.emplace("6M", make_tenor("6M", "MONTH", 6));
    ctx.tenors_by_code.emplace("1Y", make_tenor("1Y", "YEAR", 1));
    ctx.tenors_by_code.emplace("2Y", make_tenor("2Y", "YEAR", 2));

    ctx.convention.code = "RATES_SPOT_FORWARD";
    ctx.convention.measured_from = "SPOT";
    ctx.convention.resolution_algorithm = "ANCHOR_OFFSET";

    for (const auto& code : {"SPOT", "3M", "6M", "1Y", "2Y"})
        ctx.resolutions_by_tenor.emplace(code, make_resolution(code));

    ctx.horizon = 2026y / January / 1d;

    return ctx;
}

}

TEST_CASE("resolve_bootstrap_pillars resolves a single DEPOSIT pillar's dates and rate", tags) {
    const auto ctx = make_context();
    std::vector<ir_curve_bootstrap_pillar> pillars{make_pillar(0, "SPOT", "3M", "DEPOSIT")};
    std::map<std::string, double> raw_rates{{"3M", 0.03}};

    const auto resolved = resolve_bootstrap_pillars(pillars, ctx, raw_rates);

    REQUIRE(resolved.size() == 1);
    CHECK(resolved[0].point_id == "3M");
    CHECK(resolved[0].curve_role == bootstrap_curve_role_code::DEPOSIT);
    CHECK(resolved[0].observed_rate == 0.03);
    CHECK(resolved[0].start_date == ctx.horizon);
    CHECK(resolved[0].end_date == resolve_tenor_date(ctx, "3M"));
    CHECK(resolved[0].fixed_leg_dates.empty());
}

TEST_CASE("resolve_bootstrap_pillars resolves an FRA pillar's start/end from its own tenor codes",
          tags) {
    const auto ctx = make_context();
    std::vector<ir_curve_bootstrap_pillar> pillars{make_pillar(0, "3M", "6M", "FRA")};
    std::map<std::string, double> raw_rates{{"6M", 0.032}};

    const auto resolved = resolve_bootstrap_pillars(pillars, ctx, raw_rates);

    REQUIRE(resolved.size() == 1);
    CHECK(resolved[0].curve_role == bootstrap_curve_role_code::FRA);
    CHECK(resolved[0].start_date == resolve_tenor_date(ctx, "3M"));
    CHECK(resolved[0].end_date == resolve_tenor_date(ctx, "6M"));
}

TEST_CASE("resolve_bootstrap_pillars sorts by sequence_index regardless of input order", tags) {
    const auto ctx = make_context();
    std::vector<ir_curve_bootstrap_pillar> pillars{
        make_pillar(1, "3M", "6M", "FRA"),
        make_pillar(0, "SPOT", "3M", "DEPOSIT"),
    };
    std::map<std::string, double> raw_rates{{"3M", 0.03}, {"6M", 0.032}};

    const auto resolved = resolve_bootstrap_pillars(pillars, ctx, raw_rates);

    REQUIRE(resolved.size() == 2);
    CHECK(resolved[0].point_id == "3M");
    CHECK(resolved[1].point_id == "6M");
}

TEST_CASE("resolve_bootstrap_pillars builds a SWAP pillar's fixed_leg_dates from every prior "
          "pillar's own end_date plus its own",
          tags) {
    const auto ctx = make_context();
    std::vector<ir_curve_bootstrap_pillar> pillars{
        make_pillar(0, "SPOT", "1Y", "DEPOSIT"),
        make_pillar(1, "SPOT", "2Y", "SWAP"),
    };
    std::map<std::string, double> raw_rates{{"1Y", 0.03}, {"2Y", 0.035}};

    const auto resolved = resolve_bootstrap_pillars(pillars, ctx, raw_rates);

    REQUIRE(resolved.size() == 2);
    const auto& swap = resolved[1];
    REQUIRE(swap.fixed_leg_dates.size() == 2);
    CHECK(swap.fixed_leg_dates[0] == resolve_tenor_date(ctx, "1Y"));
    CHECK(swap.fixed_leg_dates[1] == resolve_tenor_date(ctx, "2Y"));
    CHECK(swap.fixed_leg_dates.back() == swap.end_date);
}

TEST_CASE("resolve_bootstrap_pillars rejects a pillar with no observed rate in the raw grid",
          tags) {
    const auto ctx = make_context();
    std::vector<ir_curve_bootstrap_pillar> pillars{make_pillar(0, "SPOT", "3M", "DEPOSIT")};
    std::map<std::string, double> raw_rates; // empty -- 3M missing

    CHECK_THROWS_AS(resolve_bootstrap_pillars(pillars, ctx, raw_rates), std::invalid_argument);
}

TEST_CASE("resolve_bootstrap_pillars rejects an unknown tenor code", tags) {
    const auto ctx = make_context();
    std::vector<ir_curve_bootstrap_pillar> pillars{make_pillar(0, "SPOT", "9M", "DEPOSIT")};
    std::map<std::string, double> raw_rates{{"9M", 0.03}};

    CHECK_THROWS_AS(resolve_bootstrap_pillars(pillars, ctx, raw_rates), std::invalid_argument);
}

TEST_CASE("resolve_bootstrap_pillars rejects an unrecognised curve_role_code", tags) {
    const auto ctx = make_context();
    std::vector<ir_curve_bootstrap_pillar> pillars{make_pillar(0, "SPOT", "3M", "BOND")};
    std::map<std::string, double> raw_rates{{"3M", 0.03}};

    CHECK_THROWS_AS(resolve_bootstrap_pillars(pillars, ctx, raw_rates), std::invalid_argument);
}
