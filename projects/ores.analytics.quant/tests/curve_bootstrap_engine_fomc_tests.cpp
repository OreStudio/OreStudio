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
// The FOMC segment's two boundary guarantees, exercised through the existing
// engine with the D3-shaped pillar grid (8 FOMC-dated pillars, split at 1Y).
// These pass today: FLAT_FORWARD_THEN_LOG_LINEAR is one configuration of this
// engine, not special-cased code (the "two-segment method" equivalence test
// in curve_bootstrap_engine_tests.cpp already covers the parse/accept side).
//
// Obligations covered here:
//   - @guarantee FlatBetweenMeetings     (constant forward between consecutive
//     FOMC pillar dates)
//   - @guarantee ContinuousBeyondSplit   (no discontinuity at the split tenor;
//     the post-split segment keeps the same continuous method)
//
// The pillar grid mirrors the D3 config: SPOT->1F, 1F->2F, ..., 8F->1Y, with
// the FOMC-to-FOMC fixing schedule (the meeting dates) as the SWAP pillars'
// fixed legs. The 2026 meeting dates are the same data task D4 transcribes
// into calendar_event.
#include "ores.analytics.quant/service/curve_bootstrap_engine.hpp"
#include "ores.analytics.quant/service/curve_instrument_pricer.hpp"
#include "ores.analytics.quant/service/day_count_calculator.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <string>
#include <vector>

using ores::analytics::quant::service::bootstrap_curve_role_code;
using ores::analytics::quant::service::bootstrap_pillar;
using ores::analytics::quant::service::bootstrapped_point;
using ores::analytics::quant::service::curve_bootstrap_engine;
using ores::analytics::quant::service::curve_instrument_pricer;
using ores::analytics::quant::service::day_count_calculator;
using ores::analytics::quant::service::day_count_convention_code;
using ores::analytics::quant::service::interpolation_method_code;
using namespace std::chrono;

namespace {

constexpr auto A365 = day_count_convention_code::A365;
constexpr auto FLAT_FORWARD_THEN_LOG_LINEAR =
    interpolation_method_code::FLAT_FORWARD_THEN_LOG_LINEAR;
constexpr auto DEPOSIT = bootstrap_curve_role_code::DEPOSIT;
constexpr auto SWAP = bootstrap_curve_role_code::SWAP;

constexpr auto meeting_count = 8;
const std::string split_code = "1Y";
const auto value_date = 2026y / January / 2d;

// A non-flat target curve -- r(t) = base + slope * t -- so the guarantees
// exercise interpolation rather than trivially passing on a flat curve.
constexpr double base_rate = 0.02;
constexpr double rate_slope = 0.005;

double true_df(year_month_day d) {
    const double t = day_count_calculator::year_fraction(value_date, d, A365);
    return std::exp(-(base_rate + rate_slope * t) * t);
}

// 2026 FOMC meeting dates -- the second day of each published two-day range
// (statement/decision day), transcribed from federalreserve.gov.
std::vector<year_month_day> fomc_2026_meeting_dates() {
    return {2026y / January / 28d, 2026y / March / 18d, 2026y / April / 29d,
            2026y / June / 17d,    2026y / July / 29d,  2026y / September / 16d,
            2026y / October / 28d, 2026y / December / 9d};
}

// The D3 pillar grid, in date order: the eight meetings plus the 1Y split
// boundary (spot + 1 year).
std::vector<year_month_day> fomc_grid() {
    auto out = fomc_2026_meeting_dates();
    out.push_back(year(int(value_date.year()) + 1) / value_date.month() / value_date.day());
    return out;
}

// SPOT->1F as a deposit ending at the first meeting; 1F->2F, ..., 8F->1Y as
// swaps whose fixed legs are the meeting dates (the FOMC-to-FOMC fixing
// schedule falls out of the pillar order).
std::vector<bootstrap_pillar> make_fomc_pillars() {
    const auto dates = fomc_grid();
    std::vector<bootstrap_pillar> out;

    {
        const double yf = day_count_calculator::year_fraction(value_date, dates[0], A365);
        const double rate = curve_instrument_pricer::deposit_rate(true_df(dates[0]), yf);
        out.push_back({"1F", DEPOSIT, rate, value_date, dates[0], {}});
    }

    for (int i = 1; i <= meeting_count; ++i) {
        std::vector<year_month_day> schedule(dates.begin(), dates.begin() + i + 1);
        std::vector<double> dfs, accruals;
        auto previous = value_date;
        for (const auto& d : schedule) {
            dfs.push_back(true_df(d));
            accruals.push_back(day_count_calculator::year_fraction(previous, d, A365));
            previous = d;
        }
        const double rate =
            curve_instrument_pricer::swap_par_rate(1.0, true_df(dates[i]), dfs, accruals);
        const std::string point_id =
            i < meeting_count ? std::to_string(i + 1) + "F" : split_code;
        out.push_back({point_id, SWAP, rate, value_date, dates[i], schedule});
    }
    return out;
}

// The continuously-compounded forward rate implied by the interpolated
// curve between a and b.
double implied_forward(const std::vector<bootstrapped_point>& points,
                       year_month_day a,
                       year_month_day b) {
    const auto dfa = curve_bootstrap_engine::interpolate_discount_factor(
        points, a, FLAT_FORWARD_THEN_LOG_LINEAR);
    const auto dfb = curve_bootstrap_engine::interpolate_discount_factor(
        points, b, FLAT_FORWARD_THEN_LOG_LINEAR);
    const double yf = day_count_calculator::year_fraction(a, b, A365);
    return std::log(dfa / dfb) / yf;
}

}

TEST_CASE("bootstrap recovers the true discount factors at every FOMC pillar date",
          "[curve_bootstrap_engine][fomc]") {
    const auto result = curve_bootstrap_engine::bootstrap(
        value_date, make_fomc_pillars(), A365, FLAT_FORWARD_THEN_LOG_LINEAR);

    REQUIRE(result.size() == meeting_count + 1);
    for (const auto& point : result)
        CHECK(point.discount_factor == Catch::Approx(true_df(point.date)).epsilon(1e-9));
}

TEST_CASE("the published forward rate between two consecutive FOMC pillars is constant (flat "
          "forward)",
          "[curve_bootstrap_engine][fomc]") {
    const auto dates = fomc_grid();
    const auto result = curve_bootstrap_engine::bootstrap(
        value_date, make_fomc_pillars(), A365, FLAT_FORWARD_THEN_LOG_LINEAR);

    // Three interior query dates within the first meeting interval.
    const auto q1 = year_month_day(sys_days(dates[0]) + days(5));
    const auto q2 = year_month_day(sys_days(dates[0]) + days(15));
    const auto q3 = year_month_day(sys_days(dates[0]) + days(25));

    const auto fwd12 = implied_forward(result, q1, q2);
    const auto fwd23 = implied_forward(result, q2, q3);
    CHECK(fwd12 == Catch::Approx(fwd23).epsilon(1e-9));

    // The interval's own forward: the same constant across the whole segment.
    const auto fwd_segment = implied_forward(result, dates[0], dates[1]);
    CHECK(fwd12 == Catch::Approx(fwd_segment).epsilon(1e-9));
}

TEST_CASE("the discount function is continuous across the split tenor",
          "[curve_bootstrap_engine][fomc]") {
    const auto dates = fomc_grid();
    const auto result = curve_bootstrap_engine::bootstrap(
        value_date, make_fomc_pillars(), A365, FLAT_FORWARD_THEN_LOG_LINEAR);

    // The boundary: the last FOMC pillar date, where the short-end segment
    // hands over to the post-split segment ending at the 1Y split tenor.
    const auto boundary = dates[meeting_count - 1];
    const auto after = year_month_day(sys_days(boundary) + days(7));

    // Just after the boundary, the curve continues from the pillar's own
    // discount factor -- no jump.
    const double yf = day_count_calculator::year_fraction(boundary, after, A365);
    const double fwd = implied_forward(result, boundary, dates[meeting_count]);
    const auto df_boundary = curve_bootstrap_engine::interpolate_discount_factor(
        result, boundary, FLAT_FORWARD_THEN_LOG_LINEAR);
    const auto df_after = curve_bootstrap_engine::interpolate_discount_factor(
        result, after, FLAT_FORWARD_THEN_LOG_LINEAR);
    CHECK(df_after == Catch::Approx(df_boundary * std::exp(-fwd * yf)).epsilon(1e-9));

    // The post-split segment keeps the same continuous method: with no
    // interior pillar, the segment is one log-linear piece, and its
    // constant forward is pinned by the boundary and split-end discount
    // factors. An interior interpolation must reproduce exactly that rate
    // -- not merely be internally self-consistent.
    const auto df_end = curve_bootstrap_engine::interpolate_discount_factor(
        result, dates[meeting_count], FLAT_FORWARD_THEN_LOG_LINEAR);
    const double yf_total =
        day_count_calculator::year_fraction(boundary, dates[meeting_count], A365);
    const double fwd_loglinear = std::log(df_boundary / df_end) / yf_total;
    const auto fwd1 = implied_forward(result, after, year_month_day(sys_days(after) + days(10)));
    CHECK(fwd1 == Catch::Approx(fwd_loglinear).epsilon(1e-9));
}
