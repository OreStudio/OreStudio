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
#include "ores.analytics.quant/service/forward_rate_calculator.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <stdexcept>

using ores::analytics::quant::service::bootstrapped_point;
using ores::analytics::quant::service::day_count_convention_code;
using ores::analytics::quant::service::forward_rate_calculator;
using namespace std::chrono;

namespace {

constexpr auto A365 = day_count_convention_code::A365;

bootstrapped_point make_point(const std::string& id, year_month_day date, double df) {
    return {id, date, df};
}

}

const std::string tags("[forward_rate_calculator]");

TEST_CASE("calculate returns an empty result for fewer than two points", tags) {
    CHECK(forward_rate_calculator::calculate({}, A365).empty());
    CHECK(forward_rate_calculator::calculate({make_point("3M", 2026y / April / 1d, 0.99)}, A365)
              .empty());
}

TEST_CASE("calculate derives a constant forward rate along a flat continuously-compounded curve",
          tags) {
    // df(t) = exp(-r*t), r = 0.03 (Act/365): every consecutive pair must recover r exactly,
    // since the instantaneous forward rate of a flat continuously-compounded curve is the flat
    // rate itself, independent of the interval chosen.
    const auto value_date = 2026y / January / 1d;
    constexpr double r = 0.03;
    auto df_at = [&](int days) {
        return std::exp(-r * days / 365.0);
    };

    std::vector<bootstrapped_point> points{
        make_point("", value_date, 1.0),
        make_point("3M", value_date + months{3}, df_at(90)),
        make_point("1Y", value_date + years{1}, df_at(365)),
        make_point("2Y", value_date + years{2}, df_at(730)),
    };

    const auto forwards = forward_rate_calculator::calculate(points, A365);

    REQUIRE(forwards.size() == 3);
    for (const auto& f : forwards)
        CHECK(f.instantaneous_forward_rate == Catch::Approx(r).epsilon(1e-6));
}

TEST_CASE("calculate recovers a hand-computed forward rate for a known non-flat pair", tags) {
    // f = ln(df_start/df_end) / yf, Act/365 over exactly one year (365 days -> yf == 1.0).
    const auto start = 2026y / January / 1d;
    const auto end = 2027y / January / 1d;
    std::vector<bootstrapped_point> points{
        make_point("start", start, 0.98),
        make_point("1Y", end, 0.95),
    };

    const auto forwards = forward_rate_calculator::calculate(points, A365);

    REQUIRE(forwards.size() == 1);
    const double expected = std::log(0.98 / 0.95) / (365.0 / 365.0);
    CHECK(forwards[0].instantaneous_forward_rate == Catch::Approx(expected));
    CHECK(forwards[0].point_id == "1Y");
    CHECK(forwards[0].start_date == start);
    CHECK(forwards[0].end_date == end);
}

TEST_CASE("calculate is sensitive to the day-count convention", tags) {
    const auto start = 2026y / January / 1d;
    const auto end = 2026y / July / 1d; // 181 days
    std::vector<bootstrapped_point> points{
        make_point("start", start, 0.99),
        make_point("6M", end, 0.97),
    };

    const auto a365 = forward_rate_calculator::calculate(points, day_count_convention_code::A365);
    const auto a360 = forward_rate_calculator::calculate(points, day_count_convention_code::A360);

    REQUIRE(a365.size() == 1);
    REQUIRE(a360.size() == 1);
    CHECK(a365[0].instantaneous_forward_rate != Catch::Approx(a360[0].instantaneous_forward_rate));
}

TEST_CASE("calculate rejects a pair that does not strictly increase in date", tags) {
    const auto d = 2026y / January / 1d;
    std::vector<bootstrapped_point> points{
        make_point("a", d, 0.99),
        make_point("b", d, 0.98), // same date -- zero-length interval
    };

    CHECK_THROWS_AS(forward_rate_calculator::calculate(points, A365), std::invalid_argument);
}

TEST_CASE("calculate rejects a non-positive discount factor", tags) {
    const auto d = 2026y / January / 1d;
    std::vector<bootstrapped_point> points{
        make_point("a", d, 1.0),
        make_point("b", d + months{3}, 0.0),
    };

    CHECK_THROWS_AS(forward_rate_calculator::calculate(points, A365), std::invalid_argument);
}

TEST_CASE("calculate handles more than two points, one forward rate per consecutive pair", tags) {
    const auto value_date = 2026y / January / 1d;
    std::vector<bootstrapped_point> points{
        make_point("", value_date, 1.0),
        make_point("3M", value_date + months{3}, 0.99),
        make_point("6M", value_date + months{6}, 0.98),
        make_point("1Y", value_date + years{1}, 0.95),
    };

    const auto forwards = forward_rate_calculator::calculate(points, A365);

    REQUIRE(forwards.size() == 3);
    CHECK(forwards[0].point_id == "3M");
    CHECK(forwards[1].point_id == "6M");
    CHECK(forwards[2].point_id == "1Y");
}
