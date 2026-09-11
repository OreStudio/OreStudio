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
#include "ores.analytics.quant/service/curve_health_checker.hpp"
#include <catch2/catch_test_macros.hpp>

using ores::analytics::quant::service::bootstrapped_point;
using ores::analytics::quant::service::curve_health_checker;
using ores::analytics::quant::service::curve_health_severity;
using ores::analytics::quant::service::forward_rate_point;
using namespace std::chrono;

namespace {

const std::string tags("[curve_health_checker]");

forward_rate_point make_forward(const std::string& id, double rate) {
    forward_rate_point f;
    f.point_id = id;
    f.instantaneous_forward_rate = rate;
    return f;
}

}

TEST_CASE("check_discount_factors finds nothing for an all-positive curve", tags) {
    std::vector<bootstrapped_point> points{
        {"3M", 2026y / April / 1d, 0.99},
        {"1Y", 2027y / January / 1d, 0.95},
        // > 1.0 is legitimate under a negative-rate regime -- must not be flagged.
        {"2Y", 2028y / January / 1d, 1.02},
    };

    CHECK(curve_health_checker::check_discount_factors(points).empty());
}

TEST_CASE("check_discount_factors flags a zero or negative discount factor", tags) {
    std::vector<bootstrapped_point> points{
        {"3M", 2026y / April / 1d, 0.99},
        {"1Y", 2027y / January / 1d, 0.0},
        {"2Y", 2028y / January / 1d, -0.01},
    };

    const auto findings = curve_health_checker::check_discount_factors(points);

    REQUIRE(findings.size() == 2);
    CHECK(findings[0].point_id == "1Y");
    CHECK(findings[0].severity == curve_health_severity::WARNING);
    CHECK(findings[1].point_id == "2Y");
}

TEST_CASE("check_forward_rates finds nothing for a flat, all-positive series", tags) {
    std::vector<forward_rate_point> forwards{
        make_forward("3M", 0.03), make_forward("6M", 0.03), make_forward("1Y", 0.03)};

    CHECK(curve_health_checker::check_forward_rates(forwards).empty());
}

TEST_CASE("check_forward_rates flags a negative forward rate as INFO, not WARNING", tags) {
    std::vector<forward_rate_point> forwards{
        make_forward("3M", 0.03), make_forward("6M", -0.01), make_forward("1Y", 0.03)};

    const auto findings = curve_health_checker::check_forward_rates(forwards);

    REQUIRE(findings.size() == 1);
    CHECK(findings[0].point_id == "6M");
    CHECK(findings[0].severity == curve_health_severity::INFO);
}

TEST_CASE("check_forward_rates flags a statistical outlier as WARNING", tags) {
    std::vector<forward_rate_point> forwards{make_forward("3M", 0.030),
                                             make_forward("6M", 0.031),
                                             make_forward("1Y", 0.029),
                                             make_forward("2Y", 0.030),
                                             // A spike wildly out of line with its neighbours.
                                             make_forward("3Y", 0.45)};

    const auto findings = curve_health_checker::check_forward_rates(forwards);

    REQUIRE(findings.size() == 1);
    CHECK(findings[0].point_id == "3Y");
    CHECK(findings[0].severity == curve_health_severity::WARNING);
}

TEST_CASE("check_forward_rates skips outlier detection for fewer than three points", tags) {
    std::vector<forward_rate_point> forwards{make_forward("3M", 0.03), make_forward("6M", 5.0)};

    // No outlier findings with only two points -- too small a sample to have a meaningful
    // notion of "outlier"; 5.0 is not negative, so no INFO finding either.
    CHECK(curve_health_checker::check_forward_rates(forwards).empty());
}

TEST_CASE("check_forward_rates does not divide by zero when every rate is identical", tags) {
    std::vector<forward_rate_point> forwards{
        make_forward("3M", 0.03), make_forward("6M", 0.03), make_forward("1Y", 0.03)};

    // stdev == 0 here; must not produce NaN/inf findings from a 0/0 division.
    CHECK(curve_health_checker::check_forward_rates(forwards).empty());
}

TEST_CASE("check_forward_rates respects a custom outlier_z_threshold", tags) {
    std::vector<forward_rate_point> forwards{make_forward("3M", 0.030),
                                             make_forward("6M", 0.031),
                                             make_forward("1Y", 0.029),
                                             make_forward("2Y", 0.032)};

    // A tight threshold flags points a looser one wouldn't.
    const auto loose = curve_health_checker::check_forward_rates(forwards, 10.0);
    const auto tight = curve_health_checker::check_forward_rates(forwards, 0.1);

    CHECK(loose.empty());
    CHECK_FALSE(tight.empty());
}
