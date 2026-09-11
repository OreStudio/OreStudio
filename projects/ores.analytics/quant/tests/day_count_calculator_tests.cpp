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
#include "ores.analytics.quant/service/day_count_calculator.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>

using ores::analytics::quant::service::day_count_calculator;
using ores::analytics::quant::service::day_count_convention_code;
using ores::analytics::quant::service::parse_day_count_convention_code;
using namespace std::chrono;

TEST_CASE("A360 divides actual days by 360", "[day_count_calculator]") {
    const auto start = 2026y / January / 1d;
    const auto end = 2026y / April / 1d; // 90 actual days
    CHECK(day_count_calculator::year_fraction(start, end, day_count_convention_code::A360) ==
          Catch::Approx(90.0 / 360.0));
}

TEST_CASE("A365 divides actual days by 365", "[day_count_calculator]") {
    const auto start = 2026y / January / 1d;
    const auto end = 2027y / January / 1d; // 365 actual days (2026 not a leap year)
    CHECK(day_count_calculator::year_fraction(start, end, day_count_convention_code::A365) ==
          Catch::Approx(1.0));
}

TEST_CASE("A360 and A365 differ on the same interval by the expected ratio",
          "[day_count_calculator]") {
    const auto start = 2026y / January / 1d;
    const auto end = 2026y / April / 1d; // 90 actual days
    const double a360 =
        day_count_calculator::year_fraction(start, end, day_count_convention_code::A360);
    const double a365 =
        day_count_calculator::year_fraction(start, end, day_count_convention_code::A365);
    CHECK(a360 / a365 == Catch::Approx(365.0 / 360.0));
}

TEST_CASE("A365F behaves identically to A365", "[day_count_calculator]") {
    const auto start = 2026y / March / 15d;
    const auto end = 2026y / June / 15d;
    CHECK(day_count_calculator::year_fraction(start, end, day_count_convention_code::A365F) ==
          Catch::Approx(
              day_count_calculator::year_fraction(start, end, day_count_convention_code::A365)));
}

TEST_CASE("30/360 treats a full calendar year as exactly 1.0", "[day_count_calculator]") {
    const auto start = 2026y / January / 1d;
    const auto end = 2027y / January / 1d;
    CHECK(day_count_calculator::year_fraction(start, end, day_count_convention_code::THIRTY_360) ==
          Catch::Approx(1.0));
}

TEST_CASE("30/360 treats every month as exactly 30 days", "[day_count_calculator]") {
    const auto start = 2026y / January / 1d;
    const auto end = 2026y / February / 1d;
    CHECK(day_count_calculator::year_fraction(start, end, day_count_convention_code::THIRTY_360) ==
          Catch::Approx(30.0 / 360.0));
}

TEST_CASE("30/360 applies the end-of-month 31-to-30 adjustment", "[day_count_calculator]") {
    const auto start = 2026y / January / 31d;
    const auto end = 2026y / March / 31d;
    // 31 -> 30 for both ends under the simplified bond-basis rule: exactly two 30-day months.
    CHECK(day_count_calculator::year_fraction(start, end, day_count_convention_code::THIRTY_360) ==
          Catch::Approx(60.0 / 360.0));
}

TEST_CASE("30/360 does not adjust a start day of 31 when the end day is not 31",
          "[day_count_calculator]") {
    const auto start = 2026y / January / 31d; // adjusted down to 30
    const auto end = 2026y / February / 15d;  // not 31, left alone
    // (Feb - Jan)*30 + (15 - 30) = 30 - 15 = 15 days.
    CHECK(day_count_calculator::year_fraction(start, end, day_count_convention_code::THIRTY_360) ==
          Catch::Approx(15.0 / 360.0));
}

TEST_CASE("year_fraction of a date with itself is zero", "[day_count_calculator]") {
    const auto d = 2026y / January / 1d;
    CHECK(day_count_calculator::year_fraction(d, d, day_count_convention_code::A360) ==
          Catch::Approx(0.0));
}

TEST_CASE("year_fraction rejects end before start", "[day_count_calculator]") {
    const auto start = 2026y / June / 1d;
    const auto end = 2026y / January / 1d;
    CHECK_THROWS_AS(
        day_count_calculator::year_fraction(start, end, day_count_convention_code::A360),
        std::invalid_argument);
}

TEST_CASE("year_fraction rejects an invalid start or end date", "[day_count_calculator]") {
    const auto valid = 2026y / January / 1d;
    const auto invalid = 2026y / February / 30d; // February has no 30th
    CHECK_FALSE(invalid.ok());
    CHECK_THROWS_AS(
        day_count_calculator::year_fraction(invalid, valid, day_count_convention_code::A360),
        std::invalid_argument);
    CHECK_THROWS_AS(
        day_count_calculator::year_fraction(valid, invalid, day_count_convention_code::A360),
        std::invalid_argument);
}

TEST_CASE("parse_day_count_convention_code accepts every implemented convention",
          "[day_count_calculator]") {
    CHECK(parse_day_count_convention_code("A360") == day_count_convention_code::A360);
    CHECK(parse_day_count_convention_code("A365") == day_count_convention_code::A365);
    CHECK(parse_day_count_convention_code("A365F") == day_count_convention_code::A365F);
    CHECK(parse_day_count_convention_code("30/360") == day_count_convention_code::THIRTY_360);
}

TEST_CASE("parse_day_count_convention_code rejects an unsupported convention",
          "[day_count_calculator]") {
    CHECK_THROWS_AS(parse_day_count_convention_code("ActAct(ISDA)"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("ActAct(ISMA)"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("ActAct(AFB)"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("30E/360"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("30E/360(ISDA)"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("Business252"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("bogus"), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code(""), std::invalid_argument);
    CHECK_THROWS_AS(parse_day_count_convention_code("a360"), std::invalid_argument);
}
