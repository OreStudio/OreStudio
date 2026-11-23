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
#include "ores.platform/time/datetime.hpp"
#include "ores.platform/time/time_of_day.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <sstream>
#include <stdexcept>

namespace {

using ores::platform::time::datetime;
using ores::platform::time::time_of_day;
using namespace std::chrono_literals;

}

TEST_CASE("a time of day is midnight by default", "[time][time_of_day]") {
    CHECK(time_of_day{}.since_midnight() == 0s);
}

TEST_CASE("a time of day is the seconds since midnight", "[time][time_of_day]") {
    const time_of_day four_pm(16h, 0min);
    CHECK(four_pm.since_midnight() == 57'600s);
    CHECK(four_pm.hms().hours() == 16h);
    CHECK(time_of_day(23h, 59min, 59s).since_midnight() == 86'399s);
}

TEST_CASE("a time of day refuses a value outside the day", "[time][time_of_day]") {
    CHECK_THROWS_AS(time_of_day(-1s), std::out_of_range);
    CHECK_THROWS_AS(time_of_day(24h, 0min), std::out_of_range);
    CHECK_THROWS_AS(time_of_day(86'400s), std::out_of_range);
}

TEST_CASE("times of day compare by their place in the day", "[time][time_of_day]") {
    CHECK(time_of_day(10h, 0min) < time_of_day(15h, 0min));
    CHECK(time_of_day(12h, 30min) == time_of_day(45'000s));
}

TEST_CASE("to_iso8601_time_formats_hours_minutes_seconds", "[time][datetime]") {
    CHECK(datetime::to_iso8601_time(time_of_day(9h, 5min, 7s)) == "09:05:07");
    CHECK(datetime::to_iso8601_time(time_of_day{}) == "00:00:00");
}

TEST_CASE("from_iso8601_time_parses_both_extended_forms", "[time][datetime]") {
    CHECK(datetime::from_iso8601_time("16:00:00") == time_of_day(16h, 0min));
    CHECK(datetime::from_iso8601_time("12:30") == time_of_day(12h, 30min));
}

TEST_CASE("from_iso8601_time_to_iso8601_time_round_trip", "[time][datetime]") {
    const time_of_day original(14h, 15min, 30s);
    CHECK(datetime::from_iso8601_time(datetime::to_iso8601_time(original)) == original);
}

TEST_CASE("from_iso8601_time_refuses_text_that_is_not_a_time_of_day", "[time][datetime]") {
    CHECK_THROWS_AS(datetime::from_iso8601_time("24:00"), std::invalid_argument);
    CHECK_THROWS_AS(datetime::from_iso8601_time("12:60"), std::invalid_argument);
    CHECK_THROWS_AS(datetime::from_iso8601_time("12:00:00.5"), std::invalid_argument);
    CHECK_THROWS_AS(datetime::from_iso8601_time("1200"), std::invalid_argument);
    CHECK_THROWS_AS(datetime::from_iso8601_time("ab:cd"), std::invalid_argument);
    CHECK_THROWS_AS(datetime::from_iso8601_time(""), std::invalid_argument);
}

TEST_CASE("a_time_of_day_streams_as_hh_mm_ss", "[time][time_of_day]") {
    using namespace std::chrono_literals;
    std::ostringstream s;
    s << ores::platform::time::time_of_day(9h, 5min, 7s);
    CHECK(s.str() == "09:05:07");
}
