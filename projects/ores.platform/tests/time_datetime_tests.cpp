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
#include "ores.platform/time/time_utils.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

const std::string tags("[time][datetime]");

using ores::platform::time::datetime;
using ores::platform::time::time_utils;

/**
 * @brief Builds a UTC time_point from explicit calendar fields.
 *
 * Uses time_utils::to_time_point_utc (timegm) so the result is always the
 * correct UTC instant regardless of the process timezone.
 */
std::chrono::system_clock::time_point
make_utc(int year, int mon, int mday, int hour, int min, int sec) {
    std::tm tm = {};
    tm.tm_year = year - 1900;
    tm.tm_mon = mon - 1;
    tm.tm_mday = mday;
    tm.tm_hour = hour;
    tm.tm_min = min;
    tm.tm_sec = sec;
    return time_utils::to_time_point_utc(tm);
}

}

TEST_CASE("to_iso8601_utc_known_fixed_point", tags) {
    const auto tp = make_utc(2026, 1, 15, 14, 30, 0);
    CHECK(datetime::to_iso8601_utc(tp) == "2026-01-15 14:30:00Z");
}

TEST_CASE("to_iso8601_utc_dst_edge_point", tags) {
    const auto tp = make_utc(2026, 3, 29, 1, 0, 0);
    CHECK(datetime::to_iso8601_utc(tp) == "2026-03-29 01:00:00Z");
}

TEST_CASE("to_iso8601_utc_midnight", tags) {
    const auto tp = make_utc(2026, 12, 31, 0, 0, 0);
    CHECK(datetime::to_iso8601_utc(tp) == "2026-12-31 00:00:00Z");
}

TEST_CASE("from_iso8601_utc_accepts_Z_suffix", tags) {
    const auto expected = make_utc(2026, 4, 8, 10, 30, 0);
    CHECK(datetime::from_iso8601_utc("2026-04-08 10:30:00Z") == expected);
}

TEST_CASE("from_iso8601_utc_accepts_T_separator", tags) {
    const auto expected = make_utc(2026, 4, 8, 10, 30, 0);
    CHECK(datetime::from_iso8601_utc("2026-04-08T10:30:00Z") == expected);
}

TEST_CASE("from_iso8601_utc_accepts_plus_00_00_suffix", tags) {
    const auto expected = make_utc(2026, 4, 8, 10, 30, 0);
    CHECK(datetime::from_iso8601_utc("2026-04-08 10:30:00+00:00") == expected);
}

TEST_CASE("from_iso8601_utc_accepts_plus_00_suffix", tags) {
    const auto expected = make_utc(2026, 4, 8, 10, 30, 0);
    CHECK(datetime::from_iso8601_utc("2026-04-08 10:30:00+00") == expected);
}

TEST_CASE("from_iso8601_utc_accepts_space_before_offset", tags) {
    const auto expected = make_utc(2026, 4, 8, 10, 30, 0);
    CHECK(datetime::from_iso8601_utc("2026-04-08 10:30:00 +00") == expected);
}

TEST_CASE("to_iso8601_utc_from_iso8601_utc_round_trip", tags) {
    const auto original = make_utc(2026, 4, 8, 10, 30, 45);
    const auto serialised = datetime::to_iso8601_utc(original);
    CHECK(serialised == "2026-04-08 10:30:45Z");
    CHECK(datetime::from_iso8601_utc(serialised) == original);
}

TEST_CASE("from_iso8601_utc_throws_on_missing_designator", tags) {
    CHECK_THROWS_AS(datetime::from_iso8601_utc("2026-04-08 10:30:00"), std::invalid_argument);
}

TEST_CASE("from_iso8601_utc_throws_on_non_utc_positive_offset", tags) {
    CHECK_THROWS_AS(datetime::from_iso8601_utc("2026-04-08 11:30:00+01"), std::invalid_argument);
}

TEST_CASE("from_iso8601_utc_throws_on_non_utc_negative_offset", tags) {
    CHECK_THROWS_AS(datetime::from_iso8601_utc("2026-04-08 05:30:00-05:00"), std::invalid_argument);
}

TEST_CASE("from_iso8601_utc_throws_on_empty_string", tags) {
    CHECK_THROWS_AS(datetime::from_iso8601_utc(""), std::invalid_argument);
}

TEST_CASE("from_iso8601_utc_throws_on_malformed_string", tags) {
    CHECK_THROWS_AS(datetime::from_iso8601_utc("not-a-dateZ"), std::invalid_argument);
}

TEST_CASE("to_db_string_known_fixed_point", tags) {
    const auto tp = make_utc(2026, 1, 15, 14, 30, 0);
    CHECK(datetime::to_db_string(tp) == "2026-01-15 14:30:00");
}

TEST_CASE("to_db_string_has_no_timezone_suffix", tags) {
    const auto tp = make_utc(2026, 4, 8, 10, 30, 45);
    const auto s = datetime::to_db_string(tp);
    CHECK(s == "2026-04-08 10:30:45");
    CHECK(s.size() == 19);
}

TEST_CASE("to_db_string_is_utc_regardless_of_host_timezone", tags) {
    const auto tp = make_utc(2026, 3, 29, 1, 0, 0);
    CHECK(datetime::to_db_string(tp) == "2026-03-29 01:00:00");
}

TEST_CASE("to_iso8601_date_formats_calendar_date", tags) {
    const std::chrono::year_month_day date{
        std::chrono::year{2026}, std::chrono::month{4}, std::chrono::day{8}};
    CHECK(datetime::to_iso8601_date(date) == "2026-04-08");
}

TEST_CASE("to_iso8601_date_pads_single_digit_fields", tags) {
    const std::chrono::year_month_day date{
        std::chrono::year{2026}, std::chrono::month{1}, std::chrono::day{5}};
    CHECK(datetime::to_iso8601_date(date) == "2026-01-05");
}

TEST_CASE("from_iso8601_date_parses_calendar_date", tags) {
    const std::chrono::year_month_day expected{
        std::chrono::year{2026}, std::chrono::month{4}, std::chrono::day{8}};
    CHECK(datetime::from_iso8601_date("2026-04-08") == expected);
}

TEST_CASE("from_iso8601_date_to_iso8601_date_round_trip", tags) {
    const std::chrono::year_month_day original{
        std::chrono::year{2026}, std::chrono::month{2}, std::chrono::day{28}};
    CHECK(datetime::from_iso8601_date(datetime::to_iso8601_date(original)) == original);
}

TEST_CASE("from_iso8601_date_rejects_impossible_calendar_date", tags) {
    CHECK_THROWS_AS(datetime::from_iso8601_date("2024-02-30"), std::invalid_argument);
}

TEST_CASE("from_iso8601_date_rejects_non_date_string", tags) {
    CHECK_THROWS_AS(datetime::from_iso8601_date("not-a-date"), std::invalid_argument);
}

TEST_CASE("to_local_display_string_uses_given_format", tags) {
    const auto tp = make_utc(2026, 1, 15, 14, 30, 0);
    CHECK(datetime::to_local_display_string(tp, "%Y/%m/%d") == "2026/01/15");
}

TEST_CASE("to_local_display_string_default_format_is_fixed_width", tags) {
    const auto tp = make_utc(2026, 1, 15, 14, 30, 0);
    const auto s = datetime::to_local_display_string(tp);
    CHECK(s.size() == 19);
    CHECK(s[4] == '-');
    CHECK(s[7] == '-');
    CHECK(s[10] == ' ');
    CHECK(s[13] == ':');
    CHECK(s[16] == ':');
}

TEST_CASE("k_timestamp_format_is_the_local_display_format", tags) {
    CHECK(std::string(ores::platform::time::k_timestamp_format) == "%Y-%m-%d %H:%M:%S");
}
