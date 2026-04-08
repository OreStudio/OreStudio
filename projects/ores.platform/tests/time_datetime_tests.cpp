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
#include <stdexcept>
#include <catch2/catch_test_macros.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.platform/time/time_utils.hpp"

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
std::chrono::system_clock::time_point make_utc(
    int year, int mon, int mday, int hour, int min, int sec) {
    std::tm tm = {};
    tm.tm_year = year - 1900;
    tm.tm_mon  = mon - 1;
    tm.tm_mday = mday;
    tm.tm_hour = hour;
    tm.tm_min  = min;
    tm.tm_sec  = sec;
    return time_utils::to_time_point_utc(tm);
}

} // namespace

// ---------------------------------------------------------------------------
// to_iso8601_utc
// ---------------------------------------------------------------------------

TEST_CASE("to_iso8601_utc_output_ends_with_Z", tags) {
    const auto tp = make_utc(2026, 4, 8, 10, 30, 0);
    const auto s = datetime::to_iso8601_utc(tp);
    CHECK(!s.empty());
    CHECK(s.back() == 'Z');
}

TEST_CASE("to_iso8601_utc_known_fixed_point", tags) {
    // 2026-01-15 14:30:00 UTC — well inside winter (no DST ambiguity)
    const auto tp = make_utc(2026, 1, 15, 14, 30, 0);
    CHECK(datetime::to_iso8601_utc(tp) == "2026-01-15 14:30:00Z");
}

TEST_CASE("to_iso8601_utc_dst_edge_point", tags) {
    // 2026-03-29 01:00:00 UTC — clocks-change instant in Europe.
    // UTC representation must be stable regardless of host timezone.
    const auto tp = make_utc(2026, 3, 29, 1, 0, 0);
    CHECK(datetime::to_iso8601_utc(tp) == "2026-03-29 01:00:00Z");
}

// ---------------------------------------------------------------------------
// from_iso8601_utc — accepted inputs
// ---------------------------------------------------------------------------

TEST_CASE("from_iso8601_utc_accepts_Z_suffix", tags) {
    const auto expected = make_utc(2026, 4, 8, 10, 30, 0);
    CHECK(datetime::from_iso8601_utc("2026-04-08 10:30:00Z") == expected);
}

TEST_CASE("from_iso8601_utc_accepts_plus_00_00_suffix", tags) {
    const auto expected = make_utc(2026, 4, 8, 10, 30, 0);
    CHECK(datetime::from_iso8601_utc("2026-04-08 10:30:00+00:00") == expected);
}

TEST_CASE("from_iso8601_utc_accepts_plus_00_suffix", tags) {
    // PostgreSQL emits "+00" when the session timezone is UTC
    const auto expected = make_utc(2026, 4, 8, 10, 30, 0);
    CHECK(datetime::from_iso8601_utc("2026-04-08 10:30:00+00") == expected);
}

TEST_CASE("from_iso8601_utc_accepts_space_before_offset", tags) {
    // PostgreSQL may emit a space before the offset: "2026-04-08 10:30:00 +00"
    const auto expected = make_utc(2026, 4, 8, 10, 30, 0);
    CHECK(datetime::from_iso8601_utc("2026-04-08 10:30:00 +00") == expected);
}

// ---------------------------------------------------------------------------
// to_iso8601_utc / from_iso8601_utc round-trip
// ---------------------------------------------------------------------------

TEST_CASE("to_iso8601_utc_from_iso8601_utc_round_trip", tags) {
    const auto original = make_utc(2026, 4, 8, 10, 30, 45);
    const auto serialised = datetime::to_iso8601_utc(original);
    const auto recovered  = datetime::from_iso8601_utc(serialised);
    CHECK(recovered == original);
}

TEST_CASE("to_iso8601_utc_from_iso8601_utc_round_trip_dst_edge", tags) {
    // DST edge: 2026-03-29 01:00:00 UTC
    const auto original  = make_utc(2026, 3, 29, 1, 0, 0);
    const auto serialised = datetime::to_iso8601_utc(original);
    const auto recovered  = datetime::from_iso8601_utc(serialised);
    CHECK(recovered == original);
}

// ---------------------------------------------------------------------------
// from_iso8601_utc — rejected inputs (must throw)
// ---------------------------------------------------------------------------

TEST_CASE("from_iso8601_utc_throws_on_missing_designator", tags) {
    // Timezone-ambiguous string — must be rejected, not silently assumed UTC
    CHECK_THROWS_AS(
        datetime::from_iso8601_utc("2026-04-08 10:30:00"),
        std::invalid_argument);
}

TEST_CASE("from_iso8601_utc_throws_on_non_utc_positive_offset", tags) {
    CHECK_THROWS_AS(
        datetime::from_iso8601_utc("2026-04-08 11:30:00+01"),
        std::invalid_argument);
}

TEST_CASE("from_iso8601_utc_throws_on_non_utc_negative_offset", tags) {
    CHECK_THROWS_AS(
        datetime::from_iso8601_utc("2026-04-08 05:30:00-05:00"),
        std::invalid_argument);
}

TEST_CASE("from_iso8601_utc_throws_on_empty_string", tags) {
    CHECK_THROWS_AS(
        datetime::from_iso8601_utc(""),
        std::invalid_argument);
}

TEST_CASE("from_iso8601_utc_throws_on_malformed_string", tags) {
    CHECK_THROWS_AS(
        datetime::from_iso8601_utc("not-a-dateZ"),
        std::invalid_argument);
}

// ---------------------------------------------------------------------------
// to_local_display_string
// ---------------------------------------------------------------------------

TEST_CASE("to_local_display_string_does_not_throw", tags) {
    const auto tp = make_utc(2026, 4, 8, 10, 30, 0);
    CHECK_NOTHROW(datetime::to_local_display_string(tp));
}

TEST_CASE("to_local_display_string_output_has_no_Z_suffix", tags) {
    const auto tp = make_utc(2026, 4, 8, 10, 30, 0);
    const auto s = datetime::to_local_display_string(tp);
    CHECK(!s.empty());
    CHECK(s.back() != 'Z');
}

TEST_CASE("to_local_display_string_accepts_custom_format", tags) {
    const auto tp = make_utc(2026, 1, 15, 14, 30, 0);
    // Custom format — just verify it doesn't crash and produces non-empty output
    const auto s = datetime::to_local_display_string(tp, "%Y/%m/%d");
    CHECK(!s.empty());
}
