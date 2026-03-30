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
#include "ores.platform/time/time_utils.hpp"

#include <ctime>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.platform.tests");
const std::string tags("[time][time_utils]");

using ores::platform::time::time_utils;

/**
 * @brief Returns a UTC time_point for a fixed date/time expressed as UTC fields.
 *
 * Uses timegm so the result is always UTC regardless of the process timezone.
 */
std::chrono::system_clock::time_point make_utc(
    int year, int mon, int mday, int hour, int min, int sec) {
    std::tm tm = {};
    tm.tm_year = year - 1900;
    tm.tm_mon = mon - 1;
    tm.tm_mday = mday;
    tm.tm_hour = hour;
    tm.tm_min = min;
    tm.tm_sec = sec;
    return time_utils::to_time_point_utc(tm);
}

/**
 * @brief Returns what libpq would return for the given UTC time_point:
 *        the local wall-clock representation without timezone suffix.
 *
 * libpq formats timestamptz in the session's local timezone, e.g.
 * "2026-03-30 12:55:00+01" for a BST session.  rfl::Timestamp strips
 * the offset (strptime stops at "+"), leaving just the wall-clock part.
 * This helper reproduces that: format the UTC tp as local time, without
 * the offset.
 */
std::string utc_to_local_string(const std::chrono::system_clock::time_point& tp) {
    const auto t = std::chrono::system_clock::to_time_t(tp);
    std::tm local_tm = {};
    time_utils::localtime_safe(&t, &local_tm);
    char buf[32];
    std::strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S", &local_tm);
    return buf;
}

} // namespace

TEST_CASE("to_time_point_local_roundtrips_utc_via_local_string", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    // A fixed UTC time that falls within a DST period in European timezones.
    // 2026-03-30 11:55:00 UTC = 12:55:00 BST / 13:55:00 CEST / 11:55:00 UTC.
    const auto expected = make_utc(2026, 3, 30, 11, 55, 0);

    // Simulate the string that rfl::Timestamp would receive: local wall-clock
    // time with the timezone suffix already stripped by strptime.
    const auto local_str = utc_to_local_string(expected);
    BOOST_LOG_SEV(lg, ores::logging::info)
        << "Local string for 2026-03-30 11:55:00 UTC: " << local_str;

    // Parse via strptime the same way rfl::Timestamp does (zero-init, then
    // strptime), and verify to_time_point_local recovers the original UTC tp.
    std::tm parsed_tm = {};
    ::strptime(local_str.c_str(), "%Y-%m-%d %H:%M:%S", &parsed_tm);

    const auto result = time_utils::to_time_point_local(parsed_tm);

    BOOST_LOG_SEV(lg, ores::logging::info)
        << "Expected epoch s: " << std::chrono::system_clock::to_time_t(expected)
        << "  Result epoch s: " << std::chrono::system_clock::to_time_t(result);

    CHECK(result == expected);
}

TEST_CASE("to_time_point_local_roundtrips_utc_via_local_string_winter", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    // A fixed UTC time in winter (no DST in European zones) as a control case.
    // 2026-01-15 14:30:00 UTC = 14:30:00 GMT / 15:30:00 CET / 14:30:00 UTC.
    const auto expected = make_utc(2026, 1, 15, 14, 30, 0);

    const auto local_str = utc_to_local_string(expected);
    BOOST_LOG_SEV(lg, ores::logging::info)
        << "Local string for 2026-01-15 14:30:00 UTC: " << local_str;

    std::tm parsed_tm = {};
    ::strptime(local_str.c_str(), "%Y-%m-%d %H:%M:%S", &parsed_tm);

    const auto result = time_utils::to_time_point_local(parsed_tm);

    CHECK(result == expected);
}

TEST_CASE("to_time_point_utc_roundtrips", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto expected = make_utc(2026, 3, 30, 11, 55, 0);

    // Round-trip: utc → tm fields → back to time_point via to_time_point_utc.
    const auto t = std::chrono::system_clock::to_time_t(expected);
    std::tm utc_tm = {};
    time_utils::gmtime_safe(&t, &utc_tm);

    const auto result = time_utils::to_time_point_utc(utc_tm);

    CHECK(result == expected);
}
