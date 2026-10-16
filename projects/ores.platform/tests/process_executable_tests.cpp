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
#include "ores.platform/process/executable.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.platform/time/time_utils.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <ctime>
#include <filesystem>
#include <iomanip>
#include <sstream>
#include <string>

namespace {

const std::string tags("[process][executable]");

using ores::platform::process::executable_build_time;
using ores::platform::process::executable_path;
using ores::platform::time::datetime;
using ores::platform::time::time_utils;

/**
 * @brief Returns the executable's own modification time, formatted through
 *        std::put_time rather than the strftime path under test.
 *
 * The conversion from file time is the component's own documented one; the
 * formatting is deliberately a different API so the comparison is a
 * cross-check rather than a restatement of the subject.
 */
std::string expected_build_time(const std::filesystem::path& path) {
    const auto ftime = std::filesystem::last_write_time(path);
    const auto stp = time_utils::file_time_to_system_clock(ftime);
    const auto t = std::chrono::system_clock::to_time_t(stp);

    std::tm utc_tm{};
    REQUIRE(time_utils::gmtime_safe(&t, &utc_tm) != nullptr);

    std::ostringstream oss;
    oss << std::put_time(&utc_tm, "%Y/%m/%d %H:%M:%S");
    return oss.str();
}

}

TEST_CASE("executable_path_returns_the_running_binary", tags) {
    const auto path = executable_path();

    REQUIRE_FALSE(path.empty());
    CHECK(path.filename() == "ores.platform.tests");
}

TEST_CASE("executable_path_points_at_an_existing_regular_file", tags) {
    const auto path = executable_path();

    REQUIRE_FALSE(path.empty());
    CHECK(std::filesystem::exists(path));
    CHECK(std::filesystem::is_regular_file(path));
}

TEST_CASE("executable_build_time_matches_the_binary_modification_time", tags) {
    const auto path = executable_path();
    REQUIRE_FALSE(path.empty());

    CHECK(executable_build_time() == expected_build_time(path));
}

TEST_CASE("executable_build_time_uses_the_documented_format", tags) {
    const auto value = executable_build_time();

    REQUIRE(value != "unknown");
    CHECK(value.size() == 19);
    CHECK(value[4] == '/');
    CHECK(value[7] == '/');
    CHECK(value[10] == ' ');
    CHECK(value[13] == ':');
    CHECK(value[16] == ':');
}

TEST_CASE("executable_build_time_parses_as_a_calendar_timestamp", tags) {
    const auto value = executable_build_time();
    REQUIRE(value != "unknown");

    // The platform parser for a format that is not ISO 8601. It throws when the
    // text does not match, so reaching the checks below is itself the assertion
    // that the whole string is a YYYY/MM/DD HH:MM:SS timestamp.
    const auto tp = datetime::from_utc_format(value, "%Y/%m/%d %H:%M:%S");
    const auto day = std::chrono::floor<std::chrono::days>(tp);
    const std::chrono::year_month_day ymd{day};
    const std::chrono::hh_mm_ss tod{tp - day};

    CHECK(static_cast<int>(ymd.year()) >= 2026);
    CHECK(static_cast<unsigned>(ymd.month()) >= 1);
    CHECK(static_cast<unsigned>(ymd.month()) <= 12);
    CHECK(static_cast<unsigned>(ymd.day()) >= 1);
    CHECK(static_cast<unsigned>(ymd.day()) <= 31);
    CHECK(tod.hours().count() >= 0);
    CHECK(tod.hours().count() <= 23);
    CHECK(tod.minutes().count() >= 0);
    CHECK(tod.minutes().count() <= 59);
    CHECK(tod.seconds().count() >= 0);
    CHECK(tod.seconds().count() <= 60);
}

TEST_CASE("executable_build_time_does_not_postdate_the_test_run", tags) {
    const auto value = executable_build_time();
    REQUIRE(value != "unknown");

    std::tm parsed{};
    REQUIRE(strptime(value.c_str(), "%Y/%m/%d %H:%M:%S", &parsed) != nullptr);
    parsed.tm_isdst = 0;
    const auto built_at = timegm(&parsed);

    const auto now = std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());
    CHECK(built_at <= now);
}
