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
#include "ores.scheduler.api/domain/cron_expression.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <ctime>
#include <rfl/json.hpp>
#include <string>
#include <vector>

using namespace ores::scheduler::domain;

TEST_CASE("cron_expression::from_string accepts valid standard expressions",
          "[domain][cron_expression]") {
    const std::vector<std::string> valid_exprs = {
        "* * * * *",    // every minute
        "0 0 * * *",    // midnight daily
        "0 12 * * 1",   // noon every Monday
        "*/5 * * * *",  // every 5 minutes
        "0 0 1 * *",    // first of every month
        "30 6 * * 1-5", // 06:30 on weekdays
    };

    for (const auto& expr : valid_exprs) {
        INFO("Testing expression: " << expr);
        auto result = cron_expression::from_string(expr);
        REQUIRE(result.has_value());
        CHECK(result->to_string() == expr);
    }
}

TEST_CASE("cron_expression::from_string rejects invalid expressions", "[domain][cron_expression]") {
    const std::vector<std::string> invalid_exprs = {
        "",            // empty
        "not a cron",  // garbage
        "99 * * * *",  // minute > 59
        "* * * * * *", // 6 fields
        "0 0 6 * * 1", // 6 fields: seconds first, as Quartz writes it
        "0 6 * *",     // 4 fields
    };

    for (const auto& expr : invalid_exprs) {
        INFO("Testing invalid expression: " << expr);
        auto result = cron_expression::from_string(expr);
        CHECK_FALSE(result.has_value());
        if (!result.has_value())
            CHECK(!result.error().empty());
    }
}

TEST_CASE("cron_expression::to_string round-trips the input", "[domain][cron_expression]") {
    const std::string expr = "0 6 * * 1-5";
    auto sut = cron_expression::from_string(expr);
    REQUIRE(sut.has_value());
    CHECK(sut->to_string() == expr);
}

TEST_CASE("cron_expression::next_occurrence advances by the expression's interval",
          "[domain][cron_expression]") {
    // A fixed instant on a minute boundary, so the expectation below is a
    // literal rather than a measurement of the clock. Asserting only that the
    // result is in the future passes for any implementation that returns
    // anything at all, which is what this case used to do.
    const auto after = std::chrono::system_clock::from_time_t(1700000040);

    auto sut = cron_expression::from_string("* * * * *");
    REQUIRE(sut.has_value());
    CHECK(sut->next_occurrence(after) - after == std::chrono::seconds(60));
}

TEST_CASE("cron_expression::next_occurrence lands on a local midnight",
          "[domain][cron_expression]") {
    const auto after = std::chrono::system_clock::from_time_t(1700000040);

    auto sut = cron_expression::from_string("0 0 * * *");
    REQUIRE(sut.has_value());

    const auto next = sut->next_occurrence(after);
    CHECK(next > after);
    const auto as_time_t = std::chrono::system_clock::to_time_t(next);
    std::tm local{};
    localtime_r(&as_time_t, &local);
    CHECK(local.tm_hour == 0);
    CHECK(local.tm_min == 0);
}

TEST_CASE("cron_expression::from_string names the field count it rejects",
          "[domain][cron_expression]") {
    const auto result = cron_expression::from_string("* * * * * *");
    REQUIRE_FALSE(result.has_value());
    CHECK(result.error().find("expected 5 fields") != std::string::npos);
    CHECK(result.error().find("found 6") != std::string::npos);
}

TEST_CASE("cron_expression::from_string accepts a run of spaces between fields",
          "[domain][cron_expression]") {
    // croncpp drops empty parts, so several spaces separate one field.
    const auto result = cron_expression::from_string("0  0   *  *  *");
    REQUIRE(result.has_value());
    CHECK(result->to_string() == "0  0   *  *  *");
}

TEST_CASE("cron_expression::from_string counts fields as croncpp splits them",
          "[domain][cron_expression]") {
    // croncpp splits on the space character alone, so a tab joins two parts
    // into one field. The count must agree with that split, or the message
    // would name a different number of fields than the library sees.
    const auto result = cron_expression::from_string("* * * *\t*");
    REQUIRE_FALSE(result.has_value());
    CHECK(result.error().find("found 4") != std::string::npos);
}

TEST_CASE("cron_expression::next_occurrence matches either restricted day field",
          "[domain][cron_expression]") {
    // POSIX cron: with both day of month and day of week restricted, a day
    // matching either one fires. 2026-10-06 is a Tuesday, so the next
    // match is Monday 2026-10-12 rather than 1 November or a Monday 1st.
    std::tm start{};
    start.tm_year = 2026 - 1900;
    start.tm_mon = 9;
    start.tm_mday = 6;
    start.tm_isdst = -1;
    const auto after = std::chrono::system_clock::from_time_t(std::mktime(&start));

    auto sut = cron_expression::from_string("0 9 1 * 1");
    REQUIRE(sut.has_value());

    const auto as_time_t = std::chrono::system_clock::to_time_t(sut->next_occurrence(after));
    std::tm local{};
    localtime_r(&as_time_t, &local);
    CHECK(local.tm_year == 2026 - 1900);
    CHECK(local.tm_mon == 9);
    CHECK(local.tm_mday == 12);
    CHECK(local.tm_hour == 9);
    CHECK(local.tm_min == 0);
}

TEST_CASE("cron_expression equality operator", "[domain][cron_expression]") {
    auto a = cron_expression::from_string("0 0 * * *");
    auto b = cron_expression::from_string("0 0 * * *");
    auto c = cron_expression::from_string("*/5 * * * *");

    REQUIRE(a.has_value());
    REQUIRE(b.has_value());
    REQUIRE(c.has_value());

    CHECK(*a == *b);
    CHECK_FALSE(*a == *c);
}

TEST_CASE("cron_expression round-trips through rfl as its string", "[domain][cron_expression]") {
    const auto parsed = cron_expression::from_string("*/5 * * * *");
    REQUIRE(parsed.has_value());

    const auto written = rfl::json::write(*parsed);
    CHECK(written == R"("*/5 * * * *")");

    const auto read = rfl::json::read<cron_expression>(written);
    REQUIRE(read.has_value());
    CHECK(*read == *parsed);
}

TEST_CASE("rfl read refuses a string that is not a cron expression", "[domain][cron_expression]") {
    const auto read = rfl::json::read<cron_expression>(R"("not a cron")");
    CHECK_FALSE(read.has_value());
}
