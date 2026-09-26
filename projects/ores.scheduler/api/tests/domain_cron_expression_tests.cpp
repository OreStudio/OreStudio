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
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <ctime>
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
        "* * * * * *", // 6 fields (croncpp uses 5)
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
