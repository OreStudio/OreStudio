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
#include <catch2/catch_test_macros.hpp>
#include "ores.scheduler/domain/cron_expression.hpp"

using namespace ores::scheduler::domain;

TEST_CASE("cron_expression::from_string accepts valid standard expressions",
          "[domain][cron_expression]") {
    const std::vector<std::string> valid_exprs = {
        "* * * * *",       // every minute
        "0 0 * * *",       // midnight daily
        "0 12 * * 1",      // noon every Monday
        "*/5 * * * *",     // every 5 minutes
        "0 0 1 * *",       // first of every month
        "30 6 * * 1-5",    // 06:30 on weekdays
    };

    for (const auto& expr : valid_exprs) {
        INFO("Testing expression: " << expr);
        auto result = cron_expression::from_string(expr);
        REQUIRE(result.has_value());
        CHECK(result->to_string() == expr);
    }
}

TEST_CASE("cron_expression::from_string rejects invalid expressions",
          "[domain][cron_expression]") {
    const std::vector<std::string> invalid_exprs = {
        "",                // empty
        "not a cron",     // garbage
        "99 * * * *",     // minute > 59
        "* * * * * *",    // 6 fields (croncpp uses 5)
    };

    for (const auto& expr : invalid_exprs) {
        INFO("Testing invalid expression: " << expr);
        auto result = cron_expression::from_string(expr);
        CHECK_FALSE(result.has_value());
        if (!result.has_value())
            CHECK(!result.error().empty());
    }
}

TEST_CASE("cron_expression::to_string round-trips the input",
          "[domain][cron_expression]") {
    const std::string expr = "0 6 * * 1-5";
    auto sut = cron_expression::from_string(expr);
    REQUIRE(sut.has_value());
    CHECK(sut->to_string() == expr);
}

TEST_CASE("cron_expression::next_occurrence returns a future time point",
          "[domain][cron_expression]") {
    auto sut = cron_expression::from_string("0 0 * * *");
    REQUIRE(sut.has_value());

    const auto now = std::chrono::system_clock::now();
    const auto next = sut->next_occurrence(now);
    CHECK(next > now);
}

TEST_CASE("cron_expression equality operator",
          "[domain][cron_expression]") {
    auto a = cron_expression::from_string("0 0 * * *");
    auto b = cron_expression::from_string("0 0 * * *");
    auto c = cron_expression::from_string("*/5 * * * *");

    REQUIRE(a.has_value());
    REQUIRE(b.has_value());
    REQUIRE(c.has_value());

    CHECK(*a == *b);
    CHECK_FALSE(*a == *c);
}
