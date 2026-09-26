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
#include "ores.scheduler.core/service/schedule_decision.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

using namespace ores::scheduler;
using namespace std::chrono_literals;

namespace {

using clock = std::chrono::system_clock;

domain::job_definition make_job(std::string_view expression, bool active = true) {
    const auto parsed = domain::cron_expression::from_string(expression);
    REQUIRE(parsed.has_value());

    domain::job_definition job;
    job.job_name = "test job";
    job.schedule_expression = *parsed;
    job.is_active = active;
    return job;
}

} // anonymous namespace

TEST_CASE("A job is due when its next occurrence is at or before now", "[schedule_decision]") {
    const auto now = clock::from_time_t(1'700'000'040); // 00:00:40 past the minute
    const auto job = make_job("*/5 * * * *");

    // Last ran at the previous 5-minute mark, so the next occurrence is 00:05:00.
    const auto last = clock::from_time_t(1'700'000'000);
    REQUIRE_FALSE(service::is_due(job, last, now));

    const auto later = clock::from_time_t(1'700'000'300); // exactly 00:05:00
    REQUIRE(service::is_due(job, last, later));
}

TEST_CASE("A job never run is treated as having run a minute ago", "[schedule_decision]") {
    // Every minute is due; the five-minute schedule is not yet due one minute
    // after the implicit previous run.
    const auto now = clock::from_time_t(1'700'000'040);
    REQUIRE(service::is_due(make_job("* * * * *"), std::nullopt, now));
    REQUIRE_FALSE(service::is_due(make_job("*/5 * * * *"), std::nullopt, now));
}

TEST_CASE("An inactive job is never due", "[schedule_decision]") {
    const auto now = clock::from_time_t(1'700'000'300);
    const auto last = clock::from_time_t(1'700'000'000);
    REQUIRE_FALSE(service::is_due(make_job("* * * * *", false), last, now));
}

TEST_CASE("The next minute boundary is strictly after the given instant", "[schedule_decision]") {
    const auto on_the_minute = clock::from_time_t(1'700'000'040);
    REQUIRE(service::next_minute_boundary(on_the_minute) == clock::from_time_t(1'700'000'100));

    const auto already_on_the_minute = clock::from_time_t(1'700'000'100);
    REQUIRE(service::next_minute_boundary(already_on_the_minute) ==
            clock::from_time_t(1'700'000'160));
}
