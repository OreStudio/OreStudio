/*
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
#include "ores.marketdata.service/app/staleness_policy.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>

constexpr const char* policy_tags = "[staleness_policy]";

namespace {

using ores::marketdata::service::app::staleness_policy;
using std::chrono::seconds;
using std::chrono::system_clock;

system_clock::time_point instant_at(seconds offset) {
    return system_clock::time_point{} + std::chrono::hours{24 * 365 * 50} + offset;
}

}

TEST_CASE("a_value_inside_the_threshold_is_not_stale", policy_tags) {
    staleness_policy::configure(60);
    const auto now = instant_at(seconds{1000});
    CHECK(staleness_policy::age_seconds(now - seconds{60}, now) == 60);
    CHECK(staleness_policy::is_stale(now - seconds{60}, now) == false);
}

TEST_CASE("a_value_past_the_threshold_is_stale", policy_tags) {
    staleness_policy::configure(60);
    const auto now = instant_at(seconds{1000});
    CHECK(staleness_policy::is_stale(now - seconds{61}, now) == true);
}

TEST_CASE("a_value_with_no_instant_is_stale_and_has_no_age", policy_tags) {
    staleness_policy::configure(60);
    const auto now = instant_at(seconds{1000});
    CHECK(staleness_policy::age_seconds(system_clock::time_point{}, now) == 0);
    CHECK(staleness_policy::is_stale(system_clock::time_point{}, now) == true);
}

TEST_CASE("an_instant_after_now_has_no_negative_age", policy_tags) {
    staleness_policy::configure(60);
    const auto now = instant_at(seconds{1000});
    CHECK(staleness_policy::age_seconds(now + seconds{30}, now) == 0);
    CHECK(staleness_policy::is_stale(now + seconds{30}, now) == false);
}

TEST_CASE("the_threshold_is_configurable_and_refuses_a_non_positive_one", policy_tags) {
    staleness_policy::configure(120);
    CHECK(staleness_policy::threshold_seconds() == 120);
    CHECK(staleness_policy::is_configured() == true);
    staleness_policy::configure(0);
    CHECK(staleness_policy::threshold_seconds() == 3600);
}
