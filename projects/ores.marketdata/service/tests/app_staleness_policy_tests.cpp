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

/// Puts the policy back, so a case that configures it leaves no verdict behind.
struct restore_policy final {
    std::int64_t was = staleness_policy::threshold_seconds();
    ~restore_policy() {
        staleness_policy::configure(was);
        staleness_policy::configure("fx", 0);
        staleness_policy::configure("ir", 0);
    }
};

}

TEST_CASE("a_value_inside_the_threshold_is_not_stale", policy_tags) {
    restore_policy guard;
    staleness_policy::configure(60);
    const auto now = instant_at(seconds{1000});
    CHECK(staleness_policy::age_seconds(now - seconds{60}, now) == 60);
    CHECK(staleness_policy::is_stale(now - seconds{60}, now) == false);
}

TEST_CASE("a_value_past_the_threshold_is_stale", policy_tags) {
    restore_policy guard;
    staleness_policy::configure(60);
    const auto now = instant_at(seconds{1000});
    CHECK(staleness_policy::is_stale(now - seconds{61}, now) == true);
}

TEST_CASE("a_value_with_no_instant_is_stale_and_has_no_age", policy_tags) {
    restore_policy guard;
    staleness_policy::configure(60);
    const auto now = instant_at(seconds{1000});
    CHECK(staleness_policy::age_seconds(system_clock::time_point{}, now) == 0);
    CHECK(staleness_policy::is_stale(system_clock::time_point{}, now) == true);
}

TEST_CASE("an_instant_after_now_has_no_negative_age", policy_tags) {
    restore_policy guard;
    staleness_policy::configure(60);
    const auto now = instant_at(seconds{1000});
    CHECK(staleness_policy::age_seconds(now + seconds{30}, now) == 0);
    CHECK(staleness_policy::is_stale(now + seconds{30}, now) == false);
}

TEST_CASE("the_threshold_is_configurable_and_refuses_a_non_positive_one", policy_tags) {
    restore_policy guard;
    staleness_policy::configure(120);
    CHECK(staleness_policy::threshold_seconds() == 120);
    CHECK(staleness_policy::is_configured() == true);
    staleness_policy::configure(0);
    CHECK(staleness_policy::threshold_seconds() == 3600);
}

TEST_CASE("each_instrument_kind_has_its_own_threshold", policy_tags) {
    // Zero removes the kind's own threshold, so a later case starts where this
    // one did rather than inheriting "fx" and "ir".
    restore_policy guard;

    staleness_policy::configure(3600);
    staleness_policy::configure("fx", 60);
    staleness_policy::configure("ir", 86400);

    const auto now = std::chrono::system_clock::time_point{} + std::chrono::hours{24 * 365 * 50};
    const auto two_minutes_ago = now - std::chrono::seconds{120};

    // One value, two verdicts: the same age is stale to the FX threshold and current to the
    // IR one, which is the whole point of stating the threshold per instrument kind.
    CHECK(staleness_policy::is_stale("fx", two_minutes_ago, now));
    CHECK(staleness_policy::is_stale("ir", two_minutes_ago, now) == false);
    CHECK(staleness_policy::threshold_seconds("fx") == 60);
    CHECK(staleness_policy::threshold_seconds("ir") == 86400);

    // A kind the deployment does not name falls back to the one threshold.
    CHECK(staleness_policy::threshold_seconds("equity") == 3600);
}
