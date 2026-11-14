/* -*- mode: c++; tab-width: 4; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
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
#include "ores.marketdata.service/app/staleness_policy.hpp"
#include "published_surface_fixture.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstdint>
#include <string>
#include <vector>

constexpr const char* published_surface_tags = "[published_surface][database]";

namespace {

using ores::marketdata::service::app::staleness_policy;
using ores::marketdata::service::tests::surface_fixture;

/// The instant a fixture is published at when the test wants it to read as fresh.
std::chrono::system_clock::time_point a_minute_ago() {
    return std::chrono::system_clock::now() - std::chrono::minutes{1};
}

/// Puts the threshold back, so a suite that changes it leaves no verdict behind.
struct threshold_guard final {
    std::int64_t was = staleness_policy::threshold_seconds();
    ~threshold_guard() {
        staleness_policy::configure(was);
    }
};

}

TEST_CASE("the_published_surface_states_its_model_its_convention_and_its_grid",
          published_surface_tags) {
    surface_fixture f(a_minute_ago());

    const auto resp = f.read();

    INFO("message: " << resp.message);
    REQUIRE(resp.success);
    // A strike column called ATM is not a volatility without the model it was
    // fitted by and the delta convention it is quoted under.
    CHECK(resp.model == "ssvi");
    CHECK(resp.delta_type == "Spot");
    CHECK(resp.strike_convention == "delta");
    CHECK(resp.publish_request_id == f.request_id());
    CHECK(resp.published_by.empty() == false);
    CHECK(resp.stale == false);

    CHECK(resp.expiries == std::vector<std::string>{"1W", "1M"});
    CHECK(resp.strikes == std::vector<std::string>{"DEL/Spot/Put/25", "ATM", "DEL/Spot/Call/25"});
    REQUIRE(resp.points.size() == 6);
    CHECK(resp.points.front().expiry == "1W");
    CHECK(resp.points.front().strike == "DEL/Spot/Put/25");
    CHECK(resp.points.front().volatility == 0.095);
}

TEST_CASE("a_convention_that_does_not_resolve_states_absolute_strikes", published_surface_tags) {
    surface_fixture f(a_minute_ago(), false);

    const auto resp = f.read();

    INFO("message: " << resp.message);
    REQUIRE(resp.success);
    CHECK(resp.model == "ssvi");
    CHECK(resp.delta_type.empty());
    // The blueprint names a convention by a soft reference and this one does not
    // resolve, so its strikes are levels and the read must not invent a delta type.
    CHECK(resp.strike_convention == "absolute");
    CHECK(resp.strikes == std::vector<std::string>{"1.05", "1.10", "1.15"});
}

TEST_CASE("the_stale_verdict_is_the_server_threshold_and_not_the_age_alone",
          published_surface_tags) {
    threshold_guard guard;
    surface_fixture f(std::chrono::system_clock::now() - std::chrono::hours{2});

    staleness_policy::configure(3600);
    const auto fresh_by_an_hour = f.read();
    REQUIRE(fresh_by_an_hour.success);
    CHECK(fresh_by_an_hour.stale == true);

    staleness_policy::configure(3 * 24 * 3600);
    const auto fresh_by_three_days = f.read();
    REQUIRE(fresh_by_three_days.success);
    CHECK(fresh_by_three_days.stale == false);
}

TEST_CASE("an_as_of_before_the_publication_finds_no_surface_and_says_so", published_surface_tags) {
    surface_fixture f(a_minute_ago());

    const auto resp = f.read(f.published_at() - std::chrono::seconds{1});

    CHECK(resp.success == false);
    CHECK(resp.message.empty() == false);
    CHECK(resp.points.empty());
}
