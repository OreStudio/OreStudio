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
#include "ores.analytics.quant/domain/driver_quote.hpp"
#include "ores.analytics.quant/domain/rate_status.hpp"
#include "ores.analytics.quant/domain/staleness_policy.hpp"
#include "ores.analytics.quant/service/fixing_replay.hpp"
#include "ores.analytics.quant/service/topology_builder.hpp"
#include "ores.analytics.quant/service/topology_from_sources.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <utility>
#include <vector>

using ores::analytics::quant::domain::crm_topology;
using ores::analytics::quant::domain::driver_quote;
using ores::analytics::quant::domain::rate_status;
using ores::analytics::quant::domain::staleness_policy;
using ores::analytics::quant::service::fixing_day;
using ores::analytics::quant::service::fixing_source;
using ores::analytics::quant::service::replay_fixings;
using ores::analytics::quant::service::topology_builder;
using ores::analytics::quant::service::topology_from_sources;

namespace {

using namespace std::chrono;

/// A star of everything the source quotes, against the base it quotes against.
crm_topology star(const std::string& base, const std::vector<std::string>& currencies) {
    const fixing_source source{"test", base, 10, currencies};
    const auto chosen = topology_from_sources({source});
    REQUIRE(chosen);
    return topology_builder::build(chosen->pairs, chosen->pivot_code, {});
}

year_month_day day_of(int day) {
    return year_month_day{year{2026}, month{3}, std::chrono::day{static_cast<unsigned>(day)}};
}

driver_quote quote(const std::string& base, const std::string& code, double rate) {
    return {.base_code = base, .quote_code = code, .rate = rate, .observed_at = {}};
}

}

TEST_CASE("a_cross_is_read_each_day_from_the_drivers_of_that_day", "[fixing_replay]") {
    const auto topology = star("USD", {"EUR", "JPY"});
    const std::vector<fixing_day> days{
        {.day = day_of(2), .quotes = {quote("USD", "EUR", 0.90), quote("USD", "JPY", 150.0)}},
        {.day = day_of(3), .quotes = {quote("USD", "EUR", 0.91), quote("USD", "JPY", 151.0)}},
        {.day = day_of(4), .quotes = {quote("USD", "EUR", 0.92), quote("USD", "JPY", 152.0)}}};
    const std::vector<std::pair<std::string, std::string>> pairs{{"EUR", "JPY"}};

    const auto replayed = replay_fixings(topology, days, pairs, staleness_policy{hours{24}});
    REQUIRE(replayed);
    REQUIRE(replayed->size() == 1);
    const auto& levels = replayed->front().levels;
    REQUIRE(levels.size() == 3);

    CHECK(levels[0].day == day_of(2));
    CHECK(levels[0].rate == Catch::Approx(150.0 / 0.90).epsilon(1e-12));
    CHECK(levels[1].rate == Catch::Approx(151.0 / 0.91).epsilon(1e-12));
    CHECK(levels[2].rate == Catch::Approx(152.0 / 0.92).epsilon(1e-12));
    for (const auto& level : levels)
        CHECK(level.status == rate_status::fresh);
}

TEST_CASE("the_days_are_stepped_oldest_first_whatever_order_they_arrive_in", "[fixing_replay]") {
    const auto topology = star("USD", {"EUR", "JPY"});
    const std::vector<fixing_day> days{
        {.day = day_of(4), .quotes = {quote("USD", "EUR", 0.92), quote("USD", "JPY", 152.0)}},
        {.day = day_of(2), .quotes = {quote("USD", "EUR", 0.90), quote("USD", "JPY", 150.0)}},
        {.day = day_of(3), .quotes = {quote("USD", "EUR", 0.91), quote("USD", "JPY", 151.0)}}};
    const std::vector<std::pair<std::string, std::string>> pairs{{"EUR", "JPY"}};

    const auto replayed = replay_fixings(topology, days, pairs, staleness_policy{hours{24}});
    REQUIRE(replayed);
    const auto& levels = replayed->front().levels;
    REQUIRE(levels.size() == 3);
    CHECK(levels[0].day == day_of(2));
    CHECK(levels[2].day == day_of(4));
    CHECK(levels[0].rate == Catch::Approx(150.0 / 0.90).epsilon(1e-12));
}

TEST_CASE("a_day_a_driver_was_not_published_keeps_its_row_and_ages", "[fixing_replay]") {
    // The yen is published on the first day only, so the cross is a day older on the
    // second: the history keeps both rows and says how old the answer is.
    const auto topology = star("USD", {"EUR", "JPY"});
    const std::vector<fixing_day> days{
        {.day = day_of(2), .quotes = {quote("USD", "EUR", 0.90), quote("USD", "JPY", 150.0)}},
        {.day = day_of(3), .quotes = {quote("USD", "EUR", 0.91)}}};
    const std::vector<std::pair<std::string, std::string>> pairs{{"EUR", "JPY"}};

    const auto replayed =
        replay_fixings(topology,
                       days,
                       pairs,
                       staleness_policy{.stale_after = hours{1}, .disconnected_after = hours{36}});
    REQUIRE(replayed);
    const auto& levels = replayed->front().levels;
    REQUIRE(levels.size() == 2);
    CHECK(levels[0].status == rate_status::fresh);
    CHECK(levels[1].status == rate_status::stale);
    CHECK(levels[1].rate == Catch::Approx(150.0 / 0.91).epsilon(1e-12));
}

TEST_CASE("a_pair_the_topology_does_not_hold_is_refused_and_named", "[fixing_replay]") {
    const auto topology = star("USD", {"EUR", "JPY"});
    const std::vector<fixing_day> days{};
    const std::vector<std::pair<std::string, std::string>> pairs{{"EUR", "CHF"}};

    const auto replayed = replay_fixings(topology, days, pairs, staleness_policy{hours{24}});
    REQUIRE_FALSE(replayed);
    CHECK(replayed.error().find("CHF") != std::string::npos);
}

TEST_CASE("a_replay_of_no_pair_is_refused", "[fixing_replay]") {
    const auto topology = star("USD", {"EUR"});
    const std::vector<fixing_day> days{};
    const std::vector<std::pair<std::string, std::string>> pairs{};

    const auto replayed = replay_fixings(topology, days, pairs, staleness_policy{hours{24}});
    REQUIRE_FALSE(replayed);
}

TEST_CASE("no_days_is_an_empty_history_and_not_a_failure", "[fixing_replay]") {
    const auto topology = star("USD", {"EUR", "JPY"});
    const std::vector<fixing_day> days{};
    const std::vector<std::pair<std::string, std::string>> pairs{{"EUR", "JPY"}};

    const auto replayed = replay_fixings(topology, days, pairs, staleness_policy{hours{24}});
    REQUIRE(replayed);
    REQUIRE(replayed->size() == 1);
    CHECK(replayed->front().levels.empty());
}
