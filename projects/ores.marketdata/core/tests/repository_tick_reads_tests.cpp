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
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/generators/market_observation_generator.hpp"
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.core/repository/feed_binding_activity_repository.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/tick_coordinate_repository.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "tick_test_support.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <set>
#include <string>

namespace {

const std::string tags("[repository][ticks]");

using namespace ores::marketdata::generators;
using ores::marketdata::repository::feed_binding_activity_entry;
using ores::marketdata::repository::feed_binding_activity_repository;
using ores::marketdata::repository::market_observation_repository;
using ores::marketdata::repository::market_series_repository;
using ores::marketdata::repository::tick_coordinate_repository;
using ores::marketdata::repository::tick_datum;
using ores::testing::database_helper;

using ores::marketdata::tests::insert_tick;
using ores::marketdata::tests::party_context;

/// The number of the series' own datum in the tick dictionary, assigned if it has none.
int number_of(const ores::database::context& owner,
              const ores::marketdata::domain::market_series& s) {
    return tick_coordinate_repository{}.resolve(owner, s.party_id, s.id, s.oresmd_uri);
}

/// A whole second, so that the instants of a test survive the round trip through text.
std::chrono::system_clock::time_point now_second() {
    return std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now());
}

}

TEST_CASE("a_tick_is_the_point_of_a_series_without_coordinates", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto s = generate_synthetic_market_series(ctx);
    market_series_repository{}.write(h.context(), s);

    const auto t0 = now_second();
    const auto owner = party_context(h, s.party_id);
    const auto number = number_of(owner, s);
    insert_tick(owner, s.party_id, s.id, number, t0, 1.0845);

    const auto points =
        market_observation_repository{}.read_as_of(owner, s.id, t0 + std::chrono::minutes(1));
    REQUIRE(points.size() == 1);
    CHECK(points.front().oresmd_uri == s.oresmd_uri);
    CHECK(points.front().value == "1.0845");
    CHECK(points.front().observation_datetime == t0);
    CHECK(points.front().source.empty());
}

TEST_CASE("a_snapshot_reads_the_latest_tick_of_each_coordinate_at_or_before_the_instant", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto s = generate_synthetic_market_series(ctx);
    market_series_repository{}.write(h.context(), s);

    const auto owner = party_context(h, s.party_id);
    const auto spot = "oresmd://test/" + boost::uuids::to_string(s.id) + "/SPOT";
    const auto year = "oresmd://test/" + boost::uuids::to_string(s.id) + "/1Y";
    tick_coordinate_repository numbers;
    const auto spot_number = numbers.resolve(owner, s.party_id, s.id, spot);
    const auto year_number = numbers.resolve(owner, s.party_id, s.id, year);
    CHECK(spot_number == 1);
    CHECK(year_number == 2);
    CHECK(numbers.resolve(owner, s.party_id, s.id, spot) == spot_number);

    const auto t0 = now_second();
    insert_tick(owner, s.party_id, s.id, spot_number, t0, 1.0);
    insert_tick(owner, s.party_id, s.id, spot_number, t0 + std::chrono::seconds(10), 2.0);
    insert_tick(owner, s.party_id, s.id, year_number, t0 + std::chrono::seconds(5), 3.0);
    insert_tick(owner, s.party_id, s.id, year_number, t0 + std::chrono::seconds(30), 4.0);

    const auto points =
        market_observation_repository{}.read_as_of(owner, s.id, t0 + std::chrono::seconds(20));
    REQUIRE(points.size() == 2);
    for (const auto& p : points) {
        if (p.oresmd_uri == spot)
            CHECK(p.value == "2");
        else if (p.oresmd_uri == year)
            CHECK(p.value == "3");
        else
            FAIL("an unexpected coordinate: " << p.oresmd_uri);
    }
}

TEST_CASE("a_tick_of_another_party_is_not_in_the_snapshot", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto s = generate_synthetic_market_series(ctx);
    market_series_repository{}.write(h.context(), s);

    const auto t0 = now_second();
    const auto owner = party_context(h, s.party_id);
    insert_tick(owner, s.party_id, s.id, number_of(owner, s), t0, 1.5);

    const auto stranger = boost::uuids::random_generator{}();
    const auto points = market_observation_repository{}.read_as_of(
        party_context(h, stranger), s.id, t0 + std::chrono::minutes(1));
    CHECK(points.empty());
}

TEST_CASE("an_observation_row_wins_a_tie_with_a_tick_at_the_same_instant", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto s = generate_synthetic_market_series(ctx);
    market_series_repository{}.write(h.context(), s);

    const auto t0 = now_second();
    auto observed = generate_synthetic_market_observation(ctx);
    observed.series_id = s.id;
    observed.party_id = s.party_id;
    observed.oresmd_uri = s.oresmd_uri;
    observed.observation_datetime = t0;
    observed.value = "1.0";
    observed.source = "feed.test";
    market_observation_repository{}.insert(h.context(), observed);

    const auto owner = party_context(h, s.party_id);
    const auto number = number_of(owner, s);
    insert_tick(owner, s.party_id, s.id, number, t0, 1.0);

    const auto points =
        market_observation_repository{}.read_as_of(owner, s.id, t0 + std::chrono::minutes(1));
    REQUIRE(points.size() == 1);
    CHECK(points.front().id == observed.id);
    CHECK(points.front().source == "feed.test");
}

TEST_CASE("each_bucket_of_the_evolution_reads_the_latest_tick_at_its_boundary", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto s = generate_synthetic_market_series(ctx);
    market_series_repository{}.write(h.context(), s);

    const auto t0 = now_second();
    const auto owner = party_context(h, s.party_id);
    const auto number = number_of(owner, s);
    insert_tick(owner, s.party_id, s.id, number, t0, 1.0);
    insert_tick(owner, s.party_id, s.id, number, t0 + std::chrono::minutes(10), 2.0);

    const auto buckets = market_observation_repository{}.read_as_of_buckets(
        owner, s.id, t0 + std::chrono::minutes(20), std::chrono::minutes(10), 3);
    REQUIRE(buckets.size() == 3);
    for (const auto& bucket : buckets)
        REQUIRE(bucket.size() == 1);
    CHECK(buckets[0].front().value == "1");
    CHECK(buckets[1].front().value == "2");
    CHECK(buckets[2].front().value == "2");
}

TEST_CASE("the_instants_of_a_series_are_the_times_of_its_ticks", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto s = generate_synthetic_market_series(ctx);
    market_series_repository{}.write(h.context(), s);

    const auto t0 = now_second();
    const auto owner = party_context(h, s.party_id);
    const auto number = number_of(owner, s);
    insert_tick(owner, s.party_id, s.id, number, t0, 1.0);
    insert_tick(owner, s.party_id, s.id, number, t0 + std::chrono::seconds(5), 2.0);

    market_observation_repository repo;
    const auto all = repo.read_instants(
        owner, s.id, t0 - std::chrono::seconds(1), t0 + std::chrono::seconds(10), {}, 10);
    REQUIRE(all.size() == 2);
    CHECK(all[0].as_of == t0);
    REQUIRE(all[0].points.size() == 1);
    CHECK(all[0].points.front().value == "1");
    CHECK(all[1].as_of == t0 + std::chrono::seconds(5));
    CHECK(all[1].points.front().value == "2");

    const auto stated = repo.read_instants(owner,
                                           s.id,
                                           t0 - std::chrono::seconds(1),
                                           t0 + std::chrono::seconds(10),
                                           {t0 + std::chrono::seconds(5)},
                                           10);
    REQUIRE(stated.size() == 1);
    CHECK(stated.front().points.front().value == "2");
}

TEST_CASE("the_numbers_of_many_datums_are_assigned_together_and_stay_the_same", tags) {
    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto first = generate_synthetic_market_series(ctx);
    auto second = generate_synthetic_market_series(ctx);
    market_series_repository{}.write(h.context(), first);
    market_series_repository{}.write(h.context(), second);
    const auto owner = party_context(h, first.party_id);

    // A URI may hold a comma, a quote and a backslash, which an array literal must carry.
    const std::string awkward = R"(oresmd://test/a,"b"\c)";
    tick_coordinate_repository numbers;
    const auto assigned = numbers.resolve_all(owner,
                                              {{first.party_id, first.id, "oresmd://test/b"},
                                               {first.party_id, first.id, "oresmd://test/a"},
                                               {second.party_id, second.id, "oresmd://test/b"},
                                               {first.party_id, first.id, awkward},
                                               {first.party_id, first.id, "oresmd://test/a"}});
    REQUIRE(assigned.size() == 5);

    // The numbers of a series are distinct and start at 1. Which URI gets which is the order of
    // the database, so the test does not state it.
    CHECK(assigned[4] == assigned[1]);
    CHECK(assigned[2] == 1);
    std::set<int> of_first{assigned[0], assigned[1], assigned[3]};
    CHECK(of_first == std::set<int>{1, 2, 3});

    // A second call finds the same numbers, and a new datum takes the next one.
    const auto again = numbers.resolve_all(owner,
                                           {{first.party_id, first.id, "oresmd://test/b"},
                                            {first.party_id, first.id, "oresmd://test/new"}});
    CHECK(again[0] == assigned[0]);
    CHECK(again[1] == 4);
    CHECK(numbers.resolve_all(owner, {}).empty());
}

TEST_CASE("the_activity_of_many_bindings_is_recorded_together_and_a_newer_tick_wins", tags) {
    database_helper h;
    const auto tag = boost::uuids::to_string(boost::uuids::random_generator{}());
    const auto party = boost::uuids::random_generator{}();
    const auto series_a = boost::uuids::random_generator{}();
    const auto series_b = boost::uuids::random_generator{}();
    const auto source_a = "synthetic.review." + tag + ".a,\"x\"";
    const auto source_b = "synthetic.review." + tag + ".b";
    const auto t0 = now_second();

    feed_binding_activity_repository repo;
    repo.record(h.context(), {{party, source_a, series_a, t0}, {party, source_b, series_b, t0}});
    const auto first = repo.read(h.context(), party, source_a);
    REQUIRE(first.has_value());
    CHECK(first->series_id == series_a);
    CHECK(first->last_tick_at == t0);
    REQUIRE(repo.read(h.context(), party, source_b).has_value());

    // An older tick changes nothing, and a newer one replaces the row.
    repo.record(h.context(), {{party, source_a, series_a, t0 - std::chrono::seconds(5)}});
    CHECK(repo.read(h.context(), party, source_a)->last_tick_at == t0);
    repo.record(h.context(), {{party, source_a, series_b, t0 + std::chrono::seconds(5)}});
    const auto later = repo.read(h.context(), party, source_a);
    CHECK(later->series_id == series_b);
    CHECK(later->last_tick_at == t0 + std::chrono::seconds(5));
    CHECK_FALSE(repo.read(h.context(), party, source_b + ".none").has_value());
    repo.record(h.context(), {});
}
