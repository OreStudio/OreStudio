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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/generators/market_observation_generator.hpp"
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/tick_coordinate_repository.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

const std::string_view test_suite("ores.marketdata.core.tests");
const std::string tags("[repository][ticks]");

using namespace ores::marketdata::generators;
using ores::marketdata::repository::market_observation_repository;
using ores::marketdata::repository::market_series_repository;
using ores::marketdata::repository::tick_coordinate_repository;
using ores::testing::database_helper;

/// The session of the series' party, which the party policy of the tick table needs.
ores::database::context party_context(database_helper& h, const boost::uuids::uuid& party_id) {
    return h.context().with_party(h.tenant_id(), party_id, {party_id}, h.db_user());
}

/// Appends one tick as the owner of the series. A statement that returns no row is refused
/// by the read helper, so the insert states one.
void insert_tick(ores::database::context ctx,
                 const boost::uuids::uuid& party_id,
                 const boost::uuids::uuid& series_id,
                 int coord,
                 std::chrono::system_clock::time_point ts,
                 double value) {
    using ores::platform::time::datetime;
    auto lg(ores::logging::make_logger(test_suite));
    ores::database::repository::execute_parameterized_multi_column_query(
        ctx,
        "INSERT INTO ores_marketdata_ticks_tbl (tenant_id, party_id, series_id, ts, coord, value) "
        "VALUES ($1::uuid, $2::uuid, $3::uuid, $4::timestamptz, $5::smallint, $6::float8) "
        "RETURNING 1",
        {ctx.tenant_id().to_string(),
         boost::uuids::to_string(party_id),
         boost::uuids::to_string(series_id),
         datetime::to_iso8601_utc(ts),
         std::to_string(coord),
         std::format("{}", value)},
        lg,
        "inserting a tick");
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
    insert_tick(owner, s.party_id, s.id, 0, t0, 1.0845);

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
    insert_tick(party_context(h, s.party_id), s.party_id, s.id, 0, t0, 1.5);

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
    insert_tick(owner, s.party_id, s.id, 0, t0, 1.0);

    const auto points =
        market_observation_repository{}.read_as_of(owner, s.id, t0 + std::chrono::minutes(1));
    REQUIRE(points.size() == 1);
    CHECK(points.front().id == observed.id);
    CHECK(points.front().source == "feed.test");
}
