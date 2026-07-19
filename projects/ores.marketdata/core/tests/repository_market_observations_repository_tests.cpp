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
#include "ores.marketdata.api/domain/market_observation_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.api/generators/market_observation_generator.hpp"
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.marketdata.core.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::marketdata::generators;

using ores::testing::database_helper;
using ores::marketdata::repository::market_series_repository;
using ores::marketdata::repository::market_observations_repository;

TEST_CASE("write_single_market_observation", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    market_observations_repository obs_repo;
    auto obs = generate_synthetic_market_observation(ctx);
    obs.series_id = s.id;
    BOOST_LOG_SEV(lg, debug) << "Observation: " << obs;
    CHECK_NOTHROW(obs_repo.write(h.context(), obs));
}

TEST_CASE("write_multiple_market_observations", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    market_observations_repository obs_repo;
    auto observations = generate_synthetic_market_observations(5, ctx);
    for (auto& o : observations)
        o.series_id = s.id;
    BOOST_LOG_SEV(lg, debug) << "Observations: " << observations;
    CHECK_NOTHROW(obs_repo.write(h.context(), observations));
}

TEST_CASE("read_latest_market_observations_by_series", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    market_observations_repository obs_repo;
    auto written = generate_synthetic_market_observations(3, ctx);
    for (auto& o : written)
        o.series_id = s.id;
    obs_repo.write(h.context(), written);

    auto read = obs_repo.read_latest(h.context(), s.id);
    BOOST_LOG_SEV(lg, debug) << "Read observations: " << read;

    CHECK(read.size() == written.size());
    for (const auto& r : read)
        CHECK(r.series_id == s.id);
}

namespace {

ores::marketdata::domain::market_observation
make_observation(ores::utility::generation::generation_context& ctx,
                 const boost::uuids::uuid& series_id,
                 const std::string& point_id,
                 std::chrono::system_clock::time_point observation_datetime,
                 double value) {
    auto o = generate_synthetic_market_observation(ctx);
    o.series_id = series_id;
    o.point_id = point_id;
    o.observation_datetime = observation_datetime;
    o.value = std::to_string(value);
    return o;
}

}

TEST_CASE("read_as_of_synchronous_publish", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    const auto now = std::chrono::system_clock::now();
    market_observations_repository obs_repo;
    obs_repo.write(h.context(), make_observation(ctx, s.id, "SPOT-1M", now, 0.04));
    obs_repo.write(h.context(), make_observation(ctx, s.id, "SPOT-3M", now, 0.041));
    obs_repo.write(h.context(), make_observation(ctx, s.id, "SPOT-2Y", now, 0.038));

    // Phase-1 case: every point shares one observation_datetime.
    auto snapshot = obs_repo.read_as_of(h.context(), s.id, now + std::chrono::minutes(1));
    BOOST_LOG_SEV(lg, debug) << "Snapshot: " << snapshot;

    CHECK(snapshot.size() == 3);
    for (const auto& o : snapshot)
        CHECK(o.series_id == s.id);
}

TEST_CASE("read_as_of_staggered_timestamps", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    const auto t0 = std::chrono::system_clock::now();
    const auto t1 = t0 + std::chrono::minutes(10);
    const auto t2 = t0 + std::chrono::minutes(20);

    market_observations_repository obs_repo;

    // SPOT-1M ticks three times; SPOT-3M ticks once early and stays stale; SPOT-2Y
    // only starts ticking at t1 -- proving the query works when points are staggered,
    // not synchronised (the general case any curve viewer must handle).
    obs_repo.write(h.context(), make_observation(ctx, s.id, "SPOT-1M", t0, 0.0400));
    obs_repo.write(h.context(), make_observation(ctx, s.id, "SPOT-1M", t1, 0.0401));
    obs_repo.write(h.context(), make_observation(ctx, s.id, "SPOT-1M", t2, 0.0402));
    obs_repo.write(h.context(), make_observation(ctx, s.id, "SPOT-3M", t0, 0.0410));
    obs_repo.write(h.context(), make_observation(ctx, s.id, "SPOT-2Y", t1, 0.0380));

    // As-of t0 + 1s: only SPOT-1M@t0 and SPOT-3M@t0 have ticked; SPOT-2Y hasn't started.
    {
        auto snap = obs_repo.read_as_of(h.context(), s.id, t0 + std::chrono::seconds(1));
        BOOST_LOG_SEV(lg, debug) << "As-of t0+1s: " << snap;
        CHECK(snap.size() == 2);
        for (const auto& o : snap) {
            if (o.point_id == "SPOT-1M")
                CHECK(o.value == "0.040000");
            else if (o.point_id == "SPOT-3M")
                CHECK(o.value == "0.041000");
            else
                FAIL("Unexpected point_id at as-of t0+1s: " << o.point_id);
        }
    }

    // As-of t1 + 1s: SPOT-1M has advanced to its t1 tick, SPOT-3M is still stale at t0,
    // SPOT-2Y has now started ticking -- exactly the "one row per point_id, latest
    // observation_datetime <= as_of" semantics, per point independently.
    {
        auto snap = obs_repo.read_as_of(h.context(), s.id, t1 + std::chrono::seconds(1));
        BOOST_LOG_SEV(lg, debug) << "As-of t1+1s: " << snap;
        CHECK(snap.size() == 3);
        for (const auto& o : snap) {
            if (o.point_id == "SPOT-1M")
                CHECK(o.value == "0.040100");
            else if (o.point_id == "SPOT-3M")
                CHECK(o.value == "0.041000");
            else if (o.point_id == "SPOT-2Y")
                CHECK(o.value == "0.038000");
            else
                FAIL("Unexpected point_id at as-of t1+1s: " << o.point_id);
        }
    }

    // Before any observation exists: empty snapshot, not an error.
    {
        auto snap = obs_repo.read_as_of(h.context(), s.id, t0 - std::chrono::hours(1));
        CHECK(snap.empty());
    }
}

TEST_CASE("read_as_of_buckets_curve_evolution", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    const auto t0 = std::chrono::system_clock::now();
    const auto t1 = t0 + std::chrono::minutes(30);
    const auto t2 = t0 + std::chrono::minutes(60);

    market_observations_repository obs_repo;
    obs_repo.write(h.context(), make_observation(ctx, s.id, "SPOT-1M", t0, 0.0400));
    obs_repo.write(h.context(), make_observation(ctx, s.id, "SPOT-1M", t1, 0.0401));
    obs_repo.write(h.context(), make_observation(ctx, s.id, "SPOT-1M", t2, 0.0402));

    // Curve-evolution view: one snapshot per bucket boundary, oldest to newest.
    std::vector<std::chrono::system_clock::time_point> boundaries{
        t0 + std::chrono::seconds(1),
        t1 + std::chrono::seconds(1),
        t2 + std::chrono::seconds(1),
    };
    auto buckets = obs_repo.read_as_of_buckets(h.context(), s.id, boundaries);
    BOOST_LOG_SEV(lg, debug) << "Bucket snapshots: " << buckets;

    REQUIRE(buckets.size() == 3);
    REQUIRE(buckets[0].size() == 1);
    REQUIRE(buckets[1].size() == 1);
    REQUIRE(buckets[2].size() == 1);
    CHECK(buckets[0].front().value == "0.040000");
    CHECK(buckets[1].front().value == "0.040100");
    CHECK(buckets[2].front().value == "0.040200");
}

TEST_CASE("remove_market_observations", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    market_observations_repository obs_repo;
    auto observations = generate_synthetic_market_observations(3, ctx);
    for (auto& o : observations)
        o.series_id = s.id;
    obs_repo.write(h.context(), observations);

    auto before = obs_repo.read_latest(h.context(), s.id);
    REQUIRE(!before.empty());

    CHECK_NOTHROW(obs_repo.remove(h.context(), s.id));

    auto after = obs_repo.read_latest(h.context(), s.id);
    BOOST_LOG_SEV(lg, debug) << "After remove count: " << after.size();
    CHECK(after.empty());
}
