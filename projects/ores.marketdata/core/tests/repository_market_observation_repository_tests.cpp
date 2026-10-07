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
#include "ores.marketdata.api/domain/observation_lineage.hpp"
#include "ores.marketdata.api/generators/market_observation_generator.hpp"
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.api/generators/observation_lineage_generator.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/observation_lineage_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.marketdata.core.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::marketdata::generators;

using ores::testing::database_helper;
using ores::marketdata::repository::market_series_repository;
using ores::marketdata::repository::market_observation_repository;

TEST_CASE("insert_single_market_observation", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    market_observation_repository obs_repo;
    auto obs = generate_synthetic_market_observation(ctx);
    obs.series_id = s.id;
    BOOST_LOG_SEV(lg, debug) << "Observation: " << obs;
    CHECK_NOTHROW(obs_repo.insert(h.context(), obs));
}

TEST_CASE("insert_multiple_market_observations", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    market_observation_repository obs_repo;
    auto observations = generate_synthetic_market_observations(5, ctx);
    for (auto& o : observations)
        o.series_id = s.id;
    BOOST_LOG_SEV(lg, debug) << "Observations: " << observations;
    CHECK_NOTHROW(obs_repo.insert(h.context(), observations));
}

TEST_CASE("read_latest_market_observations_by_series", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    market_observation_repository obs_repo;
    auto written = generate_synthetic_market_observations(3, ctx);
    for (auto& o : written)
        o.series_id = s.id;
    obs_repo.insert(h.context(), written);

    auto read = obs_repo.read_latest_for_series(h.context(), s.id);
    BOOST_LOG_SEV(lg, debug) << "Read observations: " << read;

    CHECK(read.size() == written.size());
    for (const auto& r : read)
        CHECK(r.series_id == s.id);
}

namespace {

// The observation's own URI: one series, one maturity per row. The test only
// needs the rows to differ, so the tag is the maturity.
std::string datum_uri(const std::string& maturity) {
    // The grammar lower-cases a URI's coordinate values, so the helper does too:
    // a caller may spell the maturity either way and get the canonical string.
    std::string lower = maturity;
    for (auto& c : lower)
        c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
    return "oresmd://ir/usd?type=quote&quote=ir_swap&metric=rate&maturity=" + lower;
}

ores::marketdata::domain::market_observation
make_observation(ores::utility::generation::generation_context& ctx,
                 const boost::uuids::uuid& series_id,
                 const std::string& maturity,
                 std::chrono::system_clock::time_point observation_datetime,
                 double value) {
    auto o = generate_synthetic_market_observation(ctx);
    o.series_id = series_id;
    o.oresmd_uri = datum_uri(maturity);
    o.observation_datetime = observation_datetime;
    o.value = std::to_string(value);
    return o;
}

// The cold annex row that stamps a point's provenance. A manual point carries
// no derivation config and reads no source series, so the two nullable columns
// are cleared and the source list is the empty JSON array the relaxed check
// admits; a derived point carries all three.
ores::marketdata::domain::observation_lineage
make_lineage(ores::utility::generation::generation_context& ctx,
             const ores::marketdata::domain::market_observation& obs,
             const std::string& point_source_kind) {
    auto l = generate_synthetic_observation_lineage(ctx);
    l.party_id = obs.party_id;
    l.series_id = obs.series_id;
    l.observation_datetime = obs.observation_datetime;
    l.oresmd_uri = obs.oresmd_uri;
    l.point_source_kind = point_source_kind;
    if (point_source_kind == "manual") {
        l.derivation_config_id = std::nullopt;
        l.source_as_of = std::nullopt;
        l.source_series_ids = "[]";
    } else {
        l.derivation_config_id = ctx.generate_uuid();
        l.source_as_of = ctx.past_timepoint();
        l.source_series_ids = R"(["00000000-0000-0000-0000-000000000001"])";
    }
    l.change_reason_code = "system.test";
    l.change_commentary = "curve point provenance test";
    return l;
}

// How a reader classifies a point's source kind: a point with no annex row is
// quoted, and an annex row names one of the other two. Kept here so the
// assertion reads the design's own vocabulary.
std::string reported_source_kind(
    ores::marketdata::repository::observation_lineage_repository& lineage_repo,
    market_observation_repository::context ctx,
    const boost::uuids::uuid& series_id,
    const ores::marketdata::domain::market_observation& obs) {
    const auto lineage = lineage_repo.read_latest_by_observation(
        ctx, series_id, obs.observation_datetime, obs.oresmd_uri);
    return lineage ? lineage->point_source_kind : std::string("quoted");
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
    market_observation_repository obs_repo;
    obs_repo.insert(h.context(), make_observation(ctx, s.id, "SPOT-1M", now, 0.04));
    obs_repo.insert(h.context(), make_observation(ctx, s.id, "SPOT-3M", now, 0.041));
    obs_repo.insert(h.context(), make_observation(ctx, s.id, "SPOT-2Y", now, 0.038));

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

    market_observation_repository obs_repo;

    // SPOT-1M ticks three times; SPOT-3M ticks once early and stays stale; SPOT-2Y
    // only starts ticking at t1 -- proving the query works when points are staggered,
    // not synchronised (the general case any curve viewer must handle).
    obs_repo.insert(h.context(), make_observation(ctx, s.id, "SPOT-1M", t0, 0.0400));
    obs_repo.insert(h.context(), make_observation(ctx, s.id, "SPOT-1M", t1, 0.0401));
    obs_repo.insert(h.context(), make_observation(ctx, s.id, "spot-1m", t2, 0.0402));
    obs_repo.insert(h.context(), make_observation(ctx, s.id, "spot-3m", t0, 0.0410));
    obs_repo.insert(h.context(), make_observation(ctx, s.id, "spot-2y", t1, 0.0380));

    // As-of t0 + 1s: only spot-1m@t0 and spot-3m@t0 have ticked; spot-2y hasn't started.
    {
        auto snap = obs_repo.read_as_of(h.context(), s.id, t0 + std::chrono::seconds(1));
        BOOST_LOG_SEV(lg, debug) << "As-of t0+1s: " << snap;
        CHECK(snap.size() == 2);
        for (const auto& o : snap) {
            if (o.oresmd_uri == datum_uri("spot-1m"))
                CHECK(o.value == "0.040000");
            else if (o.oresmd_uri == datum_uri("spot-3m"))
                CHECK(o.value == "0.041000");
            else
                FAIL("Unexpected datum at as-of t0+1s: " << o.oresmd_uri);
        }
    }

    // As-of t1 + 1s: spot-1m has advanced to its t1 tick, spot-3m is still stale at t0,
    // spot-2y has started ticking -- exactly the "one row per datum, latest
    // observation_datetime <= as_of" semantics, per coordinate independently.
    {
        auto snap = obs_repo.read_as_of(h.context(), s.id, t1 + std::chrono::seconds(1));
        BOOST_LOG_SEV(lg, debug) << "As-of t1+1s: " << snap;
        CHECK(snap.size() == 3);
        for (const auto& o : snap) {
            if (o.oresmd_uri == datum_uri("spot-1m"))
                CHECK(o.value == "0.040100");
            else if (o.oresmd_uri == datum_uri("spot-3m"))
                CHECK(o.value == "0.041000");
            else if (o.oresmd_uri == datum_uri("spot-2y"))
                CHECK(o.value == "0.038000");
            else
                FAIL("Unexpected datum at as-of t1+1s: " << o.oresmd_uri);
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

    market_observation_repository obs_repo;
    obs_repo.insert(h.context(), make_observation(ctx, s.id, "SPOT-1M", t0, 0.0400));
    obs_repo.insert(h.context(), make_observation(ctx, s.id, "SPOT-1M", t1, 0.0401));
    obs_repo.insert(h.context(), make_observation(ctx, s.id, "spot-1m", t2, 0.0402));

    // Curve-evolution view: one snapshot every 30 minutes, 3 buckets ending just after t2 --
    // bucket generation and the per-bucket as-of reduction both happen in the database.
    auto buckets = obs_repo.read_as_of_buckets(
        h.context(), s.id, t2 + std::chrono::seconds(1), std::chrono::minutes(30), 3);
    BOOST_LOG_SEV(lg, debug) << "Bucket snapshots: " << buckets;

    REQUIRE(buckets.size() == 3);
    REQUIRE(buckets[0].size() == 1);
    REQUIRE(buckets[1].size() == 1);
    REQUIRE(buckets[2].size() == 1);
    CHECK(buckets[0].front().value == "0.040000");
    CHECK(buckets[1].front().value == "0.040100");
    CHECK(buckets[2].front().value == "0.040200");
}

TEST_CASE("read_as_of_manual_point_is_readable_and_reports_manual", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    const auto t0 = std::chrono::system_clock::now();
    market_observation_repository obs_repo;
    ores::marketdata::repository::observation_lineage_repository lineage_repo;

    auto manual = make_observation(ctx, s.id, "SPOT-1M", t0, 0.0500);
    obs_repo.insert(h.context(), manual);
    const auto lineage = make_lineage(ctx, manual, "manual");
    lineage_repo.write(h.context(), lineage);

    const auto snapshot = obs_repo.read_as_of(h.context(), s.id, t0 + std::chrono::minutes(1));
    REQUIRE(snapshot.size() == 1);
    CHECK(snapshot.front().value == "0.050000");
    CHECK(snapshot.front().oresmd_uri == datum_uri("SPOT-1M"));

    // The annex is what reports the kind, and it names who keyed the point,
    // why, and the bitemporal instant it was keyed.
    const auto read = lineage_repo.read_latest_by_observation(
        h.context(), s.id, manual.observation_datetime, manual.oresmd_uri);
    REQUIRE(read.has_value());
    CHECK(read->point_source_kind == "manual");
    CHECK(read->modified_by == lineage.modified_by);
    CHECK(read->change_reason_code == "system.test");
    CHECK(read->change_commentary == "curve point provenance test");
    CHECK_FALSE(read->derivation_config_id.has_value());
    CHECK_FALSE(read->source_as_of.has_value());
    CHECK(read->source_series_ids == "[]");
}

TEST_CASE("read_as_of_derived_point_reports_derived", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    const auto t0 = std::chrono::system_clock::now();
    market_observation_repository obs_repo;
    ores::marketdata::repository::observation_lineage_repository lineage_repo;

    auto derived = make_observation(ctx, s.id, "SPOT-1M", t0, 0.0410);
    obs_repo.insert(h.context(), derived);
    lineage_repo.write(h.context(), make_lineage(ctx, derived, "derived"));

    const auto snapshot = obs_repo.read_as_of(h.context(), s.id, t0 + std::chrono::minutes(1));
    REQUIRE(snapshot.size() == 1);
    CHECK(snapshot.front().value == "0.041000");

    CHECK(reported_source_kind(lineage_repo, h.context(), s.id, derived) == "derived");
}

TEST_CASE("read_as_of_quoted_point_without_annex_row_reports_quoted", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    const auto t0 = std::chrono::system_clock::now();
    market_observation_repository obs_repo;
    ores::marketdata::repository::observation_lineage_repository lineage_repo;

    // A quoted tick: a hot observation row and nothing in the cold annex.
    auto quoted = make_observation(ctx, s.id, "SPOT-1M", t0, 0.0400);
    obs_repo.insert(h.context(), quoted);

    const auto snapshot = obs_repo.read_as_of(h.context(), s.id, t0 + std::chrono::minutes(1));
    REQUIRE(snapshot.size() == 1);
    CHECK(snapshot.front().value == "0.040000");

    CHECK_FALSE(
        lineage_repo
            .read_latest_by_observation(
                h.context(), s.id, quoted.observation_datetime, quoted.oresmd_uri)
            .has_value());
    CHECK(reported_source_kind(lineage_repo, h.context(), s.id, quoted) == "quoted");
}

TEST_CASE("read_as_of_manual_point_wins_over_a_later_feed_write", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    const auto t0 = std::chrono::system_clock::now();
    const auto keyed_at = t0 + std::chrono::minutes(10);
    const auto feed_after = t0 + std::chrono::minutes(20);

    market_observation_repository obs_repo;
    ores::marketdata::repository::observation_lineage_repository lineage_repo;

    obs_repo.insert(h.context(), make_observation(ctx, s.id, "SPOT-1M", t0, 0.0400));
    auto manual = make_observation(ctx, s.id, "SPOT-1M", keyed_at, 0.0500);
    obs_repo.insert(h.context(), manual);
    lineage_repo.write(h.context(), make_lineage(ctx, manual, "manual"));
    // The automatic write that would silently undo the manual point.
    obs_repo.insert(h.context(), make_observation(ctx, s.id, "SPOT-1M", feed_after, 0.0450));

    // Before the manual point was keyed, the fed value stands.
    const auto before =
        obs_repo.read_as_of(h.context(), s.id, t0 + std::chrono::minutes(5));
    REQUIRE(before.size() == 1);
    CHECK(before.front().value == "0.040000");

    // After it, the manual point owns the coordinate whatever the feed writes.
    const auto after = obs_repo.read_as_of(h.context(), s.id, t0 + std::chrono::minutes(30));
    REQUIRE(after.size() == 1);
    CHECK(after.front().value == "0.050000");
    CHECK(after.front().value != "0.045000");

    // The bucketed evolution agrees from the manual write onward, and the
    // earlier bucket keeps the fed value.
    const auto buckets = obs_repo.read_as_of_buckets(
        h.context(), s.id, t0 + std::chrono::minutes(30), std::chrono::minutes(10), 4);
    REQUIRE(buckets.size() == 4);
    REQUIRE(buckets[0].size() == 1);
    REQUIRE(buckets[1].size() == 1);
    REQUIRE(buckets[2].size() == 1);
    REQUIRE(buckets[3].size() == 1);
    CHECK(buckets[0].front().value == "0.040000");
    CHECK(buckets[1].front().value == "0.050000");
    CHECK(buckets[2].front().value == "0.050000");
    CHECK(buckets[3].front().value == "0.050000");
}

namespace {

// The session an operator writes a manual point with: the series' party, so
// the party-scoped annex row is visible, and the test database user as the
// actor the annex's modified_by must name.
ores::database::context operator_context(database_helper& h,
                                         const boost::uuids::uuid& party_id) {
    return h.context().with_party(h.tenant_id(), party_id, {party_id}, h.db_user());
}

}

TEST_CASE("write_manual_point_is_readable_and_reports_manual", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    const auto t0 = std::chrono::system_clock::now();
    const auto uri = datum_uri("SPOT-1M");

    market_observation_repository obs_repo;

    // The fed point the operator over-keys, at the same coordinate and instant.
    auto fed = make_observation(ctx, s.id, "SPOT-1M", t0, 0.0400);
    fed.party_id = s.party_id;
    obs_repo.insert(h.context(), fed);

    obs_repo.write_manual_point(
        operator_context(h, s.party_id), s.id, uri, t0, "0.050000", "system.test", "operator over-key");

    const auto snapshot = obs_repo.read_as_of(h.context(), s.id, t0 + std::chrono::minutes(1));
    REQUIRE(snapshot.size() == 1);
    CHECK(snapshot.front().value == "0.050000");
    CHECK(snapshot.front().oresmd_uri == uri);

    // The annex is what reports the kind, and it names who keyed the point and why.
    ores::marketdata::repository::observation_lineage_repository lineage_repo;
    const auto read = lineage_repo.read_latest_by_observation(h.context(), s.id, t0, uri);
    REQUIRE(read.has_value());
    CHECK(read->point_source_kind == "manual");
    CHECK(read->modified_by == h.db_user());
    CHECK(read->change_reason_code == "system.test");
    CHECK(read->change_commentary == "operator over-key");
    CHECK_FALSE(read->derivation_config_id.has_value());
    CHECK_FALSE(read->source_as_of.has_value());
    CHECK(read->source_series_ids == "[]");
}

TEST_CASE("clear_manual_point_restores_the_fed_value", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    const auto t0 = std::chrono::system_clock::now();
    const auto keyed_at = t0 + std::chrono::minutes(10);
    const auto feed_after = t0 + std::chrono::minutes(20);
    const auto uri = datum_uri("SPOT-1M");

    market_observation_repository obs_repo;
    auto fed = make_observation(ctx, s.id, "SPOT-1M", t0, 0.0400);
    fed.party_id = s.party_id;
    obs_repo.insert(h.context(), fed);

    obs_repo.write_manual_point(operator_context(h, s.party_id),
                                s.id,
                                uri,
                                keyed_at,
                                "0.050000",
                                "system.test",
                                "operator over-key");
    // The automatic write the manual point must still own the coordinate over.
    auto later = make_observation(ctx, s.id, "SPOT-1M", feed_after, 0.0450);
    later.party_id = s.party_id;
    obs_repo.insert(h.context(), later);

    const auto as_of = t0 + std::chrono::minutes(30);
    const auto before = obs_repo.read_as_of(h.context(), s.id, as_of);
    REQUIRE(before.size() == 1);
    CHECK(before.front().value == "0.050000");

    obs_repo.clear_manual_point(operator_context(h, s.party_id), s.id, uri, keyed_at);

    const auto after = obs_repo.read_as_of(h.context(), s.id, as_of);
    REQUIRE(after.size() == 1);
    CHECK(after.front().value == "0.045000");
    // The same read before and after clearing differs: this is the assertion
    // that fails when clear_manual_point does nothing.
    CHECK(after.front().value != before.front().value);
}

TEST_CASE("clear_manual_point_keeps_the_manual_row_in_history", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    const auto t0 = std::chrono::system_clock::now();
    const auto uri = datum_uri("SPOT-1M");

    market_observation_repository obs_repo;
    auto fed = make_observation(ctx, s.id, "SPOT-1M", t0, 0.0400);
    fed.party_id = s.party_id;
    obs_repo.insert(h.context(), fed);

    obs_repo.write_manual_point(
        operator_context(h, s.party_id), s.id, uri, t0, "0.050000", "system.test", "operator over-key");

    ores::marketdata::repository::observation_lineage_repository lineage_repo;
    const auto manual = lineage_repo.read_latest_by_observation(h.context(), s.id, t0, uri);
    REQUIRE(manual.has_value());
    REQUIRE(manual->point_source_kind == "manual");
    const auto manual_id = boost::uuids::to_string(manual->id);

    obs_repo.clear_manual_point(operator_context(h, s.party_id), s.id, uri, t0);

    // No current manual row remains: the coordinate is back to the feed.
    CHECK_FALSE(
        lineage_repo.read_latest_by_observation(h.context(), s.id, t0, uri).has_value());

    // The row itself is closed, not deleted, so the history still holds it.
    const auto history = lineage_repo.read_all(h.context(), manual_id);
    REQUIRE_FALSE(history.empty());
    CHECK(history.front().point_source_kind == "manual");
    CHECK(history.front().change_commentary == "operator over-key");
}

TEST_CASE("write_manual_point_leaves_neither_row_when_the_annex_write_fails", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    const auto t0 = std::chrono::system_clock::now();
    const auto uri = datum_uri("SPOT-1M");

    market_observation_repository obs_repo;
    auto fed = make_observation(ctx, s.id, "SPOT-1M", t0, 0.0400);
    fed.party_id = s.party_id;
    obs_repo.insert(h.context(), fed);

    // The annex write is refused by the store (the reason code does not exist),
    // after the observation row has been written in the same transaction. The
    // rollback must undo the observation, so the fed value is untouched.
    CHECK_THROWS(obs_repo.write_manual_point(operator_context(h, s.party_id),
                                             s.id,
                                             uri,
                                             t0,
                                             "0.050000",
                                             "no.such.reason",
                                             "refused annex write"));

    const auto snapshot = obs_repo.read_as_of(h.context(), s.id, t0 + std::chrono::minutes(1));
    REQUIRE(snapshot.size() == 1);
    CHECK(snapshot.front().value == "0.040000");

    ores::marketdata::repository::observation_lineage_repository lineage_repo;
    CHECK_FALSE(lineage_repo.read_latest_by_observation(h.context(), s.id, t0, uri).has_value());
}
