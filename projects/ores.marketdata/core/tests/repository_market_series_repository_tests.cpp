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
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/market_series_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/random_generator.hpp>
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

// A series built for the identity cases rather than generated: they need two rows
// that share an identity and nothing else, and a generated triple can collide with
// another case's row.
ores::marketdata::domain::market_series make_identity_test_series(database_helper& h,
                                                                  const boost::uuids::uuid& party) {
    ores::marketdata::domain::market_series s;
    s.id = boost::uuids::random_generator{}();
    s.version = 0;
    s.tenant_id = h.tenant_id();
    s.party_id = party;
    s.series_type = "IDENTITY_TEST";
    s.metric = "RATE";
    s.qualifier = "USD/TEST";
    s.oresmd_uri = std::string("oresmd://generic/identity-test-") + boost::uuids::to_string(s.id) +
                   "?type=fixing";
    s.series_subclass = "yield";
    s.derivation_kind = "OBSERVED";
    s.derivation_config_id = boost::uuids::nil_uuid();
    s.derivation_config_version = 0;
    s.modified_by = h.db_user();
    s.performed_by = h.db_user();
    s.change_reason_code = "system.test";
    s.change_commentary = "repository identity test";
    return s;
}


TEST_CASE("write_single_market_series", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    auto s = generate_synthetic_market_series(ctx);

    BOOST_LOG_SEV(lg, debug) << "Market series: " << s;
    CHECK_NOTHROW(repo.write(h.context(), s));
}

TEST_CASE("write_multiple_market_series", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    auto series = generate_synthetic_market_series(5, ctx);
    BOOST_LOG_SEV(lg, debug) << "Market series: " << series;

    CHECK_NOTHROW(repo.write(h.context(), series));
}

TEST_CASE("read_latest_market_series", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    auto written = generate_synthetic_market_series(3, ctx);
    BOOST_LOG_SEV(lg, debug) << "Written market series: " << written;
    repo.write(h.context(), written);

    auto read = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read market series: " << read;

    CHECK(!read.empty());
    CHECK(read.size() >= written.size());
}

TEST_CASE("read_latest_market_series_by_id", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    auto series = generate_synthetic_market_series(3, ctx);
    const auto target = series.front();
    repo.write(h.context(), series);

    auto read = repo.read_latest(h.context(), boost::uuids::to_string(target.id));
    BOOST_LOG_SEV(lg, debug) << "Read market series: " << read;

    REQUIRE(read.size() == 1);
    CHECK(read[0].qualifier == target.qualifier);
    CHECK(read[0].series_type == target.series_type);
}

TEST_CASE("read_latest_market_series_by_identity", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    auto s = generate_synthetic_market_series(ctx);
    repo.write(h.context(), s);

    // Scoped to the row's own party: the identity is unique per party, so one
    // identity can name a series in each of several parties.
    auto read =
        repo.read_latest_by_uri(h.context(), s.oresmd_uri, boost::uuids::to_string(s.party_id));
    BOOST_LOG_SEV(lg, debug) << "Read by identity: " << read;

    REQUIRE(read.size() == 1);
    CHECK(read[0].oresmd_uri == s.oresmd_uri);
}

TEST_CASE("read_all_versions_of_market_series", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    auto s = generate_synthetic_market_series(ctx);
    repo.write(h.context(), s);

    s.change_commentary = "second version";
    repo.write(h.context(), s);

    auto all = repo.read_all(h.context(), boost::uuids::to_string(s.id));
    BOOST_LOG_SEV(lg, debug) << "All versions: " << all;

    CHECK(all.size() >= 2);
}

TEST_CASE("remove_market_series", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    auto s = generate_synthetic_market_series(ctx);
    repo.write(h.context(), s);

    auto before = repo.read_latest(h.context(), boost::uuids::to_string(s.id));
    REQUIRE(!before.empty());

    CHECK_NOTHROW(repo.remove(h.context(), boost::uuids::to_string(s.id)));

    auto after = repo.read_latest(h.context(), boost::uuids::to_string(s.id));
    BOOST_LOG_SEV(lg, debug) << "After remove count: " << after.size();
    CHECK(after.empty());
}

TEST_CASE("two_series_in_one_party_cannot_share_an_identity", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party = boost::uuids::random_generator{}();

    market_series_repository repo;
    const auto first = make_identity_test_series(h, party);
    repo.write(h.context(), first);

    // The same party and the same identity, with a different decomposition and a
    // different id: the identity is what the row is keyed by, so it is refused.
    auto second = first;
    second.id = boost::uuids::random_generator{}();
    second.series_type = first.series_type + "-other";
    second.metric = first.metric + "-other";
    second.qualifier = first.qualifier + "-other";

    CHECK_THROWS(repo.write(h.context(), second));
}

TEST_CASE("two_parties_may_each_hold_one_identity", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party = boost::uuids::random_generator{}();

    market_series_repository repo;
    const auto first = make_identity_test_series(h, party);
    repo.write(h.context(), first);

    // The identity is unique per party, and a market series belongs to one party,
    // so a second party's row of the same identity is a series of its own.
    auto second = first;
    second.id = boost::uuids::random_generator{}();
    second.party_id = boost::uuids::random_generator{}();

    CHECK_NOTHROW(repo.write(h.context(), second));
}
