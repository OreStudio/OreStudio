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
#include "ores.marketdata.api/messaging/market_observation_protocol.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/service/market_observation_service.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstddef>
#include <string>
#include <vector>

using namespace ores::logging;

namespace {

using ores::marketdata::domain::market_observation;
using ores::marketdata::messaging::market_observation_change;
using ores::marketdata::messaging::market_observation_key;
using ores::marketdata::messaging::market_observation_write;
using ores::utility::domain::outcome;

market_observation_change change_for(const market_observation& o) {
    market_observation_change c;
    c.write = market_observation_write{.id = o.id,
                                       .party_id = o.party_id,
                                       .series_id = o.series_id,
                                       .observation_datetime = o.observation_datetime,
                                       .oresmd_uri = o.oresmd_uri,
                                       .value = o.value,
                                       .source = o.source,
                                       .feed_id = o.feed_id};
    return c;
}

std::vector<market_observation> observations_of_a_new_series(ores::testing::database_helper& h,
                                                             std::size_t n) {
    auto ctx = ores::testing::make_generation_context(h);
    const auto s = ores::marketdata::generators::generate_synthetic_market_series(ctx);
    ores::marketdata::repository::market_series_repository{}.write(h.context(), s);
    auto observations =
        ores::marketdata::generators::generate_synthetic_market_observations(n, ctx);
    for (auto& o : observations)
        o.series_id = s.id;
    return observations;
}

}

TEST_CASE("put_many_across_read_chunks_answers_every_row_in_order", "[service][batch]") {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::database_helper h;
    ores::marketdata::service::market_observation_service svc(h.context());
    const auto observations = observations_of_a_new_series(h, 1001);

    ores::marketdata::messaging::put_many_market_observations_request put;
    for (const auto& o : observations)
        put.changes.push_back(change_for(o));
    BOOST_LOG_SEV(lg, info) << "Putting " << put.changes.size() << " market observations.";
    const auto written = svc.put_many_market_observations(put);

    REQUIRE(written.result.outcome == outcome::ok);
    REQUIRE(written.market_observations.size() == observations.size());
    for (std::size_t i = 0; i < observations.size(); ++i)
        CHECK(written.market_observations[i].id == observations[i].id);

    ores::marketdata::messaging::get_many_market_observations_request get;
    const auto missing = boost::uuids::random_generator()();
    get.keys.push_back(market_observation_key{.id = missing});
    for (auto o = observations.rbegin(); get.keys.size() < 1000; ++o)
        get.keys.push_back(market_observation_key{.id = o->id});
    const auto read = svc.get_many_market_observations(get);

    REQUIRE(read.entries.size() == get.keys.size());
    CHECK(!read.entries.front().market_observation);
    for (std::size_t i = 1; i < read.entries.size(); ++i) {
        REQUIRE(read.entries[i].market_observation);
        CHECK(read.entries[i].market_observation->id == get.keys[i].id);
    }
}

TEST_CASE("put_many_with_an_existing_row_past_the_first_read_chunk_refuses_the_set",
          "[service][batch]") {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::database_helper h;
    ores::marketdata::service::market_observation_service svc(h.context());
    const auto observations = observations_of_a_new_series(h, 1001);

    ores::marketdata::messaging::put_many_market_observations_request first;
    first.changes.push_back(change_for(observations.back()));
    REQUIRE(svc.put_many_market_observations(first).result.outcome == outcome::ok);

    ores::marketdata::messaging::put_many_market_observations_request put;
    for (const auto& o : observations)
        put.changes.push_back(change_for(o));
    BOOST_LOG_SEV(lg, info) << "Putting " << put.changes.size()
                            << " market observations, one of which exists.";
    const auto refused = svc.put_many_market_observations(put);

    CHECK(refused.result.outcome != outcome::ok);
    CHECK(refused.result.code == "already_exists");
    CHECK(refused.market_observations.empty());

    ores::marketdata::messaging::get_many_market_observations_request get;
    for (std::size_t i = 0; i + 1 < observations.size(); ++i)
        get.keys.push_back(market_observation_key{.id = observations[i].id});
    const auto read = svc.get_many_market_observations(get);

    REQUIRE(read.entries.size() == get.keys.size());
    for (const auto& entry : read.entries)
        CHECK(!entry.market_observation);
}

TEST_CASE("get_many_of_more_than_1000_keys_is_refused", "[service][batch]") {
    auto lg(ores::logging::make_logger(test_suite));
    ores::testing::database_helper h;
    ores::marketdata::service::market_observation_service svc(h.context());

    ores::marketdata::messaging::get_many_market_observations_request get;
    boost::uuids::random_generator ids;
    for (std::size_t i = 0; i < 1001; ++i)
        get.keys.push_back(market_observation_key{.id = ids()});
    BOOST_LOG_SEV(lg, info) << "Getting " << get.keys.size() << " market observations.";
    const auto refused = svc.get_many_market_observations(get);

    CHECK(refused.result.outcome != outcome::ok);
    CHECK(refused.result.code == "filter_too_large");
    CHECK(refused.entries.empty());
}

// Hidden from the suite: it times a write rather than checking one. Run it by
// its tag, "[.benchmark]", to measure a batch put of the size the derived
// sources write.
TEST_CASE("put_many_of_one_thousand_market_observations_timed", "[.benchmark]") {
    constexpr std::size_t batch_size = 1000;
    constexpr int batches = 5;

    ores::testing::database_helper h;
    ores::marketdata::service::market_observation_service svc(h.context());

    for (int b = 0; b < batches; ++b) {
        const auto observations = observations_of_a_new_series(h, batch_size);
        ores::marketdata::messaging::put_many_market_observations_request put;
        for (const auto& o : observations)
            put.changes.push_back(change_for(o));

        const auto started = std::chrono::steady_clock::now();
        const auto written = svc.put_many_market_observations(put);
        const auto seconds =
            std::chrono::duration<double>(std::chrono::steady_clock::now() - started).count();

        REQUIRE(written.result.outcome == outcome::ok);
        ores::marketdata::messaging::get_many_market_observations_request get;
        for (const auto& o : observations)
            get.keys.push_back(market_observation_key{.id = o.id});
        std::size_t stored = 0;
        for (const auto& entry : svc.get_many_market_observations(get).entries)
            stored += entry.market_observation ? 1 : 0;
        REQUIRE(stored == batch_size);
        WARN("batch " + std::to_string(b) + ": " + std::to_string(batch_size) + " rows put in " +
             std::to_string(seconds) + " s");
    }
}
