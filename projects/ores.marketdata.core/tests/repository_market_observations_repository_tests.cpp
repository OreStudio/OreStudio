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
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/domain/market_observation_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.api/generators/market_observation_generator.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"

namespace {

const std::string_view test_suite("ores.marketdata.core.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::marketdata::generator;

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
    auto obs = generate_synthetic_market_observation(s.id, ctx);
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
    auto observations = generate_synthetic_market_observations(5, s.id, ctx);
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
    auto written = generate_synthetic_market_observations(3, s.id, ctx);
    obs_repo.write(h.context(), written);

    auto read = obs_repo.read_latest(h.context(), s.id);
    BOOST_LOG_SEV(lg, debug) << "Read observations: " << read;

    CHECK(read.size() == written.size());
    for (const auto& r : read)
        CHECK(r.series_id == s.id);
}

TEST_CASE("remove_market_observations", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository series_repo;
    auto s = generate_synthetic_market_series(ctx);
    series_repo.write(h.context(), s);

    market_observations_repository obs_repo;
    auto observations = generate_synthetic_market_observations(3, s.id, ctx);
    obs_repo.write(h.context(), observations);

    auto before = obs_repo.read_latest(h.context(), s.id);
    REQUIRE(!before.empty());

    CHECK_NOTHROW(obs_repo.remove(h.context(), s.id));

    auto after = obs_repo.read_latest(h.context(), s.id);
    BOOST_LOG_SEV(lg, debug) << "After remove count: " << after.size();
    CHECK(after.empty());
}
