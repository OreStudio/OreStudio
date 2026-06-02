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
#include "ores.marketdata.core/repository/market_series_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/market_series_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.api/generators/market_series_generator.hpp"
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

    auto read = repo.read_latest(h.context(), target.id);
    BOOST_LOG_SEV(lg, debug) << "Read market series: " << read;

    REQUIRE(read.size() == 1);
    CHECK(read[0].qualifier == target.qualifier);
    CHECK(read[0].series_type == target.series_type);
}

TEST_CASE("read_latest_market_series_by_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    market_series_repository repo;
    auto s = generate_synthetic_market_series(ctx);
    repo.write(h.context(), s);

    auto read = repo.read_latest_by_type(h.context(),
        s.series_type, s.metric, s.qualifier);
    BOOST_LOG_SEV(lg, debug) << "Read by type: " << read;

    REQUIRE(read.size() == 1);
    CHECK(read[0].qualifier == s.qualifier);
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

    auto all = repo.read_all(h.context(), s.id);
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

    auto before = repo.read_latest(h.context(), s.id);
    REQUIRE(!before.empty());

    CHECK_NOTHROW(repo.remove(h.context(), s.id));

    auto after = repo.read_latest(h.context(), s.id);
    BOOST_LOG_SEV(lg, debug) << "After remove count: " << after.size();
    CHECK(after.empty());
}
