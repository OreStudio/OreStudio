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
#include "ores.analytics.core/repository/pricing_engine_type_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.analytics.api/domain/pricing_engine_type.hpp" // IWYU pragma: keep.
#include "ores.analytics.api/domain/pricing_engine_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.analytics.api/generators/pricing_engine_type_generator.hpp"

namespace {

const std::string_view test_suite("ores.analytics.tests");
const std::string tags("[repository]");

}

using namespace ores::analytics::generators;
using ores::analytics::domain::pricing_engine_type;
using ores::analytics::repository::pricing_engine_type_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_pricing_engine_type", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto types = generate_fictional_pricing_engine_types(1, ctx);
    REQUIRE(!types.empty());
    auto t = types[0];
    BOOST_LOG_SEV(lg, debug) << "Pricing engine type: " << t;

    pricing_engine_type_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), t));
}

TEST_CASE("write_multiple_pricing_engine_types", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto types = generate_fictional_pricing_engine_types(3, ctx);
    BOOST_LOG_SEV(lg, debug) << "Pricing engine types: " << types;

    pricing_engine_type_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), types));
}

TEST_CASE("read_latest_pricing_engine_types", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto written = generate_fictional_pricing_engine_types(3, ctx);
    BOOST_LOG_SEV(lg, debug) << "Written types: " << written;

    pricing_engine_type_repository repo;
    repo.write(h.context(), written);

    auto read = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read types: " << read;

    CHECK(read.size() >= written.size());
}

TEST_CASE("read_latest_pricing_engine_type_by_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto types = generate_fictional_pricing_engine_types(1, ctx);
    REQUIRE(!types.empty());
    auto t = types[0];
    const auto original_description = t.description;
    BOOST_LOG_SEV(lg, debug) << "Pricing engine type: " << t;

    pricing_engine_type_repository repo;
    repo.write(h.context(), t);

    t.description = original_description + " v2";
    repo.write(h.context(), t);

    auto read = repo.read_latest(h.context(), t.code);
    BOOST_LOG_SEV(lg, debug) << "Read types: " << read;

    REQUIRE(read.size() == 1);
    CHECK(read[0].code == t.code);
    CHECK(read[0].description == original_description + " v2");
}

TEST_CASE("read_nonexistent_pricing_engine_type_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    pricing_engine_type_repository repo;

    const std::string nonexistent_code = "NONEXISTENT_ENGINE_TYPE_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent_code;

    auto read = repo.read_latest(h.context(), nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read types: " << read;

    CHECK(read.size() == 0);
}

TEST_CASE("read_all_pricing_engine_type_history", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto types = generate_fictional_pricing_engine_types(1, ctx);
    REQUIRE(!types.empty());
    auto t = types[0];
    BOOST_LOG_SEV(lg, debug) << "Pricing engine type: " << t;

    pricing_engine_type_repository repo;
    repo.write(h.context(), t);

    t.description = t.description + " v2";
    repo.write(h.context(), t);

    auto history = repo.read_all(h.context(), t.code);
    BOOST_LOG_SEV(lg, debug) << "History: " << history;

    CHECK(history.size() >= 2);
}

TEST_CASE("remove_pricing_engine_type", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto types = generate_fictional_pricing_engine_types(1, ctx);
    REQUIRE(!types.empty());
    auto t = types[0];
    BOOST_LOG_SEV(lg, debug) << "Pricing engine type: " << t;

    pricing_engine_type_repository repo;
    repo.write(h.context(), t);

    CHECK_NOTHROW(repo.remove(h.context(), t.code));

    auto read = repo.read_latest(h.context(), t.code);
    CHECK(read.empty());
}
