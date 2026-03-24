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
#include "ores.trading.core/repository/activity_type_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.trading.api/domain/activity_type.hpp"
#include "ores.trading.api/domain/activity_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/generators/activity_type_generator.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"

namespace {

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::trading::generator;

using ores::testing::database_helper;
using ores::trading::repository::activity_type_repository;

TEST_CASE("write_single_activity_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    activity_type_repository repo;
    auto at = generate_synthetic_activity_type(ctx);

    BOOST_LOG_SEV(lg, debug) << "Activity type: " << at;
    CHECK_NOTHROW(repo.write(h.context(), at));
}

TEST_CASE("write_multiple_activity_types", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    activity_type_repository repo;
    auto activity_types = generate_synthetic_activity_types(5, ctx);
    BOOST_LOG_SEV(lg, debug) << "Activity types: " << activity_types;

    CHECK_NOTHROW(repo.write(h.context(), activity_types));
}

TEST_CASE("read_latest_activity_types", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    activity_type_repository repo;
    auto written = generate_synthetic_activity_types(3, ctx);
    BOOST_LOG_SEV(lg, debug) << "Written activity types: " << written;
    repo.write(h.context(), written);

    auto read = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read activity types: " << read;

    CHECK(!read.empty());
    CHECK(read.size() >= written.size());
}

TEST_CASE("read_latest_activity_type_by_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    activity_type_repository repo;
    auto activity_types = generate_synthetic_activity_types(5, ctx);
    const auto target = activity_types.front();
    BOOST_LOG_SEV(lg, debug) << "Written activity types: " << activity_types;
    repo.write(h.context(), activity_types);

    auto read = repo.read_latest(h.context(), target.code);
    BOOST_LOG_SEV(lg, debug) << "Read activity types: " << read;

    REQUIRE(read.size() == 1);
    CHECK(read[0].code == target.code);
    CHECK(read[0].description == target.description);
}

TEST_CASE("read_all_versions_of_activity_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    activity_type_repository repo;
    auto at = generate_synthetic_activity_type(ctx);
    repo.write(h.context(), at);

    at.version = 1;
    at.description = "updated description";
    repo.write(h.context(), at);

    auto all = repo.read_all(h.context(), at.code);
    BOOST_LOG_SEV(lg, debug) << "All versions: " << all;

    CHECK(all.size() >= 2);
}

TEST_CASE("remove_activity_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    activity_type_repository repo;
    auto at = generate_synthetic_activity_type(ctx);
    repo.write(h.context(), at);

    auto before = repo.read_latest(h.context(), at.code);
    REQUIRE(!before.empty());

    CHECK_NOTHROW(repo.remove(h.context(), at.code));

    auto after = repo.read_latest(h.context(), at.code);
    BOOST_LOG_SEV(lg, debug) << "After remove count: " << after.size();
    CHECK(after.empty());
}

TEST_CASE("read_nonexistent_activity_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    activity_type_repository repo;
    const std::string nonexistent = "NONEXISTENT_ACTIVITY_TYPE_XYZ_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent;

    auto read = repo.read_latest(h.context(), nonexistent);
    BOOST_LOG_SEV(lg, debug) << "Read activity types: " << read;

    CHECK(read.empty());
}
