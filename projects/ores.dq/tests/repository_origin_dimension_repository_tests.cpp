/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.dq/repository/origin_dimension_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/origin_dimension_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/generators/origin_dimension_generator.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::dq::generators;

using ores::testing::database_helper;
using ores::dq::repository::origin_dimension_repository;

TEST_CASE("write_single_origin_dimension", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    origin_dimension_repository repo(h.context());
    auto origin_dimension = generate_synthetic_origin_dimension();

    BOOST_LOG_SEV(lg, debug) << "Origin dimension: " << origin_dimension;
    CHECK_NOTHROW(repo.write(origin_dimension));
}

TEST_CASE("write_multiple_origin_dimensions", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    origin_dimension_repository repo(h.context());
    auto origin_dimensions = generate_synthetic_origin_dimensions(3);
    BOOST_LOG_SEV(lg, debug) << "Origin dimensions: " << origin_dimensions;

    CHECK_NOTHROW(repo.write(origin_dimensions));
}

TEST_CASE("read_latest_origin_dimensions", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    origin_dimension_repository repo(h.context());
    auto written_origin_dimensions = generate_synthetic_origin_dimensions(3);
    BOOST_LOG_SEV(lg, debug) << "Written origin dimensions: " << written_origin_dimensions;

    repo.write(written_origin_dimensions);

    auto read_origin_dimensions = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read origin dimensions: " << read_origin_dimensions;

    CHECK(!read_origin_dimensions.empty());
    CHECK(read_origin_dimensions.size() >= written_origin_dimensions.size());
}

TEST_CASE("read_latest_origin_dimension_by_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    origin_dimension_repository repo(h.context());
    auto origin_dimensions = generate_synthetic_origin_dimensions(3);

    const auto target = origin_dimensions.front();
    BOOST_LOG_SEV(lg, debug) << "Write origin dimensions: " << origin_dimensions;
    repo.write(origin_dimensions);

    BOOST_LOG_SEV(lg, debug) << "Target origin dimension: " << target;

    auto read_origin_dimensions = repo.read_latest(target.code);
    BOOST_LOG_SEV(lg, debug) << "Read origin dimensions: " << read_origin_dimensions;

    REQUIRE(read_origin_dimensions.size() == 1);
    CHECK(read_origin_dimensions[0].code == target.code);
    CHECK(read_origin_dimensions[0].name == target.name);
}
