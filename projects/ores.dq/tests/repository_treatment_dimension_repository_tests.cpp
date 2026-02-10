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
#include "ores.dq/repository/treatment_dimension_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/treatment_dimension_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/generators/treatment_dimension_generator.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::dq::generators;

using ores::testing::database_helper;
using ores::dq::repository::treatment_dimension_repository;

TEST_CASE("write_single_treatment_dimension", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    treatment_dimension_repository repo(h.context());
    auto treatment_dimension = generate_synthetic_treatment_dimension();
    treatment_dimension.tenant_id = h.tenant_id().to_string();
    treatment_dimension.code = treatment_dimension.code + "_" + std::string(faker::string::alphanumeric(8));

    BOOST_LOG_SEV(lg, debug) << "Treatment dimension: " << treatment_dimension;
    CHECK_NOTHROW(repo.write(treatment_dimension));
}

TEST_CASE("write_multiple_treatment_dimensions", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    treatment_dimension_repository repo(h.context());
    auto treatment_dimensions = generate_synthetic_treatment_dimensions(3);
    for (auto& t : treatment_dimensions) {
        t.tenant_id = h.tenant_id().to_string();
        t.code = t.code + "_" + std::string(faker::string::alphanumeric(8));
    }
    BOOST_LOG_SEV(lg, debug) << "Treatment dimensions: " << treatment_dimensions;

    CHECK_NOTHROW(repo.write(treatment_dimensions));
}

TEST_CASE("read_latest_treatment_dimensions", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    treatment_dimension_repository repo(h.context());
    auto written_treatment_dimensions = generate_synthetic_treatment_dimensions(3);
    for (auto& t : written_treatment_dimensions) {
        t.tenant_id = h.tenant_id().to_string();
        t.code = t.code + "_" + std::string(faker::string::alphanumeric(8));
    }
    BOOST_LOG_SEV(lg, debug) << "Written treatment dimensions: " << written_treatment_dimensions;

    repo.write(written_treatment_dimensions);

    auto read_treatment_dimensions = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read treatment dimensions: " << read_treatment_dimensions;

    CHECK(!read_treatment_dimensions.empty());
    CHECK(read_treatment_dimensions.size() >= written_treatment_dimensions.size());
}

TEST_CASE("read_latest_treatment_dimension_by_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    treatment_dimension_repository repo(h.context());
    auto treatment_dimensions = generate_synthetic_treatment_dimensions(3);
    for (auto& t : treatment_dimensions) {
        t.tenant_id = h.tenant_id().to_string();
        t.code = t.code + "_" + std::string(faker::string::alphanumeric(8));
    }

    const auto target = treatment_dimensions.front();
    BOOST_LOG_SEV(lg, debug) << "Write treatment dimensions: " << treatment_dimensions;
    repo.write(treatment_dimensions);

    BOOST_LOG_SEV(lg, debug) << "Target treatment dimension: " << target;

    auto read_treatment_dimensions = repo.read_latest(target.code);
    BOOST_LOG_SEV(lg, debug) << "Read treatment dimensions: " << read_treatment_dimensions;

    REQUIRE(read_treatment_dimensions.size() == 1);
    CHECK(read_treatment_dimensions[0].code == target.code);
    CHECK(read_treatment_dimensions[0].name == target.name);
}
