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
#include "ores.refdata.core/repository/rounding_type_repository.hpp"

#include <set>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/domain/rounding_type.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/domain/rounding_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/rounding_type_generator.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::repository::rounding_type_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_rounding_type", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto rt = generate_synthetic_rounding_type(ctx);
    rt.code = rt.code + "_" + std::string(faker::string::alphanumeric(8));
    rt.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Rounding type: " << rt;

    rounding_type_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), rt));
}

TEST_CASE("read_latest_rounding_types_no_duplicate_codes", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    rounding_type_repository repo;

    // Regression: reads must be tenant-filtered. Without the filter the
    // system-tenant seed rows appear alongside the tenant's provisioned
    // copies, duplicating every code in the UI.
    auto read_rounding_types = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read rounding types: " << read_rounding_types;

    std::set<std::string> codes;
    for (const auto& rt : read_rounding_types)
        codes.insert(rt.code);
    CHECK(codes.size() == read_rounding_types.size());
}

TEST_CASE("read_latest_rounding_type_by_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto rt = generate_synthetic_rounding_type(ctx);
    rt.code = rt.code + "_" + std::string(faker::string::alphanumeric(8));
    rt.change_reason_code = "system.test";
    const auto original_name = rt.name;
    BOOST_LOG_SEV(lg, debug) << "Rounding type: " << rt;

    rounding_type_repository repo;
    repo.write(h.context(), rt);

    rt.name = original_name + " v2";
    repo.write(h.context(), rt);

    auto read_rounding_types = repo.read_latest(h.context(), rt.code);
    BOOST_LOG_SEV(lg, debug) << "Read rounding types: " << read_rounding_types;

    REQUIRE(read_rounding_types.size() == 1);
    CHECK(read_rounding_types[0].code == rt.code);
    CHECK(read_rounding_types[0].name == original_name + " v2");
}

TEST_CASE("read_nonexistent_rounding_type_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    rounding_type_repository repo;

    const std::string nonexistent_code = "NONEXISTENT_CODE_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent_code;

    auto read_rounding_types =
        repo.read_latest(h.context(), nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read rounding types: " << read_rounding_types;

    CHECK(read_rounding_types.size() == 0);
}
