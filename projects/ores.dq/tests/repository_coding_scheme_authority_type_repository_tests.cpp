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
#include "ores.dq/repository/coding_scheme_authority_type_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/coding_scheme_authority_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/generators/coding_scheme_authority_type_generator.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::dq::generators;

using ores::testing::database_helper;
using ores::dq::repository::coding_scheme_authority_type_repository;

TEST_CASE("write_single_coding_scheme_authority_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    coding_scheme_authority_type_repository repo(h.context());
    auto authority_type = generate_synthetic_coding_scheme_authority_type();
    authority_type.tenant_id = h.tenant_id().to_string();
    authority_type.code = authority_type.code + "_" + std::string(faker::string::alphanumeric(8));

    BOOST_LOG_SEV(lg, debug) << "Coding scheme authority type: " << authority_type;
    CHECK_NOTHROW(repo.write(authority_type));
}

TEST_CASE("write_multiple_coding_scheme_authority_types", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    coding_scheme_authority_type_repository repo(h.context());
    auto authority_types = generate_synthetic_coding_scheme_authority_types(3);
    for (auto& a : authority_types) {
        a.tenant_id = h.tenant_id().to_string();
        a.code = a.code + "_" + std::string(faker::string::alphanumeric(8));
    }
    BOOST_LOG_SEV(lg, debug) << "Coding scheme authority types: " << authority_types;

    CHECK_NOTHROW(repo.write(authority_types));
}

TEST_CASE("read_latest_coding_scheme_authority_types", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    coding_scheme_authority_type_repository repo(h.context());
    auto written_authority_types = generate_synthetic_coding_scheme_authority_types(3);
    for (auto& a : written_authority_types) {
        a.tenant_id = h.tenant_id().to_string();
        a.code = a.code + "_" + std::string(faker::string::alphanumeric(8));
    }
    BOOST_LOG_SEV(lg, debug) << "Written authority types: " << written_authority_types;

    repo.write(written_authority_types);

    auto read_authority_types = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read authority types: " << read_authority_types;

    CHECK(!read_authority_types.empty());
    CHECK(read_authority_types.size() >= written_authority_types.size());
}

TEST_CASE("read_latest_coding_scheme_authority_type_by_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    coding_scheme_authority_type_repository repo(h.context());
    auto authority_types = generate_synthetic_coding_scheme_authority_types(3);
    for (auto& a : authority_types) {
        a.tenant_id = h.tenant_id().to_string();
        a.code = a.code + "_" + std::string(faker::string::alphanumeric(8));
    }

    const auto target = authority_types.front();
    BOOST_LOG_SEV(lg, debug) << "Write authority types: " << authority_types;
    repo.write(authority_types);

    BOOST_LOG_SEV(lg, debug) << "Target authority type: " << target;

    auto read_authority_types = repo.read_latest(target.code);
    BOOST_LOG_SEV(lg, debug) << "Read authority types: " << read_authority_types;

    REQUIRE(read_authority_types.size() == 1);
    CHECK(read_authority_types[0].code == target.code);
    CHECK(read_authority_types[0].name == target.name);
}
