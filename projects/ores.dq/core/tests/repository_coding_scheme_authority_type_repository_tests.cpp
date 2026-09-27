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
#include "ores.dq.api/domain/coding_scheme_authority_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.api/generators/coding_scheme_authority_type_generator.hpp"
#include "ores.dq.core/repository/coding_scheme_authority_type_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::dq::generators;

using ores::testing::database_helper;
using ores::dq::repository::coding_scheme_authority_type_repository;
using ores::utility::generation::generation_context;

TEST_CASE("write_single_coding_scheme_authority_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    generation_context ctx;
    coding_scheme_authority_type_repository repo;
    auto authority_type = generate_synthetic_coding_scheme_authority_type(ctx);
    authority_type.tenant_id = h.tenant_id();
    authority_type.code = authority_type.code + "_" + std::string(faker::string::alphanumeric(8));

    BOOST_LOG_SEV(lg, debug) << "Coding scheme authority type: " << authority_type;
    repo.write(h.context(), authority_type);

    auto read_authority_types = repo.read_latest(h.context(), authority_type.code);
    BOOST_LOG_SEV(lg, debug) << "Read authority types: " << read_authority_types;

    REQUIRE(read_authority_types.size() == 1);
    CHECK(read_authority_types[0].code == authority_type.code);
    CHECK(read_authority_types[0].name == authority_type.name);
}

TEST_CASE("write_multiple_coding_scheme_authority_types", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    coding_scheme_authority_type_repository repo;
    generation_context ctx;
    auto authority_types = generate_synthetic_coding_scheme_authority_types(3, ctx);
    for (auto& a : authority_types) {
        a.tenant_id = h.tenant_id();
        a.code = a.code + "_" + std::string(faker::string::alphanumeric(8));
    }
    BOOST_LOG_SEV(lg, debug) << "Coding scheme authority types: " << authority_types;
    repo.write(h.context(), authority_types);

    auto read_authority_types = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read authority types: " << read_authority_types;

    for (const auto& written : authority_types) {
        bool found = false;
        for (const auto& read_row : read_authority_types) {
            if (read_row.code == written.code) {
                found = true;
                CHECK(read_row.name == written.name);
            }
        }
        CHECK(found);
    }
}

TEST_CASE("read_latest_coding_scheme_authority_types", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    coding_scheme_authority_type_repository repo;
    generation_context ctx;
    auto written_authority_types = generate_synthetic_coding_scheme_authority_types(3, ctx);
    for (auto& a : written_authority_types) {
        a.tenant_id = h.tenant_id();
        a.code = a.code + "_" + std::string(faker::string::alphanumeric(8));
    }
    BOOST_LOG_SEV(lg, debug) << "Written authority types: " << written_authority_types;

    repo.write(h.context(), written_authority_types);

    auto read_authority_types = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read authority types: " << read_authority_types;

    for (const auto& written : written_authority_types) {
        bool found = false;
        for (const auto& read_row : read_authority_types) {
            if (read_row.code == written.code) {
                found = true;
                CHECK(read_row.name == written.name);
            }
        }
        CHECK(found);
    }
}

TEST_CASE("read_latest_coding_scheme_authority_type_by_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    coding_scheme_authority_type_repository repo;
    generation_context ctx;
    auto authority_types = generate_synthetic_coding_scheme_authority_types(3, ctx);
    for (auto& a : authority_types) {
        a.tenant_id = h.tenant_id();
        a.code = a.code + "_" + std::string(faker::string::alphanumeric(8));
    }

    const auto target = authority_types.front();
    BOOST_LOG_SEV(lg, debug) << "Write authority types: " << authority_types;
    repo.write(h.context(), authority_types);

    BOOST_LOG_SEV(lg, debug) << "Target authority type: " << target;

    auto read_authority_types = repo.read_latest(h.context(), target.code);
    BOOST_LOG_SEV(lg, debug) << "Read authority types: " << read_authority_types;

    REQUIRE(read_authority_types.size() == 1);
    CHECK(read_authority_types[0].code == target.code);
    CHECK(read_authority_types[0].name == target.name);
}
