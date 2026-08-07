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
#include "ores.dq.api/domain/artefact_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.api/generators/artefact_type_generator.hpp"
#include "ores.dq.core/repository/artefact_type_repository.hpp"
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
using ores::dq::repository::artefact_type_repository;
using ores::utility::generation::generation_context;

TEST_CASE("write_single_artefact_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    generation_context ctx;
    artefact_type_repository repo;
    auto artefact_type = generate_synthetic_artefact_type(ctx);
    artefact_type.tenant_id = h.tenant_id();
    artefact_type.code = artefact_type.code + "_" + std::string(faker::string::alphanumeric(8));

    BOOST_LOG_SEV(lg, debug) << "Artefact type: " << artefact_type;
    CHECK_NOTHROW(repo.write(h.context(), artefact_type));
}

TEST_CASE("write_multiple_artefact_types", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    generation_context ctx;
    artefact_type_repository repo;
    auto artefact_types = generate_synthetic_artefact_types(3, ctx);
    for (auto& a : artefact_types) {
        a.tenant_id = h.tenant_id();
        a.code = a.code + "_" + std::string(faker::string::alphanumeric(8));
    }
    BOOST_LOG_SEV(lg, debug) << "Artefact types: " << artefact_types;

    CHECK_NOTHROW(repo.write(h.context(), artefact_types));
}

TEST_CASE("read_latest_artefact_types", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    generation_context ctx;
    artefact_type_repository repo;
    auto written_artefact_types = generate_synthetic_artefact_types(3, ctx);
    for (auto& a : written_artefact_types) {
        a.tenant_id = h.tenant_id();
        a.code = a.code + "_" + std::string(faker::string::alphanumeric(8));
    }
    BOOST_LOG_SEV(lg, debug) << "Written artefact types: " << written_artefact_types;

    repo.write(h.context(), written_artefact_types);

    auto read_artefact_types = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read artefact types: " << read_artefact_types;

    CHECK(!read_artefact_types.empty());
    CHECK(read_artefact_types.size() >= written_artefact_types.size());
}

TEST_CASE("read_latest_artefact_type_by_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    generation_context ctx;
    artefact_type_repository repo;
    auto artefact_types = generate_synthetic_artefact_types(3, ctx);
    for (auto& a : artefact_types) {
        a.tenant_id = h.tenant_id();
        a.code = a.code + "_" + std::string(faker::string::alphanumeric(8));
    }

    const auto target = artefact_types.front();
    BOOST_LOG_SEV(lg, debug) << "Write artefact types: " << artefact_types;
    repo.write(h.context(), artefact_types);

    BOOST_LOG_SEV(lg, debug) << "Target artefact type: " << target;

    auto read_artefact_types = repo.read_latest(h.context(), target.code);
    BOOST_LOG_SEV(lg, debug) << "Read artefact types: " << read_artefact_types;

    REQUIRE(read_artefact_types.size() == 1);
    CHECK(read_artefact_types[0].code == target.code);
    CHECK(read_artefact_types[0].name == target.name);
}

TEST_CASE("read_all_artefact_type_versions", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    artefact_type_repository repo;

    generation_context ctx;
    auto at1 = generate_synthetic_artefact_type(ctx);
    at1.tenant_id = h.tenant_id();
    at1.code = at1.code + "_" + std::string(faker::string::alphanumeric(8));
    const auto test_code = at1.code;
    BOOST_LOG_SEV(lg, debug) << "Artefact type v1: " << at1;

    auto at2 = at1;
    at2.version = 0;
    at2.description = "Updated description for version test";
    BOOST_LOG_SEV(lg, debug) << "Artefact type v2: " << at2;

    repo.write(h.context(), at1);
    repo.write(h.context(), at2);

    auto read_artefact_types = repo.read_all(h.context(), test_code);
    BOOST_LOG_SEV(lg, debug) << "Read artefact types: " << read_artefact_types;

    CHECK(read_artefact_types.size() >= 2);
}

TEST_CASE("read_nonexistent_artefact_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    artefact_type_repository repo;

    const std::string nonexistent_code = "nonexistent.artefact_type.12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent_code;

    auto read_artefact_types = repo.read_latest(h.context(), nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read artefact types: " << read_artefact_types;

    CHECK(read_artefact_types.size() == 0);
}

TEST_CASE("write_and_read_artefact_type_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    artefact_type_repository repo;

    generation_context ctx;
    auto artefact_type = generate_synthetic_artefact_type(ctx);
    artefact_type.tenant_id = h.tenant_id();
    artefact_type.code = artefact_type.code + "_" + std::string(faker::string::alphanumeric(8));
    BOOST_LOG_SEV(lg, debug) << "Artefact type: " << artefact_type;

    const auto artefact_type_code = artefact_type.code;
    repo.write(h.context(), artefact_type);

    auto read_artefact_types = repo.read_latest(h.context(), artefact_type_code);
    BOOST_LOG_SEV(lg, debug) << "Read artefact types: " << read_artefact_types;

    REQUIRE(read_artefact_types.size() == 1);
    CHECK(read_artefact_types[0].code == artefact_type.code);
    CHECK(read_artefact_types[0].description == artefact_type.description);
    CHECK(read_artefact_types[0].artefact_table == artefact_type.artefact_table);
    CHECK(read_artefact_types[0].target_table == artefact_type.target_table);
    CHECK(read_artefact_types[0].target_subject == artefact_type.target_subject);
    CHECK(read_artefact_types[0].display_order == artefact_type.display_order);
}
