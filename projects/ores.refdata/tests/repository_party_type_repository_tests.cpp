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
#include "ores.refdata/repository/party_type_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/party_type.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/party_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/generators/party_type_generator.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::party_type;
using ores::refdata::repository::party_type_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_party_type", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto pt = generate_synthetic_party_type();
    pt.tenant_id = h.tenant_id();
    BOOST_LOG_SEV(lg, debug) << "Party type: " << pt;

    party_type_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), pt));
}

TEST_CASE("write_multiple_party_types", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto party_types = generate_synthetic_party_types(3);
    for (auto& pt : party_types)
        pt.tenant_id = h.tenant_id();
    BOOST_LOG_SEV(lg, debug) << "Party types: " << party_types;

    party_type_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), party_types));
}

TEST_CASE("read_latest_party_types", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto written_party_types = generate_synthetic_party_types(3);
    for (auto& pt : written_party_types)
        pt.tenant_id = h.tenant_id();
    BOOST_LOG_SEV(lg, debug) << "Written party types: " << written_party_types;

    party_type_repository repo;
    repo.write(h.context(), written_party_types);

    auto read_party_types = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read party types: " << read_party_types;

    CHECK(read_party_types.size() >= written_party_types.size());
}

TEST_CASE("read_latest_party_type_by_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto pt = generate_synthetic_party_type();
    pt.tenant_id = h.tenant_id();
    const auto original_name = pt.name;
    BOOST_LOG_SEV(lg, debug) << "Party type: " << pt;

    party_type_repository repo;
    repo.write(h.context(), pt);

    pt.name = original_name + " v2";
    repo.write(h.context(), pt);

    auto read_party_types = repo.read_latest(h.context(), pt.code);
    BOOST_LOG_SEV(lg, debug) << "Read party types: " << read_party_types;

    REQUIRE(read_party_types.size() == 1);
    CHECK(read_party_types[0].code == pt.code);
    CHECK(read_party_types[0].name == original_name + " v2");
}

TEST_CASE("read_nonexistent_party_type_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    party_type_repository repo;

    const std::string nonexistent_code = "NONEXISTENT_CODE_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent_code;

    auto read_party_types = repo.read_latest(h.context(), nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read party types: " << read_party_types;

    CHECK(read_party_types.size() == 0);
}
