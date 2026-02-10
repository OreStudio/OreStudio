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
#include "ores.refdata/repository/contact_type_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/contact_type.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/contact_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/generators/contact_type_generator.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::contact_type;
using ores::refdata::repository::contact_type_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_contact_type", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ct = generate_synthetic_contact_type();
    BOOST_LOG_SEV(lg, debug) << "Contact type: " << ct;

    contact_type_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), ct));
}

TEST_CASE("write_multiple_contact_types", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto contact_types = generate_synthetic_contact_types(3);
    BOOST_LOG_SEV(lg, debug) << "Contact types: " << contact_types;

    contact_type_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), contact_types));
}

TEST_CASE("read_latest_contact_types", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto written_contact_types = generate_synthetic_contact_types(3);
    BOOST_LOG_SEV(lg, debug) << "Written contact types: "
                             << written_contact_types;

    contact_type_repository repo;
    repo.write(h.context(), written_contact_types);

    auto read_contact_types = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read contact types: " << read_contact_types;

    CHECK(read_contact_types.size() >= written_contact_types.size());
}

TEST_CASE("read_latest_contact_type_by_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ct = generate_synthetic_contact_type();
    const auto original_name = ct.name;
    BOOST_LOG_SEV(lg, debug) << "Contact type: " << ct;

    contact_type_repository repo;
    repo.write(h.context(), ct);

    ct.name = original_name + " v2";
    repo.write(h.context(), ct);

    auto read_contact_types = repo.read_latest(h.context(), ct.code);
    BOOST_LOG_SEV(lg, debug) << "Read contact types: " << read_contact_types;

    REQUIRE(read_contact_types.size() == 1);
    CHECK(read_contact_types[0].code == ct.code);
    CHECK(read_contact_types[0].name == original_name + " v2");
}

TEST_CASE("read_nonexistent_contact_type_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    contact_type_repository repo;

    const std::string nonexistent_code = "NONEXISTENT_CODE_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent_code;

    auto read_contact_types =
        repo.read_latest(h.context(), nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read contact types: " << read_contact_types;

    CHECK(read_contact_types.size() == 0);
}
