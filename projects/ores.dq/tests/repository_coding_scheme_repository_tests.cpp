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
#include "ores.dq/repository/coding_scheme_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/coding_scheme_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/generators/coding_scheme_generator.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::dq::generators;

using ores::testing::database_helper;
using ores::dq::repository::coding_scheme_repository;

TEST_CASE("write_single_coding_scheme", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    coding_scheme_repository repo(h.context());
    auto coding_scheme = generate_synthetic_coding_scheme();

    BOOST_LOG_SEV(lg, debug) << "Coding scheme: " << coding_scheme;
    CHECK_NOTHROW(repo.write(coding_scheme));
}

TEST_CASE("write_multiple_coding_schemes", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    coding_scheme_repository repo(h.context());
    auto coding_schemes = generate_synthetic_coding_schemes(3);
    BOOST_LOG_SEV(lg, debug) << "Coding schemes: " << coding_schemes;

    CHECK_NOTHROW(repo.write(coding_schemes));
}

TEST_CASE("read_latest_coding_schemes", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    coding_scheme_repository repo(h.context());
    auto written_coding_schemes = generate_synthetic_coding_schemes(3);
    BOOST_LOG_SEV(lg, debug) << "Written coding schemes: " << written_coding_schemes;

    repo.write(written_coding_schemes);

    auto read_coding_schemes = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read coding schemes: " << read_coding_schemes;

    CHECK(!read_coding_schemes.empty());
    CHECK(read_coding_schemes.size() >= written_coding_schemes.size());
}

TEST_CASE("read_latest_coding_scheme_by_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    coding_scheme_repository repo(h.context());
    auto coding_schemes = generate_synthetic_coding_schemes(3);

    const auto target = coding_schemes.front();
    BOOST_LOG_SEV(lg, debug) << "Write coding schemes: " << coding_schemes;
    repo.write(coding_schemes);

    BOOST_LOG_SEV(lg, debug) << "Target coding scheme: " << target;

    auto read_coding_schemes = repo.read_latest(target.code);
    BOOST_LOG_SEV(lg, debug) << "Read coding schemes: " << read_coding_schemes;

    REQUIRE(read_coding_schemes.size() == 1);
    CHECK(read_coding_schemes[0].code == target.code);
    CHECK(read_coding_schemes[0].name == target.name);
}

TEST_CASE("read_nonexistent_coding_scheme", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    coding_scheme_repository repo(h.context());

    const std::string nonexistent_code = "nonexistent.coding_scheme.12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent_code;

    auto read_coding_schemes = repo.read_latest(nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read coding schemes: " << read_coding_schemes;

    CHECK(read_coding_schemes.size() == 0);
}
