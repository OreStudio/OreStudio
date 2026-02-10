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
#include "ores.refdata/repository/party_id_scheme_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/party_id_scheme.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/party_id_scheme_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/generators/party_id_scheme_generator.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::party_id_scheme;
using ores::refdata::repository::party_id_scheme_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_party_id_scheme", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto pis = generate_synthetic_party_id_scheme();
    BOOST_LOG_SEV(lg, debug) << "Party ID scheme: " << pis;

    party_id_scheme_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), pis));
}

TEST_CASE("write_multiple_party_id_schemes", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto party_id_schemes = generate_synthetic_party_id_schemes(3);
    BOOST_LOG_SEV(lg, debug) << "Party ID schemes: " << party_id_schemes;

    party_id_scheme_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), party_id_schemes));
}

TEST_CASE("read_latest_party_id_schemes", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto written_party_id_schemes = generate_synthetic_party_id_schemes(3);
    BOOST_LOG_SEV(lg, debug) << "Written party ID schemes: "
                             << written_party_id_schemes;

    party_id_scheme_repository repo;
    repo.write(h.context(), written_party_id_schemes);

    auto read_party_id_schemes = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read party ID schemes: "
                             << read_party_id_schemes;

    CHECK(read_party_id_schemes.size() >= written_party_id_schemes.size());
}

TEST_CASE("read_latest_party_id_scheme_by_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto pis = generate_synthetic_party_id_scheme();
    const auto original_name = pis.name;
    BOOST_LOG_SEV(lg, debug) << "Party ID scheme: " << pis;

    party_id_scheme_repository repo;
    repo.write(h.context(), pis);

    pis.name = original_name + " v2";
    repo.write(h.context(), pis);

    auto read_party_id_schemes = repo.read_latest(h.context(), pis.code);
    BOOST_LOG_SEV(lg, debug) << "Read party ID schemes: "
                             << read_party_id_schemes;

    REQUIRE(read_party_id_schemes.size() == 1);
    CHECK(read_party_id_schemes[0].code == pis.code);
    CHECK(read_party_id_schemes[0].name == original_name + " v2");
}

TEST_CASE("read_nonexistent_party_id_scheme_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    party_id_scheme_repository repo;

    const std::string nonexistent_code = "NONEXISTENT_CODE_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent_code;

    auto read_party_id_schemes =
        repo.read_latest(h.context(), nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read party ID schemes: "
                             << read_party_id_schemes;

    CHECK(read_party_id_schemes.size() == 0);
}
