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
#include "ores.refdata/repository/party_identifier_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/party_identifier.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/party_identifier_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/generators/party_identifier_generator.hpp"
#include "ores.refdata/generators/party_generator.hpp"
#include "ores.refdata/repository/party_repository.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::party_identifier;
using ores::refdata::repository::party_identifier_repository;
using ores::refdata::repository::party_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_party_identifier", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto party = generate_synthetic_party();
    party.tenant_id = h.tenant_id().to_string();
    party.change_reason_code = "system.test";
    party_repository party_repo(h.context());
    party_repo.write(party);

    auto pi = generate_synthetic_party_identifier();
    pi.tenant_id = h.tenant_id().to_string();
    pi.change_reason_code = "system.test";
    pi.party_id = party.id;
    BOOST_LOG_SEV(lg, debug) << "Party identifier: " << pi;

    party_identifier_repository repo(h.context());
    CHECK_NOTHROW(repo.write(pi));
}

TEST_CASE("write_multiple_party_identifiers", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto party = generate_synthetic_party();
    party.tenant_id = h.tenant_id().to_string();
    party.change_reason_code = "system.test";
    party_repository party_repo(h.context());
    party_repo.write(party);

    auto party_identifiers = generate_synthetic_party_identifiers(3);
    for (auto& pi : party_identifiers) {
        pi.tenant_id = h.tenant_id().to_string();
        pi.change_reason_code = "system.test";
        pi.party_id = party.id;
    }
    BOOST_LOG_SEV(lg, debug) << "Party identifiers: " << party_identifiers;

    party_identifier_repository repo(h.context());
    CHECK_NOTHROW(repo.write(party_identifiers));
}

TEST_CASE("read_latest_party_identifiers", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto party = generate_synthetic_party();
    party.tenant_id = h.tenant_id().to_string();
    party.change_reason_code = "system.test";
    party_repository party_repo(h.context());
    party_repo.write(party);

    auto written_party_identifiers = generate_synthetic_party_identifiers(3);
    for (auto& pi : written_party_identifiers) {
        pi.tenant_id = h.tenant_id().to_string();
        pi.change_reason_code = "system.test";
        pi.party_id = party.id;
    }
    BOOST_LOG_SEV(lg, debug) << "Written party identifiers: "
                             << written_party_identifiers;

    party_identifier_repository repo(h.context());
    repo.write(written_party_identifiers);

    auto read_party_identifiers = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read party identifiers: "
                             << read_party_identifiers;

    CHECK(read_party_identifiers.size() >= written_party_identifiers.size());
}

TEST_CASE("read_latest_party_identifier_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto party = generate_synthetic_party();
    party.tenant_id = h.tenant_id().to_string();
    party.change_reason_code = "system.test";
    party_repository party_repo(h.context());
    party_repo.write(party);

    auto pi = generate_synthetic_party_identifier();
    pi.tenant_id = h.tenant_id().to_string();
    pi.change_reason_code = "system.test";
    pi.party_id = party.id;
    const auto original_id_value = pi.id_value;
    BOOST_LOG_SEV(lg, debug) << "Party identifier: " << pi;

    party_identifier_repository repo(h.context());
    repo.write(pi);

    pi.id_value = original_id_value + "_v2";
    repo.write(pi);

    auto read_party_identifiers = repo.read_latest(pi.id);
    BOOST_LOG_SEV(lg, debug) << "Read party identifiers: "
                             << read_party_identifiers;

    REQUIRE(read_party_identifiers.size() == 1);
    CHECK(read_party_identifiers[0].id == pi.id);
    CHECK(read_party_identifiers[0].id_value == original_id_value + "_v2");
}

TEST_CASE("read_nonexistent_party_identifier_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    party_identifier_repository repo(h.context());

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_party_identifiers = repo.read_latest(nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read party identifiers: "
                             << read_party_identifiers;

    CHECK(read_party_identifiers.size() == 0);
}
