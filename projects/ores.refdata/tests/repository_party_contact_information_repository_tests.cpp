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
#include "ores.refdata/repository/party_contact_information_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/party_contact_information.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/party_contact_information_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/generators/party_contact_information_generator.hpp"
#include "ores.refdata/generators/party_generator.hpp"
#include "ores.refdata/repository/party_repository.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::party_contact_information;
using ores::refdata::repository::party_contact_information_repository;
using ores::refdata::repository::party_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_party_contact_information", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto party = generate_synthetic_party();
    party.tenant_id = h.tenant_id().to_string();
    party.change_reason_code = "system.test";
    party_repository party_repo(h.context());
    party_repo.write(party);

    auto pci = generate_synthetic_party_contact_information();
    pci.tenant_id = h.tenant_id().to_string();
    pci.change_reason_code = "system.test";
    pci.party_id = party.id;
    BOOST_LOG_SEV(lg, debug) << "Party contact information: " << pci;

    party_contact_information_repository repo(h.context());
    CHECK_NOTHROW(repo.write(pci));
}

TEST_CASE("write_multiple_party_contact_informations", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto party = generate_synthetic_party();
    party.tenant_id = h.tenant_id().to_string();
    party.change_reason_code = "system.test";
    party_repository party_repo(h.context());
    party_repo.write(party);

    auto party_contact_informations =
        generate_synthetic_party_contact_informations(3);
    for (auto& pci : party_contact_informations) {
        pci.tenant_id = h.tenant_id().to_string();
        pci.change_reason_code = "system.test";
        pci.party_id = party.id;
    }
    BOOST_LOG_SEV(lg, debug) << "Party contact informations: "
                             << party_contact_informations;

    party_contact_information_repository repo(h.context());
    CHECK_NOTHROW(repo.write(party_contact_informations));
}

TEST_CASE("read_latest_party_contact_informations", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto party = generate_synthetic_party();
    party.tenant_id = h.tenant_id().to_string();
    party.change_reason_code = "system.test";
    party_repository party_repo(h.context());
    party_repo.write(party);

    auto written_party_contact_informations =
        generate_synthetic_party_contact_informations(3);
    for (auto& pci : written_party_contact_informations) {
        pci.tenant_id = h.tenant_id().to_string();
        pci.change_reason_code = "system.test";
        pci.party_id = party.id;
    }
    BOOST_LOG_SEV(lg, debug) << "Written party contact informations: "
                             << written_party_contact_informations;

    party_contact_information_repository repo(h.context());
    repo.write(written_party_contact_informations);

    auto read_party_contact_informations = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read party contact informations: "
                             << read_party_contact_informations;

    CHECK(read_party_contact_informations.size() >=
        written_party_contact_informations.size());
}

TEST_CASE("read_latest_party_contact_information_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto party = generate_synthetic_party();
    party.tenant_id = h.tenant_id().to_string();
    party.change_reason_code = "system.test";
    party_repository party_repo(h.context());
    party_repo.write(party);

    auto pci = generate_synthetic_party_contact_information();
    pci.tenant_id = h.tenant_id().to_string();
    pci.change_reason_code = "system.test";
    pci.party_id = party.id;
    const auto original_city = pci.city;
    BOOST_LOG_SEV(lg, debug) << "Party contact information: " << pci;

    party_contact_information_repository repo(h.context());
    repo.write(pci);

    pci.city = original_city + " v2";
    repo.write(pci);

    auto read_party_contact_informations = repo.read_latest(pci.id);
    BOOST_LOG_SEV(lg, debug) << "Read party contact informations: "
                             << read_party_contact_informations;

    REQUIRE(read_party_contact_informations.size() == 1);
    CHECK(read_party_contact_informations[0].id == pci.id);
    CHECK(read_party_contact_informations[0].city == original_city + " v2");
}

TEST_CASE("read_nonexistent_party_contact_information_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    party_contact_information_repository repo(h.context());

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_party_contact_informations = repo.read_latest(nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read party contact informations: "
                             << read_party_contact_informations;

    CHECK(read_party_contact_informations.size() == 0);
}
