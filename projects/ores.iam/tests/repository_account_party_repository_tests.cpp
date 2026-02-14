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
#include "ores.iam/repository/account_party_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/account_party.hpp"
#include "ores.iam/domain/account_party_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/repository/account_repository.hpp"
#include "ores.iam/generators/account_generator.hpp"
#include "ores.refdata/repository/party_repository.hpp"
#include "ores.refdata/generators/party_generator.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"

using namespace ores::logging;
using namespace ores::iam::generators;

using ores::iam::domain::account_party;
using ores::testing::database_helper;
using ores::iam::repository::account_party_repository;
using ores::iam::repository::account_repository;
using ores::refdata::repository::party_repository;

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[repository]");

boost::uuids::uuid find_system_party_id(
    party_repository& repo, const std::string& tid) {
    auto parties = repo.read_latest();
    for (const auto& p : parties)
        if (p.tenant_id == tid)
            return p.id;
    throw std::runtime_error("No system party for tenant: " + tid);
}

account_party make_account_party(database_helper& h,
    const boost::uuids::uuid& account_id,
    const boost::uuids::uuid& party_id) {
    account_party ap;
    ap.tenant_id = h.tenant_id().to_string();
    ap.account_id = account_id;
    ap.party_id = party_id;
    ap.modified_by = std::string(faker::internet::username());
    ap.change_reason_code = "system.test";
    ap.change_commentary = "Synthetic test data";
    ap.performed_by = std::string(faker::internet::username());
    return ap;
}

}

TEST_CASE("write_single_account_party", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    account_repository acc_repo(h.context());
    account_party_repository repo(h.context());
    party_repository party_repo(h.context());
    const auto party_id = find_system_party_id(
        party_repo, h.tenant_id().to_string());

    auto acc = generate_synthetic_account(ctx);
    acc_repo.write(acc);

    auto ap = make_account_party(h, acc.id, party_id);

    BOOST_LOG_SEV(lg, debug) << "Account party: " << ap;
    CHECK_NOTHROW(repo.write(ap));
}

TEST_CASE("write_multiple_account_parties", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    account_repository acc_repo(h.context());
    account_party_repository repo(h.context());
    party_repository party_repo(h.context());
    const auto system_party_id = find_system_party_id(
        party_repo, h.tenant_id().to_string());

    std::vector<account_party> aps;
    for (int i = 0; i < 3; ++i) {
        auto acc = generate_synthetic_account(ctx);
        acc_repo.write(acc);

        auto party = ores::refdata::generators::generate_synthetic_party(ctx);
        party.change_reason_code = "system.test";
        party.parent_party_id = system_party_id;
        party_repo.write(party);

        aps.push_back(make_account_party(h, acc.id, party.id));
    }

    BOOST_LOG_SEV(lg, debug) << "Account parties: " << aps;
    CHECK_NOTHROW(repo.write(aps));
}

TEST_CASE("read_latest_account_parties", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    account_repository acc_repo(h.context());
    account_party_repository repo(h.context());
    party_repository party_repo(h.context());
    const auto system_party_id = find_system_party_id(
        party_repo, h.tenant_id().to_string());

    std::vector<account_party> written;
    for (int i = 0; i < 3; ++i) {
        auto acc = generate_synthetic_account(ctx);
        acc_repo.write(acc);

        auto party = ores::refdata::generators::generate_synthetic_party(ctx);
        party.change_reason_code = "system.test";
        party.parent_party_id = system_party_id;
        party_repo.write(party);

        written.push_back(make_account_party(h, acc.id, party.id));
    }

    BOOST_LOG_SEV(lg, debug) << "Written account parties: " << written;
    repo.write(written);

    auto read_aps = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read account parties: " << read_aps;

    CHECK(!read_aps.empty());
    CHECK(read_aps.size() >= written.size());
}

TEST_CASE("read_latest_account_parties_by_account", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    account_repository acc_repo(h.context());
    account_party_repository repo(h.context());
    party_repository party_repo(h.context());
    const auto party_id = find_system_party_id(
        party_repo, h.tenant_id().to_string());

    auto acc = generate_synthetic_account(ctx);
    acc_repo.write(acc);

    auto ap = make_account_party(h, acc.id, party_id);
    BOOST_LOG_SEV(lg, debug) << "Written account party: " << ap;
    repo.write(ap);

    BOOST_LOG_SEV(lg, debug) << "Target account ID: " << acc.id;

    auto read_aps = repo.read_latest_by_account(acc.id);
    BOOST_LOG_SEV(lg, debug) << "Read account parties: " << read_aps;

    REQUIRE(read_aps.size() >= 1);
    CHECK(read_aps[0].account_id == acc.id);
}

TEST_CASE("read_nonexistent_account_party", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_party_repository repo(h.context());

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent account ID: " << nonexistent_id;

    auto read_aps = repo.read_latest_by_account(nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read account parties: " << read_aps;

    CHECK(read_aps.size() == 0);
}
