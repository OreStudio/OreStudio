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
#include "ores.iam/repository/account_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/account.hpp"
#include "ores.iam/domain/account_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/generators/account_generator.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string database_table("ores_iam_accounts_tbl");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::iam::generators;

using ores::testing::database_helper;
using ores::iam::domain::account;
using ores::iam::repository::account_repository;


TEST_CASE("write_single_account", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_repository repo(h.context());
    auto account = generate_synthetic_account();

    BOOST_LOG_SEV(lg, debug) << "Account: " << account;
    CHECK_NOTHROW(repo.write(account));
}

TEST_CASE("write_multiple_accounts", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_repository repo(h.context());
    auto accounts = generate_synthetic_accounts(5);
    BOOST_LOG_SEV(lg, debug) << "Accounts: " << accounts;

    CHECK_NOTHROW(repo.write(accounts));
}

TEST_CASE("read_latest_accounts", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_repository repo(h.context());
    auto written_accounts = generate_synthetic_accounts(3);
    BOOST_LOG_SEV(lg, debug) << "Written accounts: " << written_accounts;

    repo.write(written_accounts);

    auto read_accounts = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read accounts: " << read_accounts;

    CHECK(!read_accounts.empty());
    CHECK(read_accounts.size() >= written_accounts.size());
}

TEST_CASE("read_latest_account_by_id", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_repository repo(h.context());
    auto accounts = generate_synthetic_accounts(5);

    const auto target = accounts.front();
    BOOST_LOG_SEV(lg, debug) << "Write accounts: " << accounts;
    repo.write(accounts);

    BOOST_LOG_SEV(lg, debug) << "target account: " << target;

    auto read_accounts = repo.read_latest(target.id);
    BOOST_LOG_SEV(lg, debug) << "Read accounts: " << read_accounts;

    REQUIRE(read_accounts.size() == 1);
    CHECK(read_accounts[0].id == target.id);
    CHECK(read_accounts[0].username == target.username);
    CHECK(read_accounts[0].email == target.email);
}

TEST_CASE("read_all_accounts", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_repository repo(h.context());
    auto written_accounts = generate_synthetic_accounts(5);
    BOOST_LOG_SEV(lg, debug) << "Generated accounts: " << written_accounts;

    repo.write(written_accounts);

    auto read_accounts = repo.read_all();
    BOOST_LOG_SEV(lg, debug) << "Read accounts: " << read_accounts;

    CHECK(!read_accounts.empty());
    CHECK(read_accounts.size() >= written_accounts.size());
}

TEST_CASE("read_all_accounts_by_id", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_repository repo(h.context());

    // Write multiple versions of the same account
    auto acc1 = generate_synthetic_account();
    const auto test_id = acc1.id;
    BOOST_LOG_SEV(lg, debug) << "Account 1: " << acc1;

    auto acc2 = acc1;
    acc2.version = 1;
    acc2.email = "test.versions.v2@test.com";
    BOOST_LOG_SEV(lg, debug) << "Account 2: " << acc2;

    repo.write({acc1});
    repo.write({acc2});

    // Read all versions
    auto read_accounts = repo.read_all(test_id);
    BOOST_LOG_SEV(lg, debug) << "Read accounts: " << read_accounts;

    CHECK(read_accounts.size() >= 2);

    // Verify different versions exist
    bool found_v1 = false, found_v2 = false;
    for (const auto& acc : read_accounts) {
        if (acc.id == test_id && acc.version == 1) found_v1 = true;
        if (acc.id == test_id && acc.version == 2) found_v2 = true;
    }

    CHECK(found_v1);
    CHECK(found_v2);
}

TEST_CASE("read_latest_by_username", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_repository repo(h.context());
    auto accounts = generate_synthetic_accounts(5);
    BOOST_LOG_SEV(lg, debug) << "Generated accounts: " << accounts;

    repo.write(accounts);

    auto acc = accounts.front();
    BOOST_LOG_SEV(lg, debug) << "Target account: " << acc;

    // Read by username
    auto read_accounts = repo.read_latest_by_username(acc.username);
    BOOST_LOG_SEV(lg, debug) << "Read accounts: " << read_accounts;

    REQUIRE(read_accounts.size() == 1);
    CHECK(read_accounts[0].username == acc.username);
    CHECK(read_accounts[0].email == acc.email);
}

TEST_CASE("read_nonexistent_account_by_id", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_repository repo(h.context());

    // Generate a random UUID that doesn't exist in database.
    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_accounts = repo.read_latest(nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read accounts: " << read_accounts;

    CHECK(read_accounts.size() == 0);
}

TEST_CASE("read_nonexistent_username", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_repository repo(h.context());

    const std::string nonexistent_username = "nonexistent.user.12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent username: " << nonexistent_username;

    auto read_accounts = repo.read_latest_by_username(nonexistent_username);
    BOOST_LOG_SEV(lg, debug) << "Read accounts: " << read_accounts;

    CHECK(read_accounts.size() == 0);
}

TEST_CASE("write_and_read_account_by_id", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_repository repo(h.context());

    // Note: Admin privileges are now managed via RBAC role assignments
    auto acc = generate_synthetic_account();
    BOOST_LOG_SEV(lg, debug) << "Account: " << acc;

    const auto acc_id = acc.id;
    repo.write({acc});

    // Read back and verify data
    auto read_accounts = repo.read_latest(acc_id);
    BOOST_LOG_SEV(lg, debug) << "Read accounts: " << read_accounts;

    REQUIRE(read_accounts.size() == 1);
    CHECK(read_accounts[0].username == acc.username);
    CHECK(read_accounts[0].email == acc.email);
}
