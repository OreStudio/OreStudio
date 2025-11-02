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
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.accounts/domain/account.hpp"
#include "ores.accounts/repository/account_repository.hpp"
#include "ores.accounts.tests/repository_helper.hpp"

namespace {

std::string test_suite("ores.accounts.tests.");

}

using ores::accounts::domain::account;
using ores::accounts::repository::account_repository;
using ores::accounts::tests::repository_helper;

TEST_CASE("write_single_account", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;
    auto acc = helper.create_test_account("test.write.single");
    std::vector<account> accounts = {acc};
    BOOST_LOG_SEV(lg, info) << "Accounts: " << accounts;

    CHECK_NOTHROW(repo.write(helper.get_context(), accounts));
}

TEST_CASE("write_multiple_accounts", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;
    std::vector<account> accounts;
    for (int i = 0; i < 5; ++i) {
        const auto username = "test.write.multi." + std::to_string(i);
        accounts.push_back(helper.create_test_account(username, i % 2 == 0));
    }
    BOOST_LOG_SEV(lg, info) << "Accounts: " << accounts;

    CHECK_NOTHROW(repo.write(helper.get_context(), accounts));
}

TEST_CASE("read_latest_accounts", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;
    std::vector<account> written_accounts;
    for (int i = 0; i < 3; ++i) {
        const auto username = "test.read.latest." + std::to_string(i);
        written_accounts.push_back(helper.create_test_account(username));
    }
    BOOST_LOG_SEV(lg, info) << "Written accounts: " << written_accounts;

    repo.write(helper.get_context(), written_accounts);

    auto read_accounts = repo.read_latest(helper.get_context());
    BOOST_LOG_SEV(lg, info) << "Read accounts: " << read_accounts;

    CHECK(!read_accounts.empty());
    CHECK(read_accounts.size() >= written_accounts.size());
}

TEST_CASE("read_latest_account_by_id", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;

    auto acc = helper.create_test_account("test.read.by.id");
    const auto test_id = acc.id;

    std::vector<account> accounts = {acc};
    BOOST_LOG_SEV(lg, info) << "Write accounts: " << accounts;

    repo.write(helper.get_context(), accounts);

    auto read_accounts = repo.read_latest(helper.get_context(), test_id);
    BOOST_LOG_SEV(lg, info) << "Read accounts: " << read_accounts;


    REQUIRE(read_accounts.size() == 1);
    CHECK(read_accounts[0].id == test_id);
    CHECK(read_accounts[0].username == "test.read.by.id");
    CHECK(read_accounts[0].email == "test.read.by.id@test.com");
}

TEST_CASE("read_all_accounts", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;

    std::vector<account> written_accounts;
    for (int i = 0; i < 3; ++i) {
        const auto username = "test.read.all." + std::to_string(i);
        written_accounts.push_back(helper.create_test_account(username));
    }
    BOOST_LOG_SEV(lg, info) << "Written accounts: " << written_accounts;

    repo.write(helper.get_context(), written_accounts);

    auto read_accounts = repo.read_all(helper.get_context());
    BOOST_LOG_SEV(lg, info) << "Read accounts: " << read_accounts;

    CHECK(!read_accounts.empty());
    CHECK(read_accounts.size() >= written_accounts.size());
}

TEST_CASE("read_all_accounts_by_id", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;

    // Write multiple versions of the same account
    auto acc1 = helper.create_test_account("test.versions");
    const auto test_id = acc1.id;
    acc1.version = 1;
    BOOST_LOG_SEV(lg, info) << "Account 1: " << acc1;

    auto acc2 = acc1;
    acc2.version = 2;
    acc2.email = "test.versions.v2@test.com";
    BOOST_LOG_SEV(lg, info) << "Account 2: " << acc2;

    repo.write(helper.get_context(), {acc1});
    repo.write(helper.get_context(), {acc2});

    // Read all versions
    auto read_accounts = repo.read_all(helper.get_context(), test_id);
    BOOST_LOG_SEV(lg, info) << "Read accounts: " << read_accounts;

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

TEST_CASE("read_latest_by_username", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;

    // Write account with specific username
    const std::string test_username = "test.read.by.username";
    auto acc = helper.create_test_account(test_username);
    BOOST_LOG_SEV(lg, info) << "Account: " << acc;

    repo.write(helper.get_context(), {acc});

    // Read by username
    auto read_accounts = repo.read_latest_by_username(
        helper.get_context(), test_username);
    BOOST_LOG_SEV(lg, info) << "Read accounts: " << read_accounts;

    REQUIRE(read_accounts.size() == 1);
    CHECK(read_accounts[0].username == test_username);
    CHECK(read_accounts[0].email == test_username + "@test.com");
}

TEST_CASE("read_nonexistent_account_by_id", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;

    // Generate a random UUID that doesn't exist in database.
    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Non-existent ID: " << nonexistent_id;

    auto read_accounts = repo.read_latest(helper.get_context(), nonexistent_id);
    BOOST_LOG_SEV(lg, info) << "Read accounts: " << read_accounts;

    CHECK(read_accounts.size() == 0);
}

TEST_CASE("read_nonexistent_username", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;

    const std::string nonexistent_username = "nonexistent.user.12345";
    BOOST_LOG_SEV(lg, info) << "Non-existent username: " << nonexistent_username;

    auto read_accounts = repo.read_latest_by_username(
        helper.get_context(), nonexistent_username);
    BOOST_LOG_SEV(lg, info) << "Read accounts: " << read_accounts;

    CHECK(read_accounts.size() == 0);
}

TEST_CASE("write_and_read_admin_account", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;

    // Create admin account
    auto admin_acc = helper.create_test_account("test.admin.user", true);
    BOOST_LOG_SEV(lg, info) << "Admin account: " << admin_acc;

    const auto admin_id = admin_acc.id;

    repo.write(helper.get_context(), {admin_acc});

    // Read back and verify admin flag
    auto read_accounts = repo.read_latest(helper.get_context(), admin_id);
    BOOST_LOG_SEV(lg, info) << "Read accounts: " << read_accounts;

    REQUIRE(read_accounts.size() == 1);
    CHECK(read_accounts[0].is_admin == true);
    CHECK(read_accounts[0].username == "test.admin.user");
}
