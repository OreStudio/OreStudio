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
#include <boost/test/unit_test.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/test/logging.hpp"
#include "ores.accounts/domain/account.hpp"
#include "ores.accounts/repository/account_repository.hpp"
#include "ores.accounts.tests/repository_helper.hpp"

namespace {

const std::string test_module("ores.accounts.tests");
const std::string test_suite("repository_account_repository_tests");

}

using namespace ores::utility::log;
using ores::accounts::domain::account;
using ores::accounts::repository::account_repository;
using ores::accounts::tests::repository_helper;

struct database_cleanup_fixture {
    database_cleanup_fixture() {
        repository_helper helper;
        helper.cleanup_database();
    }
};

BOOST_FIXTURE_TEST_SUITE(repository_account_repository_tests, database_cleanup_fixture)

BOOST_AUTO_TEST_CASE(write_single_account) {
    SETUP_TEST_LOG_SOURCE_DEBUG("write_single_account");

    account_repository repo;
    repository_helper helper;
    auto acc = helper.create_test_account("test.write.single");

    BOOST_LOG_SEV(lg, debug) << "Writing account: " << acc;
    std::vector<account> accounts = {acc};

    BOOST_CHECK_NO_THROW(repo.write(helper.get_context(), accounts));

    BOOST_LOG_SEV(lg, info) << "Successfully wrote single account";
}

BOOST_AUTO_TEST_CASE(write_multiple_accounts) {
    SETUP_TEST_LOG_SOURCE_DEBUG("write_multiple_accounts");

    account_repository repo;
    std::vector<account> accounts;

    repository_helper helper;
    for (int i = 0; i < 5; ++i) {
        const auto username = "test.write.multi." + std::to_string(i);
        accounts.push_back(helper.create_test_account(username, i % 2 == 0));
    }

    BOOST_LOG_SEV(lg, debug) << "Writing " << accounts.size() << " accounts";

    BOOST_CHECK_NO_THROW(repo.write(helper.get_context(), accounts));

    BOOST_LOG_SEV(lg, info) << "Successfully wrote multiple accounts";
}

BOOST_AUTO_TEST_CASE(read_latest_accounts) {
    SETUP_TEST_LOG_SOURCE_DEBUG("read_latest_accounts");

    account_repository repo;
    repository_helper helper;

    std::vector<account> written_accounts;
    for (int i = 0; i < 3; ++i) {
        const auto username = "test.read.latest." + std::to_string(i);
        written_accounts.push_back(helper.create_test_account(username));
    }

    BOOST_LOG_SEV(lg, debug) << "Writing " << written_accounts.size()
                             << " accounts for reading test";
    repo.write(helper.get_context(), written_accounts);

    auto read_accounts = repo.read_latest(helper.get_context());

    BOOST_LOG_SEV(lg, debug) << "Read " << read_accounts.size()
                             << " latest accounts";

    BOOST_CHECK(!read_accounts.empty());
    BOOST_CHECK_GE(read_accounts.size(), written_accounts.size());

    BOOST_LOG_SEV(lg, info) << "Successfully read latest accounts";
}

BOOST_AUTO_TEST_CASE(read_latest_account_by_id) {
    SETUP_TEST_LOG_SOURCE_DEBUG("read_latest_account_by_id");

    account_repository repo;
    repository_helper helper;

    auto acc = helper.create_test_account("test.read.by.id");
    const auto test_id = acc.id;

    BOOST_LOG_SEV(lg, debug) << "Writing account with ID: " << test_id;
    std::vector<account> accounts = {acc};
    repo.write(helper.get_context(), accounts);

    auto read_accounts = repo.read_latest(
        helper.get_context(), test_id);

    BOOST_LOG_SEV(lg, debug) << "Read " << read_accounts.size()
                             << " accounts for ID: " << test_id;

    BOOST_REQUIRE_EQUAL(read_accounts.size(), 1);
    BOOST_CHECK_EQUAL(read_accounts[0].id, test_id);
    BOOST_CHECK_EQUAL(read_accounts[0].username, "test.read.by.id");
    BOOST_CHECK_EQUAL(read_accounts[0].email, "test.read.by.id@test.com");

    BOOST_LOG_SEV(lg, info) << "Successfully read account by ID";
}

BOOST_AUTO_TEST_CASE(read_all_accounts) {
    SETUP_TEST_LOG_SOURCE_DEBUG("read_all_accounts");

    account_repository repo;
    repository_helper helper;

    std::vector<account> written_accounts;
    for (int i = 0; i < 3; ++i) {
        const auto username = "test.read.all." + std::to_string(i);
        written_accounts.push_back(helper.create_test_account(username));
    }

    BOOST_LOG_SEV(lg, debug) << "Writing " << written_accounts.size()
                             << " accounts for read all test";
    repo.write(helper.get_context(), written_accounts);

    auto read_accounts = repo.read_all(helper.get_context());

    BOOST_LOG_SEV(lg, debug) << "Read " << read_accounts.size() << " total accounts";

    BOOST_CHECK(!read_accounts.empty());
    BOOST_CHECK_GE(read_accounts.size(), written_accounts.size());

    BOOST_LOG_SEV(lg, info) << "Successfully read all accounts";
}

BOOST_AUTO_TEST_CASE(read_all_accounts_by_id) {
    SETUP_TEST_LOG_SOURCE_DEBUG("read_all_accounts_by_id");

    account_repository repo;
    repository_helper helper;

    // Write multiple versions of the same account
    auto acc1 = helper.create_test_account("test.versions");
    const auto test_id = acc1.id;
    acc1.version = 1;

    auto acc2 = acc1;
    acc2.version = 2;
    acc2.email = "test.versions.v2@test.com";

    BOOST_LOG_SEV(lg, debug) << "Writing multiple versions for ID: " << test_id;
    repo.write(helper.get_context(), {acc1});
    repo.write(helper.get_context(), {acc2});

    // Read all versions
    auto read_accounts = repo.read_all(helper.get_context(), test_id);

    BOOST_LOG_SEV(lg, debug) << "Read " << read_accounts.size()
                             << " versions for ID: " << test_id;

    BOOST_CHECK_GE(read_accounts.size(), 2);

    // Verify different versions exist
    bool found_v1 = false, found_v2 = false;
    for (const auto& acc : read_accounts) {
        if (acc.id == test_id && acc.version == 1) found_v1 = true;
        if (acc.id == test_id && acc.version == 2) found_v2 = true;
    }

    BOOST_CHECK(found_v1);
    BOOST_CHECK(found_v2);

    BOOST_LOG_SEV(lg, info) << "Successfully read all account versions by ID";
}

BOOST_AUTO_TEST_CASE(read_latest_by_username) {
    SETUP_TEST_LOG_SOURCE_DEBUG("read_latest_by_username");

    account_repository repo;
    repository_helper helper;

    // Write account with specific username
    const std::string test_username = "test.read.by.username";
    auto acc = helper.create_test_account(test_username);

    BOOST_LOG_SEV(lg, debug) << "Writing account with username: "
                             << test_username;
    repo.write(helper.get_context(), {acc});

    // Read by username
    auto read_accounts = repo.read_latest_by_username(
        helper.get_context(), test_username);

    BOOST_LOG_SEV(lg, debug) << "Read " << read_accounts.size()
                             << " accounts for username: " << test_username;

    BOOST_REQUIRE_EQUAL(read_accounts.size(), 1);
    BOOST_CHECK_EQUAL(read_accounts[0].username, test_username);
    BOOST_CHECK_EQUAL(read_accounts[0].email, test_username + "@test.com");

    BOOST_LOG_SEV(lg, info) << "Successfully read account by username";
}

BOOST_AUTO_TEST_CASE(read_nonexistent_account_by_id) {
    SETUP_TEST_LOG_SOURCE_DEBUG("read_nonexistent_account_by_id");

    account_repository repo;
    repository_helper helper;

    // Generate a random UUID that doesn't exist
    const auto nonexistent_id = boost::uuids::random_generator()();

    BOOST_LOG_SEV(lg, debug) << "Reading nonexistent ID: " << nonexistent_id;

    auto read_accounts = repo.read_latest(helper.get_context(), nonexistent_id);

    BOOST_LOG_SEV(lg, debug) << "Read " << read_accounts.size()
                             << " accounts for nonexistent ID";

    BOOST_CHECK_EQUAL(read_accounts.size(), 0);

    BOOST_LOG_SEV(lg, info) << "Successfully handled nonexistent account read";
}

BOOST_AUTO_TEST_CASE(read_nonexistent_username) {
    SETUP_TEST_LOG_SOURCE_DEBUG("read_nonexistent_username");

    account_repository repo;
    repository_helper helper;

    const std::string nonexistent_username = "nonexistent.user.12345";

    BOOST_LOG_SEV(lg, debug) << "Reading nonexistent username: "
                             << nonexistent_username;

    auto read_accounts = repo.read_latest_by_username(
        helper.get_context(), nonexistent_username);

    BOOST_LOG_SEV(lg, debug) << "Read " << read_accounts.size()
                             << " accounts for nonexistent username";

    BOOST_CHECK_EQUAL(read_accounts.size(), 0);

    BOOST_LOG_SEV(lg, info) << "Successfully handled nonexistent username read";
}

BOOST_AUTO_TEST_CASE(write_and_read_admin_account) {
    SETUP_TEST_LOG_SOURCE_DEBUG("write_and_read_admin_account");

    account_repository repo;
    repository_helper helper;

    // Create admin account
    auto admin_acc = helper.create_test_account("test.admin.user", true);
    const auto admin_id = admin_acc.id;

    BOOST_LOG_SEV(lg, debug) << "Writing admin account: " << admin_acc;
    repo.write(helper.get_context(), {admin_acc});

    // Read back and verify admin flag
    auto read_accounts = repo.read_latest(
        helper.get_context(), admin_id);

    BOOST_REQUIRE_EQUAL(read_accounts.size(), 1);
    BOOST_CHECK_EQUAL(read_accounts[0].is_admin, true);
    BOOST_CHECK_EQUAL(read_accounts[0].username, "test.admin.user");

    BOOST_LOG_SEV(lg, info) << "Successfully wrote and read admin account";
}

BOOST_AUTO_TEST_SUITE_END()
