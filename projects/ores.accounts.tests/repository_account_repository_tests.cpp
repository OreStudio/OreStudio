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
#include "ores.accounts/generators/account_generator.hpp"
#include "ores.accounts/repository/account_repository.hpp"
#include "ores.accounts.tests/repository_helper.hpp"

namespace {

std::string test_suite("ores.accounts.tests");

}

using ores::accounts::domain::account;
using ores::accounts::tests::repository_helper;
using ores::accounts::repository::account_repository;
using ores::accounts::generators::generate_fake_accounts;

TEST_CASE("write_single_account", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;
    auto accounts =
        (generate_fake_accounts() | std::views::take(1)) |
        std::ranges::to<std::vector>();

    BOOST_LOG_SEV(lg, debug) << "Accounts: " << accounts;
    CHECK_NOTHROW(repo.write(helper.get_context(), accounts));
}

TEST_CASE("write_multiple_accounts", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;
    auto accounts =
        (generate_fake_accounts() | std::views::take(5)) |
        std::ranges::to<std::vector>();
    BOOST_LOG_SEV(lg, debug) << "Accounts: " << accounts;

    CHECK_NOTHROW(repo.write(helper.get_context(), accounts));
}

TEST_CASE("read_latest_accounts", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;
    auto written_accounts =
        (generate_fake_accounts() | std::views::take(3)) |
        std::ranges::to<std::vector>();
    BOOST_LOG_SEV(lg, debug) << "Written accounts: " << written_accounts;

    repo.write(helper.get_context(), written_accounts);

    auto read_accounts = repo.read_latest(helper.get_context());
    BOOST_LOG_SEV(lg, debug) << "Read accounts: " << read_accounts;

    CHECK(!read_accounts.empty());
    CHECK(read_accounts.size() >= written_accounts.size());
}

TEST_CASE("read_latest_account_by_id", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;
    auto accounts =
        (generate_fake_accounts() | std::views::take(5)) |
        std::ranges::to<std::vector>();

    const auto target = accounts.front();
    BOOST_LOG_SEV(lg, debug) << "Write accounts: " << accounts;
    repo.write(helper.get_context(), accounts);

    BOOST_LOG_SEV(lg, debug) << "target account: " << target;

    auto read_accounts = repo.read_latest(helper.get_context(), target.id);
    BOOST_LOG_SEV(lg, debug) << "Read accounts: " << read_accounts;

    REQUIRE(read_accounts.size() == 1);
    CHECK(read_accounts[0].id == target.id);
    CHECK(read_accounts[0].username == target.username);
    CHECK(read_accounts[0].email == target.email);
}

TEST_CASE("read_all_accounts", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;
    auto written_accounts =
        (generate_fake_accounts() | std::views::take(5)) |
        std::ranges::to<std::vector>();
    BOOST_LOG_SEV(lg, debug) << "Generated accounts: " << written_accounts;

    repo.write(helper.get_context(), written_accounts);

    auto read_accounts = repo.read_all(helper.get_context());
    BOOST_LOG_SEV(lg, debug) << "Read accounts: " << read_accounts;

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
    auto acc1 = *generate_fake_accounts().begin();
    const auto test_id = acc1.id;
    acc1.version = 1;
    BOOST_LOG_SEV(lg, debug) << "Account 1: " << acc1;

    auto acc2 = acc1;
    acc2.version = 2;
    acc2.email = "test.versions.v2@test.com";
    BOOST_LOG_SEV(lg, debug) << "Account 2: " << acc2;

    repo.write(helper.get_context(), {acc1});
    repo.write(helper.get_context(), {acc2});

    // Read all versions
    auto read_accounts = repo.read_all(helper.get_context(), test_id);
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

TEST_CASE("read_latest_by_username", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;
    auto accounts =
        (generate_fake_accounts() | std::views::take(5)) |
        std::ranges::to<std::vector>();
    BOOST_LOG_SEV(lg, debug) << "Generated accounts: " << accounts;

    repo.write(helper.get_context(), accounts);

    auto acc = accounts.front();
    BOOST_LOG_SEV(lg, debug) << "Target account: " << acc;

    // Read by username
    auto read_accounts = repo.read_latest_by_username(
        helper.get_context(), acc.username);
    BOOST_LOG_SEV(lg, debug) << "Read accounts: " << read_accounts;

    REQUIRE(read_accounts.size() == 1);
    CHECK(read_accounts[0].username == acc.username);
    CHECK(read_accounts[0].email == acc.email);
}

TEST_CASE("read_nonexistent_account_by_id", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;

    // Generate a random UUID that doesn't exist in database.
    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_accounts =
        repo.read_latest(helper.get_context(), nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read accounts: " << read_accounts;

    CHECK(read_accounts.size() == 0);
}

TEST_CASE("read_nonexistent_username", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;

    const std::string nonexistent_username = "nonexistent.user.12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent username: " << nonexistent_username;

    auto read_accounts = repo.read_latest_by_username(
        helper.get_context(), nonexistent_username);
    BOOST_LOG_SEV(lg, debug) << "Read accounts: " << read_accounts;

    CHECK(read_accounts.size() == 0);
}

TEST_CASE("write_and_read_admin_account", "[repository_account_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    account_repository repo;

    // Create admin account
    auto admin_acc = *generate_fake_accounts().begin();
    admin_acc.is_admin = true;
    BOOST_LOG_SEV(lg, debug) << "Admin account: " << admin_acc;

    const auto admin_id = admin_acc.id;
    repo.write(helper.get_context(), {admin_acc});

    // Read back and verify admin flag
    auto read_accounts = repo.read_latest(helper.get_context(), admin_id);
    BOOST_LOG_SEV(lg, debug) << "Read accounts: " << read_accounts;

    REQUIRE(read_accounts.size() == 1);
    CHECK(read_accounts[0].is_admin == true);
    CHECK(read_accounts[0].username == admin_acc.username);
}
