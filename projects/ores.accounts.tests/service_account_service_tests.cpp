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
#include <faker-cxx/internet.h>
#include <boost/asio/ip/address.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.testing/database_helper.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "faker-cxx/faker.h" // IWYU pragma: keep.
#include "ores.accounts/generators/account_generator.hpp"
#include "ores.accounts/service/account_service.hpp"

namespace {

const std::string test_suite("ores.accounts.tests");
const std::string database_table("oresdb.accounts");
const std::string tags("[service_account_service_tests]");

}

using namespace ores::accounts;
using ores::testing::database_helper;
using namespace ores::accounts::generators;

TEST_CASE("create_account_with_valid_data", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    auto original = generate_synthetic_account();
    const std::string username = original.username;
    const std::string email = original.email;
    const std::string password = faker::internet::password();
    BOOST_LOG_SEV(lg, info) << "Creating account: - username: " << username
                            << ", email: " << email;

    auto account = svc.create_account(h.get_context(), username, email,
        password, "admin", false);

    BOOST_LOG_SEV(lg, info) << "Created account: " << account;

    CHECK(account.username == username);
    CHECK(account.email == email);
    CHECK(!account.id.is_nil());
    CHECK(!account.password_hash.empty());
    CHECK(account.is_admin == false);
}

TEST_CASE("create_account_with_admin_flag", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    auto original = generate_synthetic_account();
    const std::string username = original.username;
    const std::string email = original.email;
    const std::string password = faker::internet::password();
    const std::string modified_by = original.modified_by;

    BOOST_LOG_SEV(lg, info) << "Creating admin account: - username: " << username
                            << ", email: " << email;

    auto account = svc.create_account(h.get_context(), username, email,
        password, modified_by, true);

    BOOST_LOG_SEV(lg, info) << "Created account: " << account;

    CHECK(account.username == username);
    CHECK(account.is_admin == true);
}

TEST_CASE("create_multiple_accounts", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    for (int i = 0; i < 5; ++i) {
        auto original = generate_synthetic_account();
        const std::string username = original.username;
        const std::string email = original.email;
        const std::string password = faker::internet::password();
        const std::string modified_by = original.modified_by;
        const bool is_admin = faker::datatype::boolean();
        BOOST_LOG_SEV(lg, info) << "Creating account: - username: " << username
                                << ", email: " << email;

        auto account = svc.create_account(h.get_context(), username, email,
            password, modified_by, is_admin);

        BOOST_LOG_SEV(lg, info) << "Account " << i << ": " << account;

        CHECK(account.username == username);
        CHECK(account.email == email);
        CHECK(!account.id.is_nil());
    }
}

TEST_CASE("create_account_with_empty_username_throws", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    BOOST_LOG_SEV(lg, info) << "Attempting to create account with empty username";

    auto original = generate_synthetic_account();
    const std::string username;
    const std::string email = original.email;
    const std::string password = faker::internet::password();
    const std::string modified_by = original.modified_by;
    const bool is_admin = faker::datatype::boolean();

    CHECK_THROWS_AS(svc.create_account(h.get_context(), username, email,
        password, modified_by, false), std::invalid_argument);
}

TEST_CASE("create_account_with_empty_email_throws", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    auto original = generate_synthetic_account();
    const std::string username = original.username;
    const std::string email;
    const std::string password = faker::internet::password();
    const std::string modified_by = original.modified_by;
    const bool is_admin = faker::datatype::boolean();

    BOOST_LOG_SEV(lg, info) << "Attempting to create account with empty email";

    CHECK_THROWS_AS(svc.create_account(h.get_context(), username, email,
        password, modified_by, false), std::invalid_argument);
}

TEST_CASE("create_account_with_empty_password_throws", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    auto original = generate_synthetic_account();
    const std::string username = original.username;
    const std::string email = original.email;
    const std::string password;
    const std::string modified_by = original.modified_by;
    const bool is_admin = faker::datatype::boolean();

    BOOST_LOG_SEV(lg, info) << "Attempting to create account with empty password";

    CHECK_THROWS_AS(svc.create_account(h.get_context(), username, email,
        password, modified_by, false), std::invalid_argument);
}

TEST_CASE("list_accounts_returns_empty_for_no_accounts", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    BOOST_LOG_SEV(lg, info) << "Listing accounts in empty database";

    auto accounts = svc.list_accounts(h.get_context());

    BOOST_LOG_SEV(lg, info) << "Found " << accounts.size() << " accounts";

    CHECK(accounts.empty());
}

TEST_CASE("list_accounts_returns_created_accounts",
    tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    const int expected_count = 3;
    auto written_accounts = generate_synthetic_accounts(expected_count);

    for (const auto& acc : written_accounts) {
        const std::string username = acc.username;
        const std::string email = acc.email;
        const std::string password = faker::internet::password();
        const std::string modified_by = acc.modified_by;
        BOOST_LOG_SEV(lg, info) << "Creating account: - username: " << username
                                << ", email: " << email;

        svc.create_account(h.get_context(), username, email,
            password, modified_by, false);
    }

    BOOST_LOG_SEV(lg, info) << "Listing accounts";

    auto accounts = svc.list_accounts(h.get_context());

    BOOST_LOG_SEV(lg, info) << "Found " << accounts.size() << " accounts";
    for (const auto& acc : accounts) {
        BOOST_LOG_SEV(lg, info) << "Account: " << acc;
    }

    CHECK(accounts.size() == expected_count);
}

TEST_CASE("login_with_valid_credentials", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    auto original = generate_synthetic_account();
    const std::string username = original.username;
    const std::string email = original.email;
    const std::string password = faker::internet::password();
    const std::string modified_by = original.modified_by;
    const bool is_admin = faker::datatype::boolean();
    BOOST_LOG_SEV(lg, info) << "Creating account: - username: " << username
                            << ", email: " << email;

    // Create account
    auto created_account = svc.create_account(h.get_context(), username,
        email, password, modified_by, false);

    BOOST_LOG_SEV(lg, info) << "Attempting login with valid credentials";

    // Attempt login
    auto ip = boost::asio::ip::make_address(faker::internet::ipv4());
    auto logged_in_account = svc.login(h.get_context(), username, password, ip);

    BOOST_LOG_SEV(lg, info) << "Logged in account: " << logged_in_account;

    CHECK(logged_in_account.username == username);
    CHECK(logged_in_account.id == created_account.id);
}

TEST_CASE("login_with_invalid_password_throws", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    auto original = generate_synthetic_account();
    const std::string username = original.username;
    const std::string email = original.email;
    const std::string password = faker::internet::password();
    const std::string modified_by = original.modified_by;
    const bool is_admin = faker::datatype::boolean();

    // Create account
    auto created_account = svc.create_account(h.get_context(), username,
        email, password, modified_by, false);

    BOOST_LOG_SEV(lg, info) << "Attempting login with invalid password";

    auto ip = boost::asio::ip::make_address(faker::internet::ipv4());
    CHECK_THROWS_AS(svc.login(h.get_context(), username, "wrong_password", ip),
        std::runtime_error);
}

TEST_CASE("login_with_nonexistent_username_throws", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    BOOST_LOG_SEV(lg, info) << "Attempting login with nonexistent username";
    const std::string username = std::string(faker::internet::username());
    const std::string password = faker::internet::password();
    auto ip = boost::asio::ip::make_address(faker::internet::ipv4());
    CHECK_THROWS_AS(svc.login(h.get_context(), username, password, ip),
        std::runtime_error);
}

TEST_CASE("login_with_empty_username_throws", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    BOOST_LOG_SEV(lg, info) << "Attempting login with empty username";

    const std::string username;
    const std::string password = faker::internet::password();
    auto ip = boost::asio::ip::make_address(faker::internet::ipv4());
    CHECK_THROWS_AS(svc.login(h.get_context(), username, password, ip),
        std::invalid_argument);
}

TEST_CASE("login_with_empty_password_throws", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    BOOST_LOG_SEV(lg, info) << "Attempting login with empty password";

    auto original = generate_synthetic_account();
    const std::string username = std::string(faker::internet::username());
    const auto ip = boost::asio::ip::make_address(faker::internet::ipv4());
    CHECK_THROWS_AS(svc.login(h.get_context(), username, "", ip),
        std::invalid_argument);
}

TEST_CASE("account_locks_after_multiple_failed_logins",
    tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    auto original = generate_synthetic_account();
    const std::string username = original.username;
    const std::string email = original.email;
    const std::string password = faker::internet::password();
    const std::string modified_by = original.modified_by;
    BOOST_LOG_SEV(lg, info) << "Creating account: - username: " << username
                            << ", email: " << email;

    // Create account
    svc.create_account(h.get_context(), username, email, password,
        modified_by, false);

    BOOST_LOG_SEV(lg, info) << "Attempting 5 failed logins to lock account";

    auto ip = boost::asio::ip::make_address("192.168.1.100");

    // Attempt 5 failed logins
    for (int i = 0; i < 5; ++i) {
        try {
            svc.login(h.get_context(), username, "wrong_password", ip);
        } catch (const std::runtime_error& e) {
            BOOST_LOG_SEV(lg, info) << "Failed login attempt " << (i + 1)
                                   << ": " << e.what();
        }
    }

    // Next attempt should indicate account is locked
    try {
        svc.login(h.get_context(), username, password, ip);
        FAIL("Expected account to be locked");
    } catch (const std::runtime_error& e) {
        BOOST_LOG_SEV(lg, info) << "Account locked: " << e.what();
        CHECK(std::string(e.what()).find("locked") != std::string::npos);
    }
}

TEST_CASE("unlock_account_successful", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    auto original = generate_synthetic_account();
    const std::string username = original.username;
    const std::string email = original.email;
    const std::string password = faker::internet::password();
    const std::string modified_by = original.modified_by;
    BOOST_LOG_SEV(lg, info) << "Creating account: - username: " << username
                            << ", email: " << email;

    // Create account
    auto account = svc.create_account(h.get_context(), username, email,
        password, modified_by, false);

    BOOST_LOG_SEV(lg, info) << "Locking account by failing 5 login attempts";

    auto ip = boost::asio::ip::make_address("192.168.1.100");

    // Lock the account by failing 5 logins
    for (int i = 0; i < 5; ++i) {
        try {
            svc.login(h.get_context(), username, "wrong_password", ip);
        } catch (...) {}
    }

    BOOST_LOG_SEV(lg, info) << "Unlocking account";

    // Unlock the account
    svc.unlock_account(h.get_context(), account.id);

    BOOST_LOG_SEV(lg, info) << "Attempting login after unlock";

    // Should now be able to login successfully
    auto logged_in_account = svc.login(h.get_context(), username, password, ip);

    CHECK(logged_in_account.username == username);
}

TEST_CASE("unlock_nonexistent_account_throws", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    BOOST_LOG_SEV(lg, info) << "Attempting to unlock nonexistent account";

    boost::uuids::random_generator gen;
    auto fake_id = gen();

    CHECK_THROWS_AS(svc.unlock_account(h.get_context(), fake_id),
        std::invalid_argument);
}

TEST_CASE("delete_nonexistent_account_throws",
    tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    BOOST_LOG_SEV(lg, info) << "Attempting to delete nonexistent account";

    boost::uuids::random_generator gen;
    auto fake_id = gen();

    CHECK_THROWS_AS(svc.delete_account(h.get_context(), fake_id),
        std::invalid_argument);
}

TEST_CASE("login_with_different_ip_addresses", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    repository::account_repository account_repo;
    repository::login_info_repository login_info_repo;
    service::account_service svc(account_repo, login_info_repo);

    auto original = generate_synthetic_account();
    const std::string username = original.username;
    const std::string email = original.email;
    const std::string password = faker::internet::password();
    const std::string modified_by = original.modified_by;
    BOOST_LOG_SEV(lg, info) << "Creating account: - username: " << username
                            << ", email: " << email;

    // Create account
    svc.create_account(h.get_context(), username, email, password,
        modified_by, false);

    BOOST_LOG_SEV(lg, info) << "Testing logins from different IPs";

    // Login from multiple IPs
    for (int i = 0; i < 3; ++i) {
        std::string ip_str = std::string(faker::internet::ipv4());
        auto ip = boost::asio::ip::make_address(ip_str);

        auto account = svc.login(h.get_context(), username, password, ip);

        BOOST_LOG_SEV(lg, info) << "Login " << i << " from IP: " << ip_str
                               << " - account: " << account.username;

        CHECK(account.username == username);
    }
}
