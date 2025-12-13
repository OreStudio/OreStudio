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
#include "ores.accounts/service/account_service.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/internet.h>
#include <boost/asio/ip/address.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/faker/internet.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.accounts/generators/account_generator.hpp"
#include "ores.accounts/domain/account_json_io.hpp" // IWYU pragma: keep.

namespace {

const std::string_view test_suite("ores.accounts.tests");
const std::string database_table("oresdb.accounts");
const std::string tags("[service]");

}

using namespace ores::accounts;
using namespace ores::utility::log;
using ores::utility::faker::internet;
using ores::testing::scoped_database_helper;
using namespace ores::accounts::generators;

TEST_CASE("create_account_with_valid_data", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    const auto e = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const std::string password = faker::internet::password();
    const auto a = sut.create_account(e.username, e.email, password,
            e.modified_by, false);
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.username == e.username);
    CHECK(a.email == e.email);
    CHECK(a.is_admin == e.is_admin);

    CHECK(!a.id.is_nil());
    CHECK(!a.password_hash.empty());
}

TEST_CASE("create_account_with_admin_flag", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    auto e = generate_synthetic_account();
    e.is_admin = true;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const std::string password = faker::internet::password();
    const auto a =
        sut.create_account(e.username, e.email, password,
        e.modified_by, e.is_admin);
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.username == e.username);
    CHECK(a.is_admin == e.is_admin);
}

TEST_CASE("create_multiple_accounts", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    for (int i = 0; i < 5; ++i) {
        BOOST_LOG_SEV(lg, info) << "Creating account: " << i;

        const auto e = generate_synthetic_account();
        BOOST_LOG_SEV(lg, info) << "Expected: " << e;

        const std::string password = faker::internet::password();
        const auto a =
            sut.create_account(e.username, e.email, password,
            e.modified_by, e.is_admin);
        BOOST_LOG_SEV(lg, info) << "Actual: " << a;

        CHECK(a.username == e.username);
        CHECK(a.email == e.email);
        CHECK(!a.id.is_nil());
    }
}

TEST_CASE("create_account_with_empty_username_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    auto e = generate_synthetic_account();
    e.username = "";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const std::string password = faker::internet::password();
    CHECK_THROWS_AS(sut.create_account(e.username, e.email, password,
            e.modified_by, e.is_admin), std::invalid_argument);
}

TEST_CASE("create_account_with_empty_email_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    auto e = generate_synthetic_account();
    e.email = "";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const std::string password = faker::internet::password();
    CHECK_THROWS_AS(sut.create_account(e.username, e.email,
            password, e.modified_by, e.is_admin), std::invalid_argument);
}

TEST_CASE("create_account_with_empty_password_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    const auto e = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const std::string empty_password;
    CHECK_THROWS_AS(sut.create_account(e.username, e.email,
            empty_password, e.modified_by, e.is_admin), std::invalid_argument);
}

TEST_CASE("list_accounts_returns_empty_for_no_accounts", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());
    const auto a = sut.list_accounts();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
    CHECK(a.empty());
}

TEST_CASE("list_accounts_returns_created_accounts", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    const int expected_count = 3;
    const auto expected_list = generate_synthetic_accounts(expected_count);

    for (const auto& e : expected_list) {
        const std::string password = faker::internet::password();
        BOOST_LOG_SEV(lg, info) << "Expected: " << e;
        sut.create_account(e.username, e.email, password,
            e.modified_by, e.is_admin);
    }

    auto actual_list = sut.list_accounts();
    BOOST_LOG_SEV(lg, info) << "Actual: " << actual_list;
    CHECK(actual_list.size() == expected_count);
}

TEST_CASE("login_with_valid_credentials", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    auto e = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const std::string password = faker::internet::password();
    const auto account = sut.create_account(e.username,
        e.email, password, e.modified_by, e.is_admin);

    auto ip = internet::ipv4();
    auto a = sut.login(account.username, password, ip);

    CHECK(a.username == e.username);
    CHECK(a.id == account.id);
}

TEST_CASE("login_with_invalid_password_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    auto e = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const std::string password = faker::internet::password();
    const auto account = sut.create_account(e.username,
        e.email, password, e.modified_by, e.is_admin);

    auto ip = internet::ipv4();
    CHECK_THROWS_AS(sut.login(e.username, "wrong_password", ip),
        std::runtime_error);
}

TEST_CASE("login_with_nonexistent_username_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    BOOST_LOG_SEV(lg, info) << "Attempting login with nonexistent username";
    const std::string username = std::string(faker::internet::username());
    const std::string password = faker::internet::password();
    auto ip = internet::ipv4();
    BOOST_LOG_SEV(lg, info) << "Creating account: - username: " << username
                            << " password: " << password << " IP: " << ip;

    CHECK_THROWS_AS(sut.login(username, password, ip),
        std::runtime_error);
}

TEST_CASE("login_with_empty_username_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    BOOST_LOG_SEV(lg, info) << "Attempting login with empty username";
    const std::string username;
    const std::string password = faker::internet::password();
    auto ip = internet::ipv4();
    BOOST_LOG_SEV(lg, info) << "Creating account: - username: " << username
                            << " password: " << password << " IP: " << ip;

    CHECK_THROWS_AS(sut.login(username, password, ip),
        std::invalid_argument);
}

TEST_CASE("login_with_empty_password_throws", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    BOOST_LOG_SEV(lg, info) << "Attempting login with empty password";

    auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Account: " << account;

    const std::string password = faker::internet::password();
    sut.create_account(account.username, account.email, password,
        account.modified_by, account.is_admin);

    const std::string empty_password;
    auto ip = internet::ipv4();
    CHECK_THROWS_AS(sut.login(account.username, empty_password, ip),
        std::invalid_argument);
}

TEST_CASE("account_locks_after_multiple_failed_logins", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Account: " << account;

    const std::string password = faker::internet::password();
    sut.create_account(account.username, account.email, password,
        account.modified_by, account.is_admin);

    BOOST_LOG_SEV(lg, info) << "Attempting 5 failed logins to lock account";

    auto ip = internet::ipv4();
    for (int i = 0; i < 5; ++i) {
        try {
            sut.login(account.username, "wrong_password", ip);
        } catch (const std::runtime_error& e) {
            BOOST_LOG_SEV(lg, info) << "Failed login attempt " << (i + 1)
                                   << ": " << e.what();
        }
    }

    // Next attempt should indicate account is locked
    try {
        sut.login(account.username, password, ip);
        FAIL("Expected account to be locked.");
    } catch (const std::runtime_error& e) {
        BOOST_LOG_SEV(lg, info) << "Account locked: " << e.what();
        CHECK(std::string(e.what()).find("locked") != std::string::npos);
    }
}

TEST_CASE("lock_account_successful", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    const auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Account: " << account;
    const std::string password = faker::internet::password();

    const auto generated =
        sut.create_account(account.username, account.email,
        password, account.modified_by, account.is_admin);

    BOOST_LOG_SEV(lg, info) << "Locking account.";
    bool lock_result = sut.lock_account(generated.id);
    CHECK(lock_result == true);

    BOOST_LOG_SEV(lg, info) << "Attempting login after lock";
    auto ip = internet::ipv4();
    CHECK_THROWS_AS(sut.login(generated.username, password, ip), std::runtime_error);
}

TEST_CASE("lock_nonexistent_account_returns_false", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    boost::uuids::random_generator gen;
    const auto non_existent_id = gen();
    BOOST_LOG_SEV(lg, info) << "Attempting to lock nonexistent account: "
                            << non_existent_id;

    CHECK(sut.lock_account(non_existent_id) == false);
}

TEST_CASE("unlock_account_successful", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    const auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Account: " << account;
    const std::string password = faker::internet::password();

    const auto generated =
        sut.create_account(account.username, account.email,
        password, account.modified_by, account.is_admin);

    BOOST_LOG_SEV(lg, info) << "Locking account by failing 5 login attempts";
    auto ip = internet::ipv4();
    for (int i = 0; i < 5; ++i) {
        try {
            sut.login(account.username, "wrong_password", ip);
        } catch (...) {}
    }

    BOOST_LOG_SEV(lg, info) << "Unlocking account.";
    bool unlock_result = sut.unlock_account(generated.id);
    CHECK(unlock_result == true);

    BOOST_LOG_SEV(lg, info) << "Attempting login after unlock";

    // Should now be able to login successfully
    auto logged_in_account =
        sut.login(generated.username, password, ip);

    CHECK(logged_in_account.username == account.username);
}

TEST_CASE("unlock_nonexistent_account_returns_false", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    boost::uuids::random_generator gen;
    const auto non_existent_id = gen();
    BOOST_LOG_SEV(lg, info) << "Attempting to unlock nonexistent account: "
                            << non_existent_id;

    CHECK(sut.unlock_account(non_existent_id) == false);
}

TEST_CASE("delete_nonexistent_account_throws",
    tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    boost::uuids::random_generator gen;
    const auto non_existent_id = gen();
    BOOST_LOG_SEV(lg, info) << "Attempting to delete nonexistent account: "
                            << non_existent_id;

    CHECK_THROWS_AS(sut.delete_account(non_existent_id),
        std::invalid_argument);
}

TEST_CASE("login_with_different_ip_addresses", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    service::account_service sut(h.context());

    auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Account: " << account;

    const std::string password = faker::internet::password();
    sut.create_account(account.username, account.email, password,
        account.modified_by, account.is_admin);

    BOOST_LOG_SEV(lg, info) << "Testing logins from different IPs.";
    for (int i = 0; i < 3; ++i) {
        auto ip = internet::ipv4();
        auto login = sut.login(account.username, password, ip);

        BOOST_LOG_SEV(lg, info) << "Login " << i << " from IP: " << ip
                                << " - account: " << account.username;
        CHECK(account.username == login.username);
    }
}
