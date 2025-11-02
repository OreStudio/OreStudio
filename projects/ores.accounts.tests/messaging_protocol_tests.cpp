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
#include "ores.utility/log/make_logger.hpp"
#include "faker-cxx/faker.h" // IWYU pragma: keep.
#include "ores.accounts/messaging/protocol.hpp"

namespace {

std::string test_suite("ores.accounts.tests.");

}

using namespace ores::accounts::messaging;
using ores::accounts::domain::account;

TEST_CASE("create_account_request_with_valid_fields", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    create_account_request req;
    req.username = "testuser";
    req.password = "test_password";
    req.totp_secret = "JBSWY3DPEHPK3PXP";
    req.email = "test@example.com";
    req.modified_by = "admin";
    req.is_admin = false;
    BOOST_LOG_SEV(lg, info) << "create_account_request: " << req;

    CHECK(req.username == "testuser");
    CHECK(req.password == "test_password");
    CHECK(req.totp_secret == "JBSWY3DPEHPK3PXP");
    CHECK(req.email == "test@example.com");
    CHECK(req.modified_by == "admin");
    CHECK(req.is_admin == false);
}

TEST_CASE("create_account_request_with_faker", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    create_account_request req;
    req.username = std::string(faker::internet::username());
    req.password = std::string(faker::internet::password());
    req.totp_secret = faker::string::alphanumeric(16);
    req.email = std::string(faker::internet::email());
    req.modified_by = std::string(faker::internet::username());
    req.is_admin = faker::datatype::boolean();
    BOOST_LOG_SEV(lg, info) << "create_account_request: " << req;

    CHECK(!req.username.empty());
    CHECK(!req.password.empty());
    CHECK(req.totp_secret.length() == 16);
    CHECK(!req.email.empty());
    CHECK(!req.modified_by.empty());
}

TEST_CASE("create_account_request_serialize_deserialize", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    create_account_request original;
    original.username = std::string(faker::internet::username());
    original.password = std::string(faker::internet::password());
    original.totp_secret = faker::string::alphanumeric(20);
    original.email = std::string(faker::internet::email());
    original.modified_by = "";
    original.is_admin = false;
    BOOST_LOG_SEV(lg, info) << "Original: " << original;

    const auto serialized = original.serialize();
    const auto result = create_account_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();
    BOOST_LOG_SEV(lg, info) << "Deserialized: " << deserialized;

    CHECK(deserialized.username == original.username);
    CHECK(deserialized.password == original.password);
    CHECK(deserialized.totp_secret == original.totp_secret);
    CHECK(deserialized.email == original.email);
}

TEST_CASE("create_account_response_with_valid_uuid", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    create_account_response resp;
    resp.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "create_account_response: " << resp;

    CHECK(!resp.account_id.is_nil());
}

TEST_CASE("create_account_response_serialize_deserialize", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    create_account_response original;
    original.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Original: " << original;

    const auto serialized = original.serialize();
    const auto result = create_account_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();
    BOOST_LOG_SEV(lg, info) << "Deserialized: " << deserialized;

    CHECK(deserialized.account_id == original.account_id);
}

TEST_CASE("list_accounts_request_serialize_deserialize", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    list_accounts_request original;
    BOOST_LOG_SEV(lg, info) << "Original: " << original;

    const auto serialized = original.serialize();
    const auto result = list_accounts_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();
    BOOST_LOG_SEV(lg, info) << "Deserialized: " << deserialized;
}

TEST_CASE("list_accounts_response_with_faker", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    list_accounts_response resp;

    for (int i = 0; i < 3; ++i) {
        account acc;
        acc.version = faker::number::integer(1, 100);
        acc.modified_by = std::string(faker::internet::username());
        acc.id = boost::uuids::random_generator()();
        acc.username = std::string(faker::internet::username());
        acc.password_hash = std::string(faker::crypto::sha256());
        acc.password_salt = std::string(faker::crypto::sha256());
        acc.totp_secret = faker::string::alphanumeric(20);
        acc.email = std::string(faker::internet::email());
        acc.is_admin = faker::datatype::boolean();
        resp.accounts.push_back(acc);
    }
    BOOST_LOG_SEV(lg, info) << "list_accounts_response: " << resp;

    CHECK(resp.accounts.size() == 3);
}

TEST_CASE("list_accounts_response_serialize_deserialize", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    list_accounts_response original;

    for (int i = 0; i < 2; ++i) {
        account acc;
        acc.version = i + 1;
        acc.modified_by = "user" + std::to_string(i);
        acc.id = boost::uuids::random_generator()();
        acc.username = "username" + std::to_string(i);
        acc.password_hash = std::string(faker::crypto::sha256());
        acc.password_salt = std::string(faker::crypto::sha256());
        acc.totp_secret = faker::string::alphanumeric(16);
        acc.email = "user" + std::to_string(i) + "@example.com";
        acc.is_admin = (i == 0);
        original.accounts.push_back(acc);
    }
    BOOST_LOG_SEV(lg, info) << "Original: " << original;

    const auto serialized = original.serialize();
    const auto result = list_accounts_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();
    BOOST_LOG_SEV(lg, info) << "Deserialized: " << deserialized;

    REQUIRE(deserialized.accounts.size() == original.accounts.size());
    for (size_t i = 0; i < original.accounts.size(); ++i) {
        CHECK(deserialized.accounts[i].version == original.accounts[i].version);
        CHECK(deserialized.accounts[i].username == original.accounts[i].username);
        CHECK(deserialized.accounts[i].email == original.accounts[i].email);
        CHECK(deserialized.accounts[i].is_admin == original.accounts[i].is_admin);
    }
}

TEST_CASE("login_request_with_valid_credentials", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    login_request req;
    req.username = "testuser";
    req.password = "test_password";
    BOOST_LOG_SEV(lg, info) << "login_request: " << req;

    CHECK(req.username == "testuser");
    CHECK(req.password == "test_password");
}

TEST_CASE("login_request_with_faker", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    login_request req;
    req.username = std::string(faker::internet::username());
    req.password = std::string(faker::internet::password());
    BOOST_LOG_SEV(lg, info) << "login_request: " << req;

    CHECK(!req.username.empty());
    CHECK(!req.password.empty());
}

TEST_CASE("login_request_serialize_deserialize", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    login_request original;
    original.username = std::string(faker::internet::username());
    original.password = std::string(faker::internet::password());
    BOOST_LOG_SEV(lg, info) << "Original: " << original;

    const auto serialized = original.serialize();
    const auto result = login_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();
    BOOST_LOG_SEV(lg, info) << "Deserialized: " << deserialized;

    CHECK(deserialized.username == original.username);
    CHECK(deserialized.password == original.password);
}

TEST_CASE("login_response_success", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    login_response resp;
    resp.success = true;
    resp.error_message = "";
    resp.account_id = boost::uuids::random_generator()();
    resp.username = std::string(faker::internet::username());
    resp.is_admin = faker::datatype::boolean();
    BOOST_LOG_SEV(lg, info) << "login_response: " << resp;

    CHECK(resp.success == true);
    CHECK(resp.error_message.empty());
    CHECK(!resp.account_id.is_nil());
    CHECK(!resp.username.empty());
}

TEST_CASE("login_response_failure", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    login_response resp;
    resp.success = false;
    resp.error_message = "Invalid credentials";
    resp.account_id = boost::uuids::nil_uuid();
    resp.username = "";
    resp.is_admin = false;
    BOOST_LOG_SEV(lg, info) << "login_response: " << resp;

    CHECK(resp.success == false);
    CHECK(resp.error_message == "Invalid credentials");
    CHECK(resp.account_id.is_nil());
    CHECK(resp.username.empty());
}

TEST_CASE("login_response_serialize_deserialize", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    login_response original;
    original.success = true;
    original.error_message = "";
    original.account_id = boost::uuids::random_generator()();
    original.username = std::string(faker::internet::username());
    original.is_admin = faker::datatype::boolean();
    BOOST_LOG_SEV(lg, info) << "Original: " << original;

    const auto serialized = original.serialize();
    const auto result = login_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();
    BOOST_LOG_SEV(lg, info) << "Deserialized: " << deserialized;

    CHECK(deserialized.success == original.success);
    CHECK(deserialized.error_message == original.error_message);
    CHECK(deserialized.account_id == original.account_id);
    CHECK(deserialized.username == original.username);
    CHECK(deserialized.is_admin == original.is_admin);
}

TEST_CASE("unlock_account_request_with_valid_uuid", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    unlock_account_request req;
    req.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "unlock_account_request: " << req;

    CHECK(!req.account_id.is_nil());
}

TEST_CASE("unlock_account_request_serialize_deserialize", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    unlock_account_request original;
    original.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Original: " << original;

    const auto serialized = original.serialize();
    const auto result = unlock_account_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();
    BOOST_LOG_SEV(lg, info) << "Deserialized: " << deserialized;

    CHECK(deserialized.account_id == original.account_id);
}

TEST_CASE("unlock_account_response_success", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    unlock_account_response resp;
    resp.success = true;
    resp.error_message = "";
    BOOST_LOG_SEV(lg, info) << "unlock_account_response: " << resp;

    CHECK(resp.success == true);
    CHECK(resp.error_message.empty());
}

TEST_CASE("unlock_account_response_failure", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    unlock_account_response resp;
    resp.success = false;
    resp.error_message = "Account not found";
    BOOST_LOG_SEV(lg, info) << "unlock_account_response: " << resp;

    CHECK(resp.success == false);
    CHECK(resp.error_message == "Account not found");
}

TEST_CASE("unlock_account_response_serialize_deserialize", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    unlock_account_response original;
    original.success = faker::datatype::boolean();
    original.error_message = original.success ? "" : "Error occurred";
    BOOST_LOG_SEV(lg, info) << "Original: " << original;

    const auto serialized = original.serialize();
    const auto result = unlock_account_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();
    BOOST_LOG_SEV(lg, info) << "Deserialized: " << deserialized;

    CHECK(deserialized.success == original.success);
    CHECK(deserialized.error_message == original.error_message);
}

TEST_CASE("create_multiple_random_login_requests", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        login_request req;
        req.username = std::string(faker::internet::username());
        req.password = std::string(faker::internet::password());
        BOOST_LOG_SEV(lg, info) << "login_request " << i << ":" << req;

        CHECK(!req.username.empty());
        CHECK(!req.password.empty());
    }
}

TEST_CASE("create_multiple_random_create_account_requests", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        create_account_request req;
        req.username = std::string(faker::internet::username());
        req.password = std::string(faker::internet::password());
        req.totp_secret = faker::string::alphanumeric(16);
        req.email = std::string(faker::internet::email());
        req.modified_by = std::string(faker::person::firstName()) + " " +
            std::string(faker::person::lastName());
        req.is_admin = faker::datatype::boolean();
        BOOST_LOG_SEV(lg, info) << "create_account_request " << i << ":" << req;

        CHECK(!req.username.empty());
        CHECK(!req.email.empty());
        CHECK(req.totp_secret.length() == 16);
    }
}
