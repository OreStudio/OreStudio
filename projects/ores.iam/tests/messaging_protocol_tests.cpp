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
#include "ores.iam/messaging/protocol.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/log/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[messaging]");

}

using namespace ores::iam::messaging;
using ores::iam::domain::account;
using namespace ores::utility::log;

TEST_CASE("create_account_request_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    create_account_request rq;
    rq.username = "testuser";
    rq.password = "test_password";
    rq.totp_secret = "JBSWY3DPEHPK3PXP";
    rq.email = "test@example.com";
    rq.recorded_by = "admin";
    BOOST_LOG_SEV(lg, info) << "Rrequest: " << rq;

    CHECK(rq.username == "testuser");
    CHECK(rq.password == "test_password");
    CHECK(rq.totp_secret == "JBSWY3DPEHPK3PXP");
    CHECK(rq.email == "test@example.com");
    CHECK(rq.recorded_by == "admin");
}

TEST_CASE("create_account_request_with_faker", tags) {
    auto lg(make_logger(test_suite));

    create_account_request rq;
    rq.username = std::string(faker::internet::username());
    rq.password = std::string(faker::internet::password());
    rq.totp_secret = faker::string::alphanumeric(16);
    rq.email = std::string(faker::internet::email());
    rq.recorded_by = std::string(faker::internet::username());
    BOOST_LOG_SEV(lg, info) << "create_account_request: " << rq;

    CHECK(!rq.username.empty());
    CHECK(!rq.password.empty());
    CHECK(rq.totp_secret.length() == 16);
    CHECK(!rq.email.empty());
    CHECK(!rq.recorded_by.empty());
}

TEST_CASE("create_account_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    create_account_request e;
    e.username = std::string(faker::internet::username());
    e.password = std::string(faker::internet::password());
    e.totp_secret = faker::string::alphanumeric(20);
    e.email = std::string(faker::internet::email());
    e.recorded_by = std::string(faker::internet::username());
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = create_account_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.username == e.username);
    CHECK(a.password == e.password);
    CHECK(a.totp_secret == e.totp_secret);
    CHECK(a.email == e.email);
}

TEST_CASE("create_account_response_with_valid_uuid", tags) {
    auto lg(make_logger(test_suite));

    create_account_response rp;
    rp.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(!rp.account_id.is_nil());
}

TEST_CASE("create_account_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    create_account_response e;
    e.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = create_account_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.account_id == e.account_id);
}

TEST_CASE("list_accounts_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    list_accounts_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = list_accounts_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
}

TEST_CASE("list_accounts_response_with_faker", tags) {
    auto lg(make_logger(test_suite));

    list_accounts_response rp;

    const auto expected_size = 3;
    rp.accounts.reserve(expected_size);
    for (int i = 0; i < expected_size; ++i) {
        account a;
        a.version = faker::number::integer(1, 100);
        a.recorded_by = std::string(faker::internet::username());
        a.id = boost::uuids::random_generator()();
        a.username = std::string(faker::internet::username());
        a.password_hash = std::string(faker::crypto::sha256());
        a.password_salt = std::string(faker::crypto::sha256());
        a.totp_secret = faker::string::alphanumeric(20);
        a.email = std::string(faker::internet::email());
        rp.accounts.push_back(a);
    }
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.accounts.size() == expected_size);
}

TEST_CASE("list_accounts_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    list_accounts_response e;

    const auto expected_size = 2;
    e.accounts.reserve(expected_size);
    for (int i = 0; i < expected_size; ++i) {
        account a;
        a.version = i + 1;
        a.recorded_by = "user" + std::to_string(i);
        a.id = boost::uuids::random_generator()();
        a.username = "username" + std::to_string(i);
        a.password_hash = std::string(faker::crypto::sha256());
        a.password_salt = std::string(faker::crypto::sha256());
        a.totp_secret = faker::string::alphanumeric(16);
        a.email = "user" + std::to_string(i) + "@example.com";
        e.accounts.push_back(a);
    }
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = list_accounts_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.accounts.size() == e.accounts.size());
    for (size_t i = 0; i < e.accounts.size(); ++i) {
        CHECK(a.accounts[i].version == e.accounts[i].version);
        CHECK(a.accounts[i].username == e.accounts[i].username);
        CHECK(a.accounts[i].email == e.accounts[i].email);
    }
}

TEST_CASE("login_request_with_valid_credentials", tags) {
    auto lg(make_logger(test_suite));

    login_request rq;
    rq.username = "testuser";
    rq.password = "test_password";
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    CHECK(rq.username == "testuser");
    CHECK(rq.password == "test_password");
}

TEST_CASE("login_request_with_faker", tags) {
    auto lg(make_logger(test_suite));

    login_request rq;
    rq.username = std::string(faker::internet::username());
    rq.password = std::string(faker::internet::password());
    BOOST_LOG_SEV(lg, info) << "login_request: " << rq;

    CHECK(!rq.username.empty());
    CHECK(!rq.password.empty());
}

TEST_CASE("login_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    login_request e;
    e.username = std::string(faker::internet::username());
    e.password = std::string(faker::internet::password());
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = login_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.username == e.username);
    CHECK(a.password == e.password);
}

TEST_CASE("login_response_success", tags) {
    auto lg(make_logger(test_suite));

    login_response rp;
    rp.success = true;
    rp.error_message = "";
    rp.account_id = boost::uuids::random_generator()();
    rp.username = std::string(faker::internet::username());
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == true);
    CHECK(rp.error_message.empty());
    CHECK(!rp.account_id.is_nil());
    CHECK(!rp.username.empty());
}

TEST_CASE("login_response_failure", tags) {
    auto lg(make_logger(test_suite));

    login_response rp;
    rp.success = false;
    rp.error_message = "Invalid credentials";
    rp.account_id = boost::uuids::nil_uuid();
    rp.username = "";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == false);
    CHECK(rp.error_message == "Invalid credentials");
    CHECK(rp.account_id.is_nil());
    CHECK(rp.username.empty());
}

TEST_CASE("login_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    login_response e;
    e.success = true;
    e.error_message = "";
    e.account_id = boost::uuids::random_generator()();
    e.username = std::string(faker::internet::username());
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = login_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.error_message == e.error_message);
    CHECK(a.account_id == e.account_id);
    CHECK(a.username == e.username);
}

TEST_CASE("unlock_account_request_with_valid_uuid", tags) {
    auto lg(make_logger(test_suite));

    unlock_account_request rq;
    rq.account_ids.push_back(boost::uuids::random_generator()());
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    CHECK(rq.account_ids.size() == 1);
    CHECK(!rq.account_ids[0].is_nil());
}

TEST_CASE("unlock_account_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    unlock_account_request e;
    e.account_ids.push_back(boost::uuids::random_generator()());
    e.account_ids.push_back(boost::uuids::random_generator()());
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = unlock_account_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.account_ids.size() == e.account_ids.size());
    for (size_t i = 0; i < e.account_ids.size(); ++i) {
        CHECK(a.account_ids[i] == e.account_ids[i]);
    }
}

TEST_CASE("unlock_account_response_success", tags) {
    auto lg(make_logger(test_suite));

    unlock_account_response rp;
    rp.results.push_back({boost::uuids::random_generator()(), true, ""});
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.results.size() == 1);
    CHECK(rp.results[0].success == true);
    CHECK(rp.results[0].message.empty());
}

TEST_CASE("unlock_account_response_failure", tags) {
    auto lg(make_logger(test_suite));

    unlock_account_response rp;
    rp.results.push_back({boost::uuids::random_generator()(), false, "Account not found"});
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.results.size() == 1);
    CHECK(rp.results[0].success == false);
    CHECK(rp.results[0].message == "Account not found");
}

TEST_CASE("unlock_account_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    unlock_account_response e;
    e.results.push_back({boost::uuids::random_generator()(), true, ""});
    e.results.push_back({boost::uuids::random_generator()(), false, "Error occurred"});
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = unlock_account_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.results.size() == e.results.size());
    for (size_t i = 0; i < e.results.size(); ++i) {
        CHECK(a.results[i].account_id == e.results[i].account_id);
        CHECK(a.results[i].success == e.results[i].success);
        CHECK(a.results[i].message == e.results[i].message);
    }
}

TEST_CASE("lock_account_request_with_valid_uuid", tags) {
    auto lg(make_logger(test_suite));

    lock_account_request rq;
    rq.account_ids.push_back(boost::uuids::random_generator()());
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    CHECK(rq.account_ids.size() == 1);
    CHECK(!rq.account_ids[0].is_nil());
}

TEST_CASE("lock_account_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    lock_account_request e;
    e.account_ids.push_back(boost::uuids::random_generator()());
    e.account_ids.push_back(boost::uuids::random_generator()());
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = lock_account_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.account_ids.size() == e.account_ids.size());
    for (size_t i = 0; i < e.account_ids.size(); ++i) {
        CHECK(a.account_ids[i] == e.account_ids[i]);
    }
}

TEST_CASE("lock_account_response_success", tags) {
    auto lg(make_logger(test_suite));

    lock_account_response rp;
    rp.results.push_back({boost::uuids::random_generator()(), true, ""});
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.results.size() == 1);
    CHECK(rp.results[0].success == true);
    CHECK(rp.results[0].message.empty());
}

TEST_CASE("lock_account_response_failure", tags) {
    auto lg(make_logger(test_suite));

    lock_account_response rp;
    rp.results.push_back({boost::uuids::random_generator()(), false, "Account not found"});
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.results.size() == 1);
    CHECK(rp.results[0].success == false);
    CHECK(rp.results[0].message == "Account not found");
}

TEST_CASE("lock_account_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    lock_account_response e;
    e.results.push_back({boost::uuids::random_generator()(), true, ""});
    e.results.push_back({boost::uuids::random_generator()(), false, "Error occurred"});
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = lock_account_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.results.size() == e.results.size());
    for (size_t i = 0; i < e.results.size(); ++i) {
        CHECK(a.results[i].account_id == e.results[i].account_id);
        CHECK(a.results[i].success == e.results[i].success);
        CHECK(a.results[i].message == e.results[i].message);
    }
}

TEST_CASE("create_multiple_random_login_requests", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        login_request rq;
        rq.username = std::string(faker::internet::username());
        rq.password = std::string(faker::internet::password());
        BOOST_LOG_SEV(lg, info) << "Request " << i << ":" << rq;

        CHECK(!rq.username.empty());
        CHECK(!rq.password.empty());
    }
}

TEST_CASE("create_multiple_random_create_account_requests", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        create_account_request rq;
        rq.username = std::string(faker::internet::username());
        rq.password = std::string(faker::internet::password());
        rq.totp_secret = faker::string::alphanumeric(16);
        rq.email = std::string(faker::internet::email());
        rq.recorded_by = std::string(faker::person::firstName()) + " " +
            std::string(faker::person::lastName());
        BOOST_LOG_SEV(lg, info) << "Request " << i << ":" << rq;

        CHECK(!rq.username.empty());
        CHECK(!rq.email.empty());
        CHECK(rq.totp_secret.length() == 16);
    }
}

TEST_CASE("delete_account_request_with_valid_uuid", tags) {
    auto lg(make_logger(test_suite));

    delete_account_request rq;
    rq.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    CHECK(!rq.account_id.is_nil());
}

TEST_CASE("delete_account_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_account_request e;
    e.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_account_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.account_id == e.account_id);
}

TEST_CASE("delete_account_response_success", tags) {
    auto lg(make_logger(test_suite));

    delete_account_response rp;
    rp.success = true;
    rp.message = "Account deleted successfully";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == true);
    CHECK(rp.message == "Account deleted successfully");
}

TEST_CASE("delete_account_response_failure", tags) {
    auto lg(make_logger(test_suite));

    delete_account_response rp;
    rp.success = false;
    rp.message = "Account not found";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == false);
    CHECK(rp.message == "Account not found");
}

TEST_CASE("delete_account_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_account_response e;
    e.success = faker::datatype::boolean();
    e.message = e.success ? "Account deleted successfully" : "Error occurred";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_account_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("logout_request_empty_payload", tags) {
    auto lg(make_logger(test_suite));

    // logout_request is now empty - account_id is determined from session context
    logout_request rq;
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    // Serialization should produce empty buffer
    const auto serialized = rq.serialize();
    CHECK(serialized.empty());
}

TEST_CASE("logout_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    // logout_request is empty - no fields to serialize
    logout_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    CHECK(serialized.empty());

    const auto r = logout_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    // Empty struct deserializes successfully
    (void)a; // Nothing to check, struct is empty
}

TEST_CASE("logout_response_success", tags) {
    auto lg(make_logger(test_suite));

    logout_response rp;
    rp.success = true;
    rp.message = "Logged out successfully";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == true);
    CHECK(rp.message == "Logged out successfully");
}

TEST_CASE("logout_response_failure", tags) {
    auto lg(make_logger(test_suite));

    logout_response rp;
    rp.success = false;
    rp.message = "Account not found";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == false);
    CHECK(rp.message == "Account not found");
}

TEST_CASE("logout_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    logout_response e;
    e.success = faker::datatype::boolean();
    e.message = e.success ? "Logged out successfully" : "Error occurred";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = logout_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("login_response_with_password_reset_required", tags) {
    auto lg(make_logger(test_suite));

    login_response rp;
    rp.success = true;
    rp.error_message = "";
    rp.account_id = boost::uuids::random_generator()();
    rp.username = std::string(faker::internet::username());
    rp.password_reset_required = true;
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == true);
    CHECK(rp.password_reset_required == true);
}

TEST_CASE("login_response_serialize_deserialize_with_password_reset", tags) {
    auto lg(make_logger(test_suite));

    login_response e;
    e.success = true;
    e.error_message = "";
    e.account_id = boost::uuids::random_generator()();
    e.username = std::string(faker::internet::username());
    e.password_reset_required = faker::datatype::boolean();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = login_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.error_message == e.error_message);
    CHECK(a.account_id == e.account_id);
    CHECK(a.username == e.username);
    CHECK(a.password_reset_required == e.password_reset_required);
}

TEST_CASE("reset_password_request_with_valid_uuids", tags) {
    auto lg(make_logger(test_suite));

    reset_password_request rq;
    rq.account_ids.push_back(boost::uuids::random_generator()());
    rq.account_ids.push_back(boost::uuids::random_generator()());
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    CHECK(rq.account_ids.size() == 2);
    CHECK(!rq.account_ids[0].is_nil());
    CHECK(!rq.account_ids[1].is_nil());
}

TEST_CASE("reset_password_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    reset_password_request e;
    e.account_ids.push_back(boost::uuids::random_generator()());
    e.account_ids.push_back(boost::uuids::random_generator()());
    e.account_ids.push_back(boost::uuids::random_generator()());
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = reset_password_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.account_ids.size() == e.account_ids.size());
    for (size_t i = 0; i < e.account_ids.size(); ++i) {
        CHECK(a.account_ids[i] == e.account_ids[i]);
    }
}

TEST_CASE("reset_password_response_success", tags) {
    auto lg(make_logger(test_suite));

    reset_password_response rp;
    rp.results.push_back({boost::uuids::random_generator()(), true, ""});
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.results.size() == 1);
    CHECK(rp.results[0].success == true);
    CHECK(rp.results[0].message.empty());
}

TEST_CASE("reset_password_response_mixed_results", tags) {
    auto lg(make_logger(test_suite));

    reset_password_response rp;
    rp.results.push_back({boost::uuids::random_generator()(), true, ""});
    rp.results.push_back({boost::uuids::random_generator()(), false, "Account not found"});
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.results.size() == 2);
    CHECK(rp.results[0].success == true);
    CHECK(rp.results[1].success == false);
    CHECK(rp.results[1].message == "Account not found");
}

TEST_CASE("reset_password_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    reset_password_response e;
    e.results.push_back({boost::uuids::random_generator()(), true, ""});
    e.results.push_back({boost::uuids::random_generator()(), false, "Error occurred"});
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = reset_password_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.results.size() == e.results.size());
    for (size_t i = 0; i < e.results.size(); ++i) {
        CHECK(a.results[i].account_id == e.results[i].account_id);
        CHECK(a.results[i].success == e.results[i].success);
        CHECK(a.results[i].message == e.results[i].message);
    }
}

TEST_CASE("change_password_request_with_valid_password", tags) {
    auto lg(make_logger(test_suite));

    change_password_request rq;
    rq.new_password = std::string(faker::internet::password(12));
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    CHECK(!rq.new_password.empty());
    CHECK(rq.new_password.length() >= 8);
}

TEST_CASE("change_password_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    change_password_request e;
    e.new_password = std::string(faker::internet::password(16));
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = change_password_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.new_password == e.new_password);
}

TEST_CASE("change_password_response_success", tags) {
    auto lg(make_logger(test_suite));

    change_password_response rp;
    rp.success = true;
    rp.message = "Password changed successfully";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == true);
    CHECK(rp.message == "Password changed successfully");
}

TEST_CASE("change_password_response_failure", tags) {
    auto lg(make_logger(test_suite));

    change_password_response rp;
    rp.success = false;
    rp.message = "Password must be at least 8 characters long";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == false);
    CHECK(!rp.message.empty());
}

TEST_CASE("change_password_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    change_password_response e;
    e.success = faker::datatype::boolean();
    e.message = e.success ? "Password changed successfully" : "Error occurred";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = change_password_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}
