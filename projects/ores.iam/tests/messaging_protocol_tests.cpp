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
#include "ores.iam/messaging/tenant_protocol.hpp"
#include "ores.iam/messaging/tenant_type_protocol.hpp"
#include "ores.iam/messaging/tenant_status_protocol.hpp"
#include "ores.iam/messaging/account_party_protocol.hpp"
#include "ores.iam/messaging/authorization_protocol.hpp"
#include "ores.iam/messaging/session_protocol.hpp"
#include "ores.iam/messaging/signup_protocol.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.iam/generators/account_party_generator.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[messaging]");

}

using namespace ores::iam::messaging;
using ores::iam::domain::account;
using namespace ores::logging;

TEST_CASE("save_account_request_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    save_account_request rq;
    rq.principal = "testuser";
    rq.password = "test_password";
    rq.totp_secret = "JBSWY3DPEHPK3PXP";
    rq.email = "test@example.com";
    BOOST_LOG_SEV(lg, info) << "Rrequest: " << rq;

    CHECK(rq.principal == "testuser");
    CHECK(rq.password == "test_password");
    CHECK(rq.totp_secret == "JBSWY3DPEHPK3PXP");
    CHECK(rq.email == "test@example.com");
}

TEST_CASE("save_account_request_with_faker", tags) {
    auto lg(make_logger(test_suite));

    save_account_request rq;
    rq.principal = std::string(faker::internet::username());
    rq.password = std::string(faker::internet::password());
    rq.totp_secret = faker::string::alphanumeric(16);
    rq.email = std::string(faker::internet::email());
    BOOST_LOG_SEV(lg, info) << "save_account_request: " << rq;

    CHECK(!rq.principal.empty());
    CHECK(!rq.password.empty());
    CHECK(rq.totp_secret.length() == 16);
    CHECK(!rq.email.empty());
}

TEST_CASE("save_account_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_account_request e;
    e.principal = std::string(faker::internet::username());
    e.password = std::string(faker::internet::password());
    e.totp_secret = faker::string::alphanumeric(20);
    e.email = std::string(faker::internet::email());
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_account_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.principal == e.principal);
    CHECK(a.password == e.password);
    CHECK(a.totp_secret == e.totp_secret);
    CHECK(a.email == e.email);
}

TEST_CASE("save_account_response_with_valid_uuid", tags) {
    auto lg(make_logger(test_suite));

    save_account_response rp;
    rp.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(!rp.account_id.is_nil());
}

TEST_CASE("save_account_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_account_response e;
    e.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_account_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.account_id == e.account_id);
}

TEST_CASE("get_accounts_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_accounts_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_accounts_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
}

TEST_CASE("get_accounts_response_with_faker", tags) {
    auto lg(make_logger(test_suite));

    get_accounts_response rp;

    const auto expected_size = 3;
    rp.accounts.reserve(expected_size);
    for (int i = 0; i < expected_size; ++i) {
        account a;
        a.version = faker::number::integer(1, 100);
        a.modified_by = std::string(faker::internet::username());
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

TEST_CASE("get_accounts_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_accounts_response e;

    const auto expected_size = 2;
    e.accounts.reserve(expected_size);
    for (int i = 0; i < expected_size; ++i) {
        account a;
        a.version = i + 1;
        a.modified_by = "user" + std::to_string(i);
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
    const auto r = get_accounts_response::deserialize(serialized);

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
    rq.principal = "testuser";
    rq.password = "test_password";
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    CHECK(rq.principal == "testuser");
    CHECK(rq.password == "test_password");
}

TEST_CASE("login_request_with_faker", tags) {
    auto lg(make_logger(test_suite));

    login_request rq;
    rq.principal = std::string(faker::internet::username());
    rq.password = std::string(faker::internet::password());
    BOOST_LOG_SEV(lg, info) << "login_request: " << rq;

    CHECK(!rq.principal.empty());
    CHECK(!rq.password.empty());
}

TEST_CASE("login_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    login_request e;
    e.principal = std::string(faker::internet::username());
    e.password = std::string(faker::internet::password());
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = login_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.principal == e.principal);
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
    CHECK(a.tenant_bootstrap_mode == false);
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
        rq.principal = std::string(faker::internet::username());
        rq.password = std::string(faker::internet::password());
        BOOST_LOG_SEV(lg, info) << "Request " << i << ":" << rq;

        CHECK(!rq.principal.empty());
        CHECK(!rq.password.empty());
    }
}

TEST_CASE("create_multiple_random_save_account_requests", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        save_account_request rq;
        rq.principal = std::string(faker::internet::username());
        rq.password = std::string(faker::internet::password());
        rq.totp_secret = faker::string::alphanumeric(16);
        rq.email = std::string(faker::internet::email());
        BOOST_LOG_SEV(lg, info) << "Request " << i << ":" << rq;

        CHECK(!rq.principal.empty());
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
    CHECK(a.tenant_bootstrap_mode == e.tenant_bootstrap_mode);
}

TEST_CASE("login_response_with_tenant_bootstrap_mode", tags) {
    auto lg(make_logger(test_suite));

    login_response rp;
    rp.success = true;
    rp.error_message = "";
    rp.account_id = boost::uuids::random_generator()();
    rp.username = std::string(faker::internet::username());
    rp.tenant_bootstrap_mode = true;
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == true);
    CHECK(rp.tenant_bootstrap_mode == true);
}

TEST_CASE("login_response_serialize_deserialize_with_tenant_bootstrap", tags) {
    auto lg(make_logger(test_suite));

    login_response e;
    e.success = true;
    e.error_message = "";
    e.account_id = boost::uuids::random_generator()();
    e.username = std::string(faker::internet::username());
    e.password_reset_required = false;
    e.tenant_bootstrap_mode = true;
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
    CHECK(a.tenant_bootstrap_mode == e.tenant_bootstrap_mode);
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

// ============================================================================
// Tenant Protocol
// ============================================================================

TEST_CASE("get_tenants_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_tenants_request e;
    e.include_deleted = false;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_tenants_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.include_deleted == e.include_deleted);
}

TEST_CASE("get_tenants_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_tenants_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_tenants_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.tenants.size() == e.tenants.size());
}

TEST_CASE("save_tenant_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_tenant_request e;
    e.tenant.version = 1;
    e.tenant.id = boost::uuids::random_generator()();
    e.tenant.name = "Test Tenant";
    e.tenant.code = "TEST";
    e.tenant.hostname = "test.example.com";
    e.tenant.description = "A test tenant";
    e.tenant.status = "active";
    e.tenant.type = "standard";
    e.tenant.modified_by = "admin";
    e.tenant.change_commentary = "Initial creation";
    e.tenant.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_tenant_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.tenant.version == e.tenant.version);
    CHECK(a.tenant.id == e.tenant.id);
    CHECK(a.tenant.name == e.tenant.name);
    CHECK(a.tenant.code == e.tenant.code);
    CHECK(a.tenant.hostname == e.tenant.hostname);
    CHECK(a.tenant.description == e.tenant.description);
    CHECK(a.tenant.status == e.tenant.status);
    CHECK(a.tenant.type == e.tenant.type);
    CHECK(a.tenant.change_commentary == e.tenant.change_commentary);
}

TEST_CASE("save_tenant_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_tenant_response e;
    e.success = true;
    e.message = "Tenant saved successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_tenant_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("delete_tenant_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_tenant_request e;
    e.ids.push_back(boost::uuids::random_generator()());
    e.ids.push_back(boost::uuids::random_generator()());
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_tenant_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.ids.size() == e.ids.size());
    for (size_t i = 0; i < e.ids.size(); ++i) {
        CHECK(a.ids[i] == e.ids[i]);
    }
}

TEST_CASE("delete_tenant_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_tenant_response e;
    e.results.push_back({boost::uuids::random_generator()(), true, ""});
    e.results.push_back({boost::uuids::random_generator()(), false, "Tenant not found"});
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_tenant_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.results.size() == e.results.size());
    for (size_t i = 0; i < e.results.size(); ++i) {
        CHECK(a.results[i].id == e.results[i].id);
        CHECK(a.results[i].success == e.results[i].success);
        CHECK(a.results[i].message == e.results[i].message);
    }
}

TEST_CASE("get_tenant_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_tenant_history_request e;
    e.id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_tenant_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.id == e.id);
}

TEST_CASE("get_tenant_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_tenant_history_response e;
    e.success = true;
    e.message = "History retrieved";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_tenant_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

TEST_CASE("provision_tenant_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    provision_tenant_request e;
    e.type = "organisation";
    e.code = "ACME";
    e.name = "Acme Corp";
    e.hostname = "acme.example.com";
    e.description = "Acme Corporation tenant";
    e.admin_username = "admin";
    e.admin_password = "secret123";
    e.admin_email = "admin@acme.com";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = provision_tenant_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.type == e.type);
    CHECK(a.code == e.code);
    CHECK(a.name == e.name);
    CHECK(a.hostname == e.hostname);
    CHECK(a.description == e.description);
    CHECK(a.admin_username == e.admin_username);
    CHECK(a.admin_password == e.admin_password);
    CHECK(a.admin_email == e.admin_email);
}

TEST_CASE("provision_tenant_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    provision_tenant_response e;
    e.success = true;
    e.error_message = "";
    e.tenant_id = "some-tenant-uuid";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = provision_tenant_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.error_message == e.error_message);
    CHECK(a.tenant_id == e.tenant_id);
}

// ============================================================================
// Tenant Type Protocol
// ============================================================================

TEST_CASE("get_tenant_types_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_tenant_types_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_tenant_types_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
}

TEST_CASE("get_tenant_types_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_tenant_types_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_tenant_types_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.types.size() == e.types.size());
}

TEST_CASE("save_tenant_type_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_tenant_type_request e;
    e.type.version = 1;
    e.type.type = "standard";
    e.type.name = "Standard";
    e.type.description = "Standard tenant";
    e.type.modified_by = "admin";
    e.type.change_commentary = "test";
    e.type.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_tenant_type_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.type.version == e.type.version);
    CHECK(a.type.type == e.type.type);
    CHECK(a.type.name == e.type.name);
    CHECK(a.type.description == e.type.description);
    CHECK(a.type.change_commentary == e.type.change_commentary);
}

TEST_CASE("save_tenant_type_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_tenant_type_response e;
    e.success = true;
    e.message = "Tenant type saved";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_tenant_type_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("delete_tenant_type_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_tenant_type_request e;
    e.types.push_back("standard");
    e.types.push_back("sandbox");
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_tenant_type_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.types.size() == e.types.size());
    for (size_t i = 0; i < e.types.size(); ++i) {
        CHECK(a.types[i] == e.types[i]);
    }
}

TEST_CASE("delete_tenant_type_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_tenant_type_response e;
    e.results.push_back({"standard", true, ""});
    e.results.push_back({"sandbox", false, "Type in use"});
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_tenant_type_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.results.size() == e.results.size());
    for (size_t i = 0; i < e.results.size(); ++i) {
        CHECK(a.results[i].type == e.results[i].type);
        CHECK(a.results[i].success == e.results[i].success);
        CHECK(a.results[i].message == e.results[i].message);
    }
}

TEST_CASE("get_tenant_type_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_tenant_type_history_request e;
    e.type = "standard";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_tenant_type_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.type == e.type);
}

TEST_CASE("get_tenant_type_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_tenant_type_history_response e;
    e.success = true;
    e.message = "History retrieved";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_tenant_type_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

// ============================================================================
// Tenant Status Protocol
// ============================================================================

TEST_CASE("get_tenant_statuses_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_tenant_statuses_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_tenant_statuses_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
}

TEST_CASE("get_tenant_statuses_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_tenant_statuses_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_tenant_statuses_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.statuses.size() == e.statuses.size());
}

TEST_CASE("save_tenant_status_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_tenant_status_request e;
    e.status.version = 1;
    e.status.status = "active";
    e.status.name = "Active";
    e.status.description = "Active tenant";
    e.status.modified_by = "admin";
    e.status.change_commentary = "test";
    e.status.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_tenant_status_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.status.version == e.status.version);
    CHECK(a.status.status == e.status.status);
    CHECK(a.status.name == e.status.name);
    CHECK(a.status.description == e.status.description);
    CHECK(a.status.change_commentary == e.status.change_commentary);
}

TEST_CASE("save_tenant_status_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_tenant_status_response e;
    e.success = true;
    e.message = "Tenant status saved";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_tenant_status_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("delete_tenant_status_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_tenant_status_request e;
    e.statuses.push_back("active");
    e.statuses.push_back("suspended");
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_tenant_status_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.statuses.size() == e.statuses.size());
    for (size_t i = 0; i < e.statuses.size(); ++i) {
        CHECK(a.statuses[i] == e.statuses[i]);
    }
}

TEST_CASE("delete_tenant_status_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_tenant_status_response e;
    e.results.push_back({"active", true, ""});
    e.results.push_back({"suspended", false, "Status in use"});
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_tenant_status_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.results.size() == e.results.size());
    for (size_t i = 0; i < e.results.size(); ++i) {
        CHECK(a.results[i].status == e.results[i].status);
        CHECK(a.results[i].success == e.results[i].success);
        CHECK(a.results[i].message == e.results[i].message);
    }
}

TEST_CASE("get_tenant_status_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_tenant_status_history_request e;
    e.status = "active";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_tenant_status_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.status == e.status);
}

TEST_CASE("get_tenant_status_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_tenant_status_history_response e;
    e.success = true;
    e.message = "History retrieved";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_tenant_status_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

// ============================================================================
// Account Party Protocol
// ============================================================================

TEST_CASE("get_account_parties_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_account_parties_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_account_parties_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
}

TEST_CASE("get_account_parties_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_account_parties_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_account_parties_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.account_parties.size() == e.account_parties.size());
}

TEST_CASE("get_account_parties_by_account_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_account_parties_by_account_request e;
    e.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_account_parties_by_account_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.account_id == e.account_id);
}

TEST_CASE("get_account_parties_by_account_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_account_parties_by_account_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_account_parties_by_account_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.account_parties.size() == e.account_parties.size());
}

TEST_CASE("save_account_party_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    ores::utility::generation::generation_context ctx;
    save_account_party_request e;
    e.account_party = ores::iam::generators::generate_synthetic_account_party(ctx);
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_account_party_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.account_party.version == e.account_party.version);
    CHECK(a.account_party.account_id == e.account_party.account_id);
    CHECK(a.account_party.party_id == e.account_party.party_id);
}

TEST_CASE("save_account_party_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_account_party_response e;
    e.success = true;
    e.message = "Account party saved";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_account_party_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("delete_account_party_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_account_party_request e;
    account_party_key k1;
    k1.account_id = boost::uuids::random_generator()();
    k1.party_id = boost::uuids::random_generator()();
    account_party_key k2;
    k2.account_id = boost::uuids::random_generator()();
    k2.party_id = boost::uuids::random_generator()();
    e.keys.push_back(k1);
    e.keys.push_back(k2);
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_account_party_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.keys.size() == e.keys.size());
    for (size_t i = 0; i < e.keys.size(); ++i) {
        CHECK(a.keys[i].account_id == e.keys[i].account_id);
        CHECK(a.keys[i].party_id == e.keys[i].party_id);
    }
}

TEST_CASE("delete_account_party_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_account_party_response e;
    e.results.push_back({boost::uuids::random_generator()(),
        boost::uuids::random_generator()(), true, ""});
    e.results.push_back({boost::uuids::random_generator()(),
        boost::uuids::random_generator()(), false, "Not found"});
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_account_party_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.results.size() == e.results.size());
    for (size_t i = 0; i < e.results.size(); ++i) {
        CHECK(a.results[i].account_id == e.results[i].account_id);
        CHECK(a.results[i].party_id == e.results[i].party_id);
        CHECK(a.results[i].success == e.results[i].success);
        CHECK(a.results[i].message == e.results[i].message);
    }
}

// ============================================================================
// Authorization Protocol
// ============================================================================

TEST_CASE("list_roles_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    list_roles_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = list_roles_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
}

TEST_CASE("list_roles_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    list_roles_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = list_roles_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.roles.size() == e.roles.size());
}

TEST_CASE("list_permissions_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    list_permissions_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = list_permissions_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
}

TEST_CASE("list_permissions_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    list_permissions_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = list_permissions_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.permissions.size() == e.permissions.size());
}

TEST_CASE("assign_role_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    assign_role_request e;
    e.account_id = boost::uuids::random_generator()();
    e.role_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = assign_role_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.account_id == e.account_id);
    CHECK(a.role_id == e.role_id);
}

TEST_CASE("assign_role_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    assign_role_response e;
    e.success = true;
    e.error_message = "";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = assign_role_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.error_message == e.error_message);
}

TEST_CASE("revoke_role_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    revoke_role_request e;
    e.account_id = boost::uuids::random_generator()();
    e.role_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = revoke_role_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.account_id == e.account_id);
    CHECK(a.role_id == e.role_id);
}

TEST_CASE("revoke_role_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    revoke_role_response e;
    e.success = false;
    e.error_message = "Role not assigned";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = revoke_role_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.error_message == e.error_message);
}

TEST_CASE("get_account_roles_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_account_roles_request e;
    e.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_account_roles_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.account_id == e.account_id);
}

TEST_CASE("get_account_roles_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_account_roles_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_account_roles_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.roles.size() == e.roles.size());
}

TEST_CASE("get_account_permissions_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_account_permissions_request e;
    e.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_account_permissions_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.account_id == e.account_id);
}

TEST_CASE("get_account_permissions_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_account_permissions_response e;
    e.permission_codes.push_back("iam::accounts:read");
    e.permission_codes.push_back("iam::accounts:create");
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_account_permissions_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.permission_codes.size() == e.permission_codes.size());
    for (size_t i = 0; i < e.permission_codes.size(); ++i) {
        CHECK(a.permission_codes[i] == e.permission_codes[i]);
    }
}

TEST_CASE("get_role_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_role_request e;
    e.identifier = "SuperAdmin";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_role_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.identifier == e.identifier);
}

TEST_CASE("get_role_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_role_response e;
    e.found = false;
    e.error_message = "Role not found";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_role_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.found == e.found);
    CHECK(a.error_message == e.error_message);
}

TEST_CASE("suggest_role_commands_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    suggest_role_commands_request e;
    e.username = "admin";
    e.hostname = "localhost";
    e.tenant_id = "ffffffff-ffff-ffff-ffff-ffffffffffff";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = suggest_role_commands_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.username == e.username);
    CHECK(a.hostname == e.hostname);
    CHECK(a.tenant_id == e.tenant_id);
}

TEST_CASE("suggest_role_commands_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    suggest_role_commands_response e;
    e.commands.push_back("assign-role admin@localhost SuperAdmin");
    e.commands.push_back("assign-role admin@localhost Trading");
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = suggest_role_commands_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.commands.size() == e.commands.size());
    for (size_t i = 0; i < e.commands.size(); ++i) {
        CHECK(a.commands[i] == e.commands[i]);
    }
}

// ============================================================================
// Session Protocol
// ============================================================================

TEST_CASE("list_sessions_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    list_sessions_request e;
    e.account_id = boost::uuids::random_generator()();
    e.limit = 100;
    e.offset = 0;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = list_sessions_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.account_id == e.account_id);
    CHECK(a.limit == e.limit);
    CHECK(a.offset == e.offset);
}

TEST_CASE("list_sessions_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    list_sessions_response e;
    e.total_count = 0;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = list_sessions_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.sessions.size() == e.sessions.size());
    CHECK(a.total_count == e.total_count);
}

TEST_CASE("get_session_statistics_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_session_statistics_request e;
    e.account_id = boost::uuids::random_generator()();
    e.start_time = std::chrono::system_clock::now() - std::chrono::hours(24);
    e.end_time = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_session_statistics_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.account_id == e.account_id);
}

TEST_CASE("get_session_statistics_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_session_statistics_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_session_statistics_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.statistics.size() == e.statistics.size());
}

TEST_CASE("get_active_sessions_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_active_sessions_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_active_sessions_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
}

TEST_CASE("get_active_sessions_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_active_sessions_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_active_sessions_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.sessions.size() == e.sessions.size());
}

// ============================================================================
// Signup Protocol
// ============================================================================

TEST_CASE("signup_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    signup_request e;
    e.username = std::string(faker::internet::username());
    e.email = std::string(faker::internet::email());
    e.password = std::string(faker::internet::password());
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = signup_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.username == e.username);
    CHECK(a.email == e.email);
    CHECK(a.password == e.password);
}

TEST_CASE("signup_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    signup_response e;
    e.success = true;
    e.error_message = "";
    e.account_id = boost::uuids::random_generator()();
    e.username = std::string(faker::internet::username());
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = signup_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.error_message == e.error_message);
    CHECK(a.account_id == e.account_id);
    CHECK(a.username == e.username);
}

// ============================================================================
// Login Protocol (untested messages)
// ============================================================================

TEST_CASE("list_login_info_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    list_login_info_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = list_login_info_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
}

TEST_CASE("list_login_info_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    list_login_info_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = list_login_info_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.login_infos.size() == e.login_infos.size());
}

// ============================================================================
// Account Protocol (untested messages)
// ============================================================================

TEST_CASE("update_my_email_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    update_my_email_request e;
    e.new_email = std::string(faker::internet::email());
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = update_my_email_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.new_email == e.new_email);
}

TEST_CASE("update_my_email_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    update_my_email_response e;
    e.success = true;
    e.message = "Email updated successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = update_my_email_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

// ============================================================================
// Bootstrap Protocol
// ============================================================================

TEST_CASE("bootstrap_status_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    bootstrap_status_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = bootstrap_status_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
}

TEST_CASE("bootstrap_status_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    bootstrap_status_response e;
    e.is_in_bootstrap_mode = true;
    e.message = "System is in bootstrap mode";
    bootstrap_bundle_info bundle;
    bundle.code = "lei_small";
    bundle.name = "Small LEI Dataset";
    bundle.description = "A small LEI dataset for testing";
    e.available_bundles.push_back(bundle);
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = bootstrap_status_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.is_in_bootstrap_mode == e.is_in_bootstrap_mode);
    CHECK(a.message == e.message);
    REQUIRE(a.available_bundles.size() == e.available_bundles.size());
    CHECK(a.available_bundles[0].code == e.available_bundles[0].code);
    CHECK(a.available_bundles[0].name == e.available_bundles[0].name);
    CHECK(a.available_bundles[0].description == e.available_bundles[0].description);
}

TEST_CASE("create_initial_admin_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    create_initial_admin_request e;
    e.principal = "admin@localhost";
    e.password = "secure_password_123";
    e.email = "admin@example.com";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = create_initial_admin_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.principal == e.principal);
    CHECK(a.password == e.password);
    CHECK(a.email == e.email);
}

TEST_CASE("create_initial_admin_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    create_initial_admin_response e;
    e.success = true;
    e.error_message = "";
    e.account_id = boost::uuids::random_generator()();
    e.tenant_id = ores::utility::uuid::tenant_id::system();
    e.tenant_name = "System";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = create_initial_admin_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.error_message == e.error_message);
    CHECK(a.account_id == e.account_id);
    CHECK(a.tenant_id == e.tenant_id);
    CHECK(a.tenant_name == e.tenant_name);
}
