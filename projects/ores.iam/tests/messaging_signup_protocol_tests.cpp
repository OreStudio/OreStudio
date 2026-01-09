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
#include "ores.iam/messaging/signup_protocol.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[messaging][signup]");

}

using namespace ores::iam::messaging;
using namespace ores::logging;

TEST_CASE("signup_request_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    signup_request rq;
    rq.username = "newuser";
    rq.email = "newuser@example.com";
    rq.password = "SecurePassword123!";
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    CHECK(rq.username == "newuser");
    CHECK(rq.email == "newuser@example.com");
    CHECK(rq.password == "SecurePassword123!");
}

TEST_CASE("signup_request_with_faker", tags) {
    auto lg(make_logger(test_suite));

    signup_request rq;
    rq.username = std::string(faker::internet::username());
    rq.email = std::string(faker::internet::email());
    rq.password = std::string(faker::internet::password(16));
    BOOST_LOG_SEV(lg, info) << "signup_request: " << rq;

    CHECK(!rq.username.empty());
    CHECK(!rq.email.empty());
    CHECK(!rq.password.empty());
    CHECK(rq.password.length() >= 16);
}

TEST_CASE("signup_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    signup_request e;
    e.username = std::string(faker::internet::username());
    e.email = std::string(faker::internet::email());
    e.password = std::string(faker::internet::password(16));
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

TEST_CASE("signup_response_success", tags) {
    auto lg(make_logger(test_suite));

    signup_response rp;
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

TEST_CASE("signup_response_failure_signup_disabled", tags) {
    auto lg(make_logger(test_suite));

    signup_response rp;
    rp.success = false;
    rp.error_message = "User registration is currently disabled";
    rp.account_id = boost::uuids::nil_uuid();
    rp.username = "";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == false);
    CHECK(rp.error_message == "User registration is currently disabled");
    CHECK(rp.account_id.is_nil());
    CHECK(rp.username.empty());
}

TEST_CASE("signup_response_failure_username_taken", tags) {
    auto lg(make_logger(test_suite));

    signup_response rp;
    rp.success = false;
    rp.error_message = "Username is already taken";
    rp.account_id = boost::uuids::nil_uuid();
    rp.username = "existinguser";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == false);
    CHECK(rp.error_message == "Username is already taken");
}

TEST_CASE("signup_response_failure_email_taken", tags) {
    auto lg(make_logger(test_suite));

    signup_response rp;
    rp.success = false;
    rp.error_message = "Email address is already registered";
    rp.account_id = boost::uuids::nil_uuid();
    rp.username = "";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == false);
    CHECK(rp.error_message == "Email address is already registered");
}

TEST_CASE("signup_response_failure_weak_password", tags) {
    auto lg(make_logger(test_suite));

    signup_response rp;
    rp.success = false;
    rp.error_message = "Password must be at least 12 characters long";
    rp.account_id = boost::uuids::nil_uuid();
    rp.username = "";
    BOOST_LOG_SEV(lg, info) << "Response: " << rp;

    CHECK(rp.success == false);
    CHECK(!rp.error_message.empty());
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

TEST_CASE("signup_response_serialize_deserialize_with_error", tags) {
    auto lg(make_logger(test_suite));

    signup_response e;
    e.success = false;
    e.error_message = "Username is already taken";
    e.account_id = boost::uuids::nil_uuid();
    e.username = "testuser";
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

TEST_CASE("create_multiple_random_signup_requests", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        signup_request rq;
        rq.username = std::string(faker::internet::username());
        rq.email = std::string(faker::internet::email());
        rq.password = std::string(faker::internet::password(16));
        BOOST_LOG_SEV(lg, info) << "Request " << i << ": " << rq;

        CHECK(!rq.username.empty());
        CHECK(!rq.email.empty());
        CHECK(!rq.password.empty());

        // Verify round-trip serialization
        const auto serialized = rq.serialize();
        const auto deserialized = signup_request::deserialize(serialized);
        REQUIRE(deserialized.has_value());
        CHECK(deserialized->username == rq.username);
        CHECK(deserialized->email == rq.email);
        CHECK(deserialized->password == rq.password);
    }
}
