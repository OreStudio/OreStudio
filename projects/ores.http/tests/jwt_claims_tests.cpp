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
#include "ores.http/domain/jwt_claims.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.telemetry/log/make_logger.hpp"

namespace {

const std::string test_suite("ores.http.tests");
const std::string tags("[jwt_claims]");

}

using namespace ores::http::domain;
using namespace ores::telemetry::log;

TEST_CASE("jwt_claims_default_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing jwt_claims default construction";

    jwt_claims sut;

    CHECK(sut.subject.empty());
    CHECK(sut.issuer.empty());
    CHECK(sut.audience.empty());
    CHECK(sut.roles.empty());
    CHECK_FALSE(sut.username.has_value());
    CHECK_FALSE(sut.email.has_value());
    CHECK_FALSE(sut.session_id.has_value());
    CHECK_FALSE(sut.session_start_time.has_value());
}

TEST_CASE("jwt_claims_with_required_fields", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing jwt_claims with required fields";

    jwt_claims sut;
    sut.subject = "user-123";
    sut.issuer = "ores";
    sut.audience = "ores-api";

    auto now = std::chrono::system_clock::now();
    sut.issued_at = now;
    sut.expires_at = now + std::chrono::hours(1);

    CHECK(sut.subject == "user-123");
    CHECK(sut.issuer == "ores");
    CHECK(sut.audience == "ores-api");
    CHECK(sut.expires_at > sut.issued_at);
}

TEST_CASE("jwt_claims_with_roles", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing jwt_claims with roles";

    jwt_claims sut;
    sut.subject = "admin-user";
    sut.roles = {"admin", "user", "moderator"};

    REQUIRE(sut.roles.size() == 3);
    CHECK(sut.roles[0] == "admin");
    CHECK(sut.roles[1] == "user");
    CHECK(sut.roles[2] == "moderator");
}

TEST_CASE("jwt_claims_with_optional_fields", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing jwt_claims with optional fields";

    jwt_claims sut;
    sut.subject = "user-456";
    sut.username = "johndoe";
    sut.email = "john@example.com";
    sut.session_id = "session-789";

    REQUIRE(sut.username.has_value());
    CHECK(*sut.username == "johndoe");

    REQUIRE(sut.email.has_value());
    CHECK(*sut.email == "john@example.com");

    REQUIRE(sut.session_id.has_value());
    CHECK(*sut.session_id == "session-789");
}

TEST_CASE("jwt_claims_with_session_start_time", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing jwt_claims with session start time";

    jwt_claims sut;
    sut.subject = "user-789";
    sut.session_id = "session-abc";
    sut.session_start_time = std::chrono::system_clock::now();

    REQUIRE(sut.session_start_time.has_value());
    CHECK(*sut.session_start_time <= std::chrono::system_clock::now());
}

TEST_CASE("jwt_claims_expired_check", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing jwt_claims expiration timestamp";

    jwt_claims sut;
    auto now = std::chrono::system_clock::now();
    sut.expires_at = now - std::chrono::hours(1);

    CHECK(sut.expires_at < now);
}

TEST_CASE("jwt_claims_not_expired_check", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing jwt_claims not expired";

    jwt_claims sut;
    auto now = std::chrono::system_clock::now();
    sut.expires_at = now + std::chrono::hours(1);

    CHECK(sut.expires_at > now);
}
