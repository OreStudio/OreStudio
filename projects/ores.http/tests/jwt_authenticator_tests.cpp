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
#include "ores.http/middleware/jwt_authenticator.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.telemetry/log/make_logger.hpp"

namespace {

const std::string test_suite("ores.http.tests");
const std::string tags("[jwt_authenticator]");
const std::string test_secret("super-secret-key-for-testing-purposes-only-32bytes!");
const std::string test_issuer("test-issuer");
const std::string test_audience("test-audience");

}

using namespace ores::http::middleware;
using namespace ores::http::domain;
using namespace ores::telemetry::log;

TEST_CASE("jwt_authenticator_create_hs256_configured", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing HS256 authenticator creation";

    auto auth = jwt_authenticator::create_hs256(test_secret, test_issuer, test_audience);

    REQUIRE(auth.is_configured());
}

TEST_CASE("jwt_authenticator_create_hs256_empty_secret_not_configured", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing HS256 with empty secret";

    auto auth = jwt_authenticator::create_hs256("", test_issuer, test_audience);

    REQUIRE_FALSE(auth.is_configured());
}

TEST_CASE("jwt_authenticator_create_rs256_configured", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing RS256 authenticator creation";

    // A minimal RSA public key for testing (not for production use)
    const std::string test_public_key = R"(-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu1SU1LfVLPHCozMxH2Mo
4lgOEePzNm0tRgeLezV6ffAt0gunVTLw7onLRnrq0/IzW7yWR7QkrmBL7jTKEn5u
+qKhbwKfBstIs+bMY2Zkp18gnTxKLxoS2tFczGkPLPgizskuemMghRniWaoLcyeh
kd3qqGElvW/VDL5AaWTg0nLVkjRo9z+40RQzuVaE8AkAFmxZzow3x+VJYKdjykkJ
0iT9wCS0DRTXu269V264Vf/3jvredZiKRkgwlL9xNAwxXFg0x/XFw005UWVRIkdg
cKWTjpBP2dPwVZ4WWC+9aGVd+Gyn1o0CLelf4rEjGoXbAAEgAqeGUxrcIlbjXfbc
mwIDAQAB
-----END PUBLIC KEY-----)";

    auto auth = jwt_authenticator::create_rs256(test_public_key, test_issuer, test_audience);

    REQUIRE(auth.is_configured());
}

TEST_CASE("jwt_authenticator_create_rs256_empty_key_not_configured", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing RS256 with empty key";

    auto auth = jwt_authenticator::create_rs256("", test_issuer, test_audience);

    REQUIRE_FALSE(auth.is_configured());
}

TEST_CASE("jwt_authenticator_create_and_validate_token", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing token creation and validation";

    auto auth = jwt_authenticator::create_hs256(test_secret, test_issuer, test_audience);

    jwt_claims claims;
    claims.subject = "user123";
    claims.issuer = test_issuer;
    claims.audience = test_audience;
    claims.issued_at = std::chrono::system_clock::now();
    claims.expires_at = std::chrono::system_clock::now() + std::chrono::hours(1);
    claims.roles = {"admin", "user"};
    claims.username = "testuser";
    claims.email = "test@example.com";

    auto token = auth.create_token(claims);
    REQUIRE(token.has_value());
    REQUIRE_FALSE(token->empty());

    auto validated = auth.validate(*token);
    REQUIRE(validated.has_value());
    REQUIRE(validated->subject == "user123");
    REQUIRE(validated->roles.size() == 2);
    REQUIRE(validated->roles[0] == "admin");
    REQUIRE(validated->roles[1] == "user");
    REQUIRE(validated->username.has_value());
    REQUIRE(*validated->username == "testuser");
    REQUIRE(validated->email.has_value());
    REQUIRE(*validated->email == "test@example.com");
}

TEST_CASE("jwt_authenticator_validate_expired_token", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing expired token validation";

    auto auth = jwt_authenticator::create_hs256(test_secret, test_issuer, test_audience);

    jwt_claims claims;
    claims.subject = "user123";
    claims.issued_at = std::chrono::system_clock::now() - std::chrono::hours(2);
    claims.expires_at = std::chrono::system_clock::now() - std::chrono::hours(1);

    auto token = auth.create_token(claims);
    REQUIRE(token.has_value());

    auto validated = auth.validate(*token);
    REQUIRE_FALSE(validated.has_value());
    REQUIRE(validated.error() == jwt_error::expired_token);
}

TEST_CASE("jwt_authenticator_validate_invalid_signature", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing invalid signature detection";

    auto auth1 = jwt_authenticator::create_hs256(test_secret, test_issuer, test_audience);
    auto auth2 = jwt_authenticator::create_hs256("different-secret-key-32-bytes!!", test_issuer, test_audience);

    jwt_claims claims;
    claims.subject = "user123";
    claims.issued_at = std::chrono::system_clock::now();
    claims.expires_at = std::chrono::system_clock::now() + std::chrono::hours(1);

    auto token = auth1.create_token(claims);
    REQUIRE(token.has_value());

    auto validated = auth2.validate(*token);
    REQUIRE_FALSE(validated.has_value());
    // JWT library may return invalid_signature or invalid_token depending on error message
    REQUIRE((validated.error() == jwt_error::invalid_signature ||
             validated.error() == jwt_error::invalid_token));
}

TEST_CASE("jwt_authenticator_validate_invalid_issuer", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing invalid issuer detection";

    auto auth_create = jwt_authenticator::create_hs256(test_secret, "issuer-a", test_audience);
    auto auth_verify = jwt_authenticator::create_hs256(test_secret, "issuer-b", test_audience);

    jwt_claims claims;
    claims.subject = "user123";
    claims.issuer = "issuer-a";
    claims.issued_at = std::chrono::system_clock::now();
    claims.expires_at = std::chrono::system_clock::now() + std::chrono::hours(1);

    auto token = auth_create.create_token(claims);
    REQUIRE(token.has_value());

    auto validated = auth_verify.validate(*token);
    REQUIRE_FALSE(validated.has_value());
    // JWT library may return invalid_issuer or invalid_token depending on error message
    REQUIRE((validated.error() == jwt_error::invalid_issuer ||
             validated.error() == jwt_error::invalid_token));
}

TEST_CASE("jwt_authenticator_validate_invalid_audience", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing invalid audience detection";

    auto auth_create = jwt_authenticator::create_hs256(test_secret, test_issuer, "audience-a");
    auto auth_verify = jwt_authenticator::create_hs256(test_secret, test_issuer, "audience-b");

    jwt_claims claims;
    claims.subject = "user123";
    claims.audience = "audience-a";
    claims.issued_at = std::chrono::system_clock::now();
    claims.expires_at = std::chrono::system_clock::now() + std::chrono::hours(1);

    auto token = auth_create.create_token(claims);
    REQUIRE(token.has_value());

    auto validated = auth_verify.validate(*token);
    REQUIRE_FALSE(validated.has_value());
    REQUIRE(validated.error() == jwt_error::invalid_audience);
}

TEST_CASE("jwt_authenticator_validate_malformed_token", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing malformed token detection";

    auto auth = jwt_authenticator::create_hs256(test_secret, test_issuer, test_audience);

    auto validated = auth.validate("not.a.valid.jwt.token");
    REQUIRE_FALSE(validated.has_value());
    REQUIRE(validated.error() == jwt_error::invalid_token);
}

TEST_CASE("jwt_authenticator_validate_empty_token", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing empty token detection";

    auto auth = jwt_authenticator::create_hs256(test_secret, test_issuer, test_audience);

    auto validated = auth.validate("");
    REQUIRE_FALSE(validated.has_value());
    REQUIRE(validated.error() == jwt_error::invalid_token);
}

TEST_CASE("jwt_authenticator_unconfigured_validate_fails", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing unconfigured authenticator validation";

    auto auth = jwt_authenticator::create_hs256("", test_issuer, test_audience);

    auto validated = auth.validate("any.token.here");
    REQUIRE_FALSE(validated.has_value());
    REQUIRE(validated.error() == jwt_error::invalid_token);
}

TEST_CASE("jwt_authenticator_create_token_with_optional_issuer_from_config", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing token creation with issuer from config";

    auto auth = jwt_authenticator::create_hs256(test_secret, test_issuer, test_audience);

    jwt_claims claims;
    claims.subject = "user123";
    claims.issued_at = std::chrono::system_clock::now();
    claims.expires_at = std::chrono::system_clock::now() + std::chrono::hours(1);
    // Issuer not set in claims, should use config issuer

    auto token = auth.create_token(claims);
    REQUIRE(token.has_value());

    auto validated = auth.validate(*token);
    REQUIRE(validated.has_value());
    REQUIRE(validated->issuer == test_issuer);
}

TEST_CASE("jwt_authenticator_create_token_with_optional_audience_from_config", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing token creation with audience from config";

    auto auth = jwt_authenticator::create_hs256(test_secret, test_issuer, test_audience);

    jwt_claims claims;
    claims.subject = "user123";
    claims.issued_at = std::chrono::system_clock::now();
    claims.expires_at = std::chrono::system_clock::now() + std::chrono::hours(1);
    // Audience not set in claims, should use config audience

    auto token = auth.create_token(claims);
    REQUIRE(token.has_value());

    auto validated = auth.validate(*token);
    REQUIRE(validated.has_value());
    REQUIRE(validated->audience == test_audience);
}

TEST_CASE("jwt_error_to_string", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing jwt_error to_string";

    REQUIRE(to_string(jwt_error::invalid_token) == "Invalid token");
    REQUIRE(to_string(jwt_error::expired_token) == "Token has expired");
    REQUIRE(to_string(jwt_error::invalid_signature) == "Invalid signature");
    REQUIRE(to_string(jwt_error::missing_claims) == "Missing required claims");
    REQUIRE(to_string(jwt_error::invalid_issuer) == "Invalid issuer");
    REQUIRE(to_string(jwt_error::invalid_audience) == "Invalid audience");
}
