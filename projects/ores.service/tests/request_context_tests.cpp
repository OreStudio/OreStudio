/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.service/service/request_context.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.service/error_code.hpp"
#include "ores.testing/database_helper.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace {

const std::string tags("[request_context]");
const std::string secret("ores-service-request-context-test-secret-32b");
const std::string issuer("ores.service.tests");
const std::string audience("ores.service.tests");
const std::string tenant("11111111-1111-1111-1111-111111111111");
const std::string party("22222222-2222-2222-2222-222222222222");
const std::string workspace("33333333-3333-3333-3333-333333333333");
const std::string ancestor("44444444-4444-4444-4444-444444444444");

ores::security::jwt::jwt_authenticator make_authenticator(const std::string& key) {
    return ores::security::jwt::jwt_authenticator::create_hs256(key, issuer, audience);
}

std::string make_token(std::chrono::seconds ttl, const std::string& key = secret) {
    auto claims = ores::security::jwt::jwt_claims::with_ttl(ttl);
    claims.subject = "account-1";
    claims.issuer = issuer;
    claims.audience = audience;
    claims.roles = {"iam::accounts:read", "iam::*"};
    claims.username = "alice";
    claims.tenant_id = tenant;
    return make_authenticator(key).create_token(claims).value();
}

std::string bearer(std::string token) {
    return std::string(ores::nats::headers::bearer_prefix) + token;
}

}

using ores::service::error_code;
using ores::service::service::make_request_context;

TEST_CASE("a request without a verifier keeps the base context", tags) {
    ores::testing::database_helper h;
    ores::nats::message msg;

    const auto result = make_request_context(h.context(), msg, std::nullopt);

    REQUIRE(result.has_value());
    REQUIRE(result->tenant_id().to_string() == h.context().tenant_id().to_string());
}

TEST_CASE("a request without an authorization header is unauthorized", tags) {
    ores::testing::database_helper h;
    ores::nats::message msg;

    const auto result = make_request_context(h.context(), msg, make_authenticator(secret));

    REQUIRE_FALSE(result.has_value());
    REQUIRE(result.error() == error_code::unauthorized);
}

TEST_CASE("an authorization header that is not a bearer token is unauthorized", tags) {
    ores::testing::database_helper h;
    ores::nats::message msg;
    msg.headers[std::string(ores::nats::headers::authorization)] = make_token(std::chrono::minutes(5));

    const auto result = make_request_context(h.context(), msg, make_authenticator(secret));

    REQUIRE_FALSE(result.has_value());
    REQUIRE(result.error() == error_code::unauthorized);
}

TEST_CASE("an expired bearer token reports the token as expired", tags) {
    ores::testing::database_helper h;
    ores::nats::message msg;
    msg.headers[std::string(ores::nats::headers::authorization)] =
        bearer(make_token(std::chrono::seconds(-60)));

    const auto result = make_request_context(h.context(), msg, make_authenticator(secret));

    REQUIRE_FALSE(result.has_value());
    REQUIRE(result.error() == error_code::token_expired);
}

TEST_CASE("a token signed with another key is unauthorized", tags) {
    ores::testing::database_helper h;
    ores::nats::message msg;
    msg.headers[std::string(ores::nats::headers::authorization)] =
        bearer(make_token(std::chrono::minutes(5), "another-secret-that-is-long-enough-32b"));

    const auto result = make_request_context(h.context(), msg, make_authenticator(secret));

    REQUIRE_FALSE(result.has_value());
    REQUIRE(result.error() == error_code::unauthorized);
}

TEST_CASE("a valid bearer token scopes the context to its tenant, actor and permissions", tags) {
    ores::testing::database_helper h;
    ores::nats::message msg;
    msg.headers[std::string(ores::nats::headers::authorization)] = bearer(make_token(std::chrono::minutes(5)));

    const auto result = make_request_context(h.context(), msg, make_authenticator(secret));

    REQUIRE(result.has_value());
    REQUIRE(result->tenant_id().to_string() == tenant);
    REQUIRE(result->actor() == "alice");
    REQUIRE(result->roles() == std::vector<std::string>{"iam::accounts:read", "iam::*"});
    REQUIRE_FALSE(result->party_id().has_value());
}

TEST_CASE("a token naming a party scopes the context to that party", tags) {
    ores::testing::database_helper h;
    auto claims = ores::security::jwt::jwt_claims::with_ttl(std::chrono::minutes(5));
    claims.issuer = issuer;
    claims.audience = audience;
    claims.roles = {"iam::*"};
    claims.username = "alice";
    claims.tenant_id = tenant;
    claims.party_id = party;
    claims.visible_party_ids = {party};

    ores::nats::message msg;
    msg.headers[std::string(ores::nats::headers::authorization)] =
        bearer(make_authenticator(secret).create_token(claims).value());

    const auto result = make_request_context(h.context(), msg, make_authenticator(secret));

    REQUIRE(result.has_value());
    REQUIRE(result->tenant_id().to_string() == tenant);
    REQUIRE(result->party_id().has_value());
    REQUIRE(*result->party_id() == boost::uuids::string_generator()(party));
    REQUIRE(result->actor() == "alice");
}

TEST_CASE("a token with an unparseable party falls back to tenant scope", tags) {
    ores::testing::database_helper h;
    auto claims = ores::security::jwt::jwt_claims::with_ttl(std::chrono::minutes(5));
    claims.issuer = issuer;
    claims.audience = audience;
    claims.roles = {"iam::*"};
    claims.username = "alice";
    claims.tenant_id = tenant;
    claims.party_id = "not-a-uuid";

    ores::nats::message msg;
    msg.headers[std::string(ores::nats::headers::authorization)] =
        bearer(make_authenticator(secret).create_token(claims).value());

    const auto result = make_request_context(h.context(), msg, make_authenticator(secret));

    REQUIRE(result.has_value());
    REQUIRE(result->tenant_id().to_string() == tenant);
    REQUIRE_FALSE(result->party_id().has_value());
    REQUIRE(result->actor() == "alice");
}

TEST_CASE("a token with no tenant is unauthorized", tags) {
    ores::testing::database_helper h;
    auto claims = ores::security::jwt::jwt_claims::with_ttl(std::chrono::minutes(5));
    claims.issuer = issuer;
    claims.audience = audience;
    claims.username = "alice";

    ores::nats::message msg;
    msg.headers[std::string(ores::nats::headers::authorization)] =
        bearer(make_authenticator(secret).create_token(claims).value());

    const auto result = make_request_context(h.context(), msg, make_authenticator(secret));

    REQUIRE_FALSE(result.has_value());
    REQUIRE(result.error() == error_code::unauthorized);
}

TEST_CASE("the workspace headers scope the context to the requested workspace and chain", tags) {
    ores::testing::database_helper h;
    ores::nats::message msg;
    msg.headers[std::string(ores::nats::headers::authorization)] = bearer(make_token(std::chrono::minutes(5)));
    msg.headers[std::string(ores::nats::headers::x_workspace_id)] = workspace;
    msg.headers[std::string(ores::nats::headers::x_workspace_resolution)] =
        workspace + "," + ancestor;

    const auto result = make_request_context(h.context(), msg, make_authenticator(secret));

    REQUIRE(result.has_value());
    REQUIRE(result->workspace_id() == workspace);
    REQUIRE(result->workspace_resolution() == std::vector<std::string>{workspace, ancestor});
}

TEST_CASE("a delegated bearer token builds the original user's context", tags) {
    ores::testing::database_helper h;
    ores::nats::message msg;
    msg.headers[std::string(ores::nats::headers::delegated_authorization)] =
        bearer(make_token(std::chrono::minutes(5)));

    const auto result = make_request_context(h.context(), msg, make_authenticator(secret));

    REQUIRE(result.has_value());
    REQUIRE(result->tenant_id().to_string() == tenant);
    REQUIRE(result->actor() == "alice");
}

TEST_CASE("a delegated header that is not a bearer token is unauthorized", tags) {
    ores::testing::database_helper h;
    ores::nats::message msg;
    msg.headers[std::string(ores::nats::headers::delegated_authorization)] =
        make_token(std::chrono::minutes(5));

    const auto result = make_request_context(h.context(), msg, make_authenticator(secret));

    REQUIRE_FALSE(result.has_value());
    REQUIRE(result.error() == error_code::unauthorized);
}
