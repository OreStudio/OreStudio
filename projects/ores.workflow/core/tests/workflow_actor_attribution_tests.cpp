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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.workflow.core/service/workflow_actor.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

// The rule the engine attributes a workflow run by: the username from the
// caller's own token when there is one, and the service account when there is
// not. It is pinned here rather than through the engine because the engine
// needs a database and a NATS connection to exist at all, and the rule is
// decided before either is touched.
namespace {

using ores::security::jwt::jwt_authenticator;
using ores::security::jwt::jwt_claims;

const std::string actor_secret = "workflow-actor-attribution-test-secret";
const std::string other_secret = "workflow-actor-attribution-another-secret";
const std::string fallback = "ores_clever_dijkstra_workflow_service";

std::string token_for(const std::optional<std::string>& username,
                      const std::string& secret = actor_secret) {
    jwt_claims claims;
    claims.subject = "0e1f4d2c-1111-2222-3333-444455556666";
    claims.username = username;
    claims.issued_at = std::chrono::system_clock::now();
    claims.expires_at = claims.issued_at + std::chrono::hours(1);
    const auto token = jwt_authenticator::create_hs256(secret).create_token(claims);
    REQUIRE(token.has_value());
    return *token;
}

ores::nats::message message_with(std::string_view header, const std::string& token) {
    ores::nats::message msg;
    msg.headers[std::string(header)] = std::string(ores::nats::headers::bearer_prefix) + token;
    return msg;
}

std::string actor_for(const ores::nats::message& msg,
                      const std::optional<jwt_authenticator>& verifier = std::nullopt) {
    return ores::workflow::service::actor_from_message(msg, verifier, fallback);
}

}

TEST_CASE("a_forwarded_caller_token_names_the_actor", "[workflow][actor]") {
    const auto verifier = jwt_authenticator::create_hs256(actor_secret);
    const auto msg = message_with(ores::nats::headers::delegated_authorization,
                                  token_for("alice"));
    CHECK(actor_for(msg, verifier) == "alice");
}

TEST_CASE("the_delegated_token_wins_over_the_last_hop", "[workflow][actor]") {
    // An impersonated call carries the impersonation token in Authorization
    // and the original caller in the delegated header; the actor is the
    // caller, not the identity the service is impersonating.
    auto msg = message_with(ores::nats::headers::authorization, token_for("service_account"));
    msg.headers[std::string(ores::nats::headers::delegated_authorization)] =
        std::string(ores::nats::headers::bearer_prefix) + token_for("alice");
    CHECK(actor_for(msg, jwt_authenticator::create_hs256(actor_secret)) == "alice");
}

TEST_CASE("the_authorization_header_is_used_when_nothing_is_forwarded",
          "[workflow][actor]") {
    const auto msg = message_with(ores::nats::headers::authorization, token_for("bob"));
    CHECK(actor_for(msg, jwt_authenticator::create_hs256(actor_secret)) == "bob");
}

TEST_CASE("a_message_with_no_token_falls_back_to_the_service_account",
          "[workflow][actor]") {
    CHECK(actor_for(ores::nats::message{},
                    jwt_authenticator::create_hs256(actor_secret)) == fallback);
}

TEST_CASE("a_token_that_names_nobody_falls_back_to_the_service_account",
          "[workflow][actor]") {
    // The store refuses an empty actor, so this must not reach it.
    const auto msg = message_with(ores::nats::headers::delegated_authorization,
                                  token_for(std::nullopt));
    CHECK(actor_for(msg, jwt_authenticator::create_hs256(actor_secret)) == fallback);
}

TEST_CASE("a_token_this_service_cannot_verify_falls_back_to_the_service_account",
          "[workflow][actor]") {
    const auto msg = message_with(ores::nats::headers::delegated_authorization,
                                  token_for("alice", other_secret));
    CHECK(actor_for(msg, jwt_authenticator::create_hs256(actor_secret)) == fallback);
}

TEST_CASE("a_service_with_no_verifier_attributes_everything_to_itself",
          "[workflow][actor]") {
    const auto msg = message_with(ores::nats::headers::delegated_authorization,
                                  token_for("alice"));
    CHECK(actor_for(msg, std::nullopt) == fallback);
}
