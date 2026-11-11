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
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http/routes/iam/account_door_routes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <optional>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::net::router;
using ores::http::openapi::endpoint_registry;
using ores::http::routes::iam::account_door_routes;
using ores::nats::service::nats_client;

namespace {

/// The answer an open route of the unit gives, on a client that is not connected.
http_response door_answer(http_method method, const std::string& pattern, std::string body = {}) {
    nats_client session;
    auto table = std::make_shared<router>();
    account_door_routes::register_routes(table, std::make_shared<endpoint_registry>(), session);
    const auto route = std::ranges::find_if(
        table->routes(), [&](const auto& r) { return r.method == method && r.pattern == pattern; });
    REQUIRE(route != table->routes().end());
    http_request req;
    req.body = std::move(body);
    boost::asio::io_context io;
    std::optional<http_response> answer;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> { answer = co_await route->handler(req); },
        boost::asio::detached);
    io.run();
    REQUIRE(answer.has_value());
    return *answer;
}

}

TEST_CASE("account_door_routes_refuse_a_registration_as_the_bff_did", "[account_door]") {
    const auto check = [](const char* code, http_status status, const char* api_code) {
        INFO(code);
        const auto r = account_door_routes::signup_refusal(code, "words");
        CHECK(r.status == status);
        CHECK(r.code == api_code);
        CHECK(r.message == "words");
    };
    check("username_taken", http_status::conflict, "username-taken");
    check("email_taken", http_status::conflict, "email-taken");
    check("weak_password", http_status::bad_request, "weak-password");
    check("invalid_request", http_status::bad_request, "invalid-request");
    check("signup_disabled", http_status::forbidden, "signups-disabled");
    check("signup_requires_authorization", http_status::forbidden, "signup-requires-authorization");
    check("no_registration_destination", http_status::forbidden, "no-registration-destination");
    check("no_default_role", http_status::forbidden, "no-default-role");
    check("a_new_code", http_status::forbidden, "signup-refused");
    CHECK(account_door_routes::signup_refusal("weak_password", "").message ==
          "The registration was refused.");
}

TEST_CASE("account_door_routes_answer_in_the_door_s_shapes", "[account_door]") {
    ores::iam::messaging::signup_response signup;
    signup.success = true;
    signup.account_id = "acc-1";
    signup.account_status = "pending";
    signup.party_id = "p-1";
    CHECK(account_door_routes::signup_json(signup) ==
          R"({"success":true,"message":"","errorCode":"","accountId":"acc-1",)"
          R"("accountStatus":"pending","partyId":"p-1","roleId":""})");
    ores::iam::messaging::get_password_policy_response policy;
    policy.success = true;
    policy.min_length = 12;
    policy.require_digit = true;
    policy.special_chars = "!\"#";
    CHECK(account_door_routes::policy_json(policy) ==
          R"({"success":true,"message":"","minLength":12,"requireUppercase":false,)"
          R"("requireLowercase":false,"requireDigit":true,"requireSpecial":false,)"
          R"("specialChars":"!\"#"})");
}

TEST_CASE("account_door_routes_open_the_door_and_share_the_sign_in_budget", "[account_door]") {
    nats_client session;
    auto table = std::make_shared<router>();
    account_door_routes::register_routes(table, std::make_shared<endpoint_registry>(), session);
    const auto find = [&](http_method m, const std::string& p) {
        return *std::ranges::find_if(
            table->routes(), [&](const auto& r) { return r.method == m && r.pattern == p; });
    };
    const auto signup = find(http_method::post, "/api/signup");
    CHECK_FALSE(signup.requires_auth);
    CHECK(signup.rate_policy == "credential");
    CHECK_FALSE(find(http_method::get, "/api/password-policy").requires_auth);
    CHECK(find(http_method::post, "/api/account/password").requires_auth);
}

TEST_CASE("account_door_routes_refuse_an_incomplete_body_before_forwarding", "[account_door]") {
    for (const auto* body : {"{}", R"({"principal":"a","password":"","email":"e"})", "nope"}) {
        INFO(body);
        const auto answer = door_answer(http_method::post, "/api/signup", body);
        CHECK(answer.status == http_status::bad_request);
        CHECK(answer.body.find("are required") != std::string::npos);
    }
    http_request bad;
    bad.body = R"({"currentPassword":"a"})";
    const auto password = ores::http::iam::tests::route_answer<account_door_routes>(
        http_method::post, "/api/account/password", bad);
    CHECK(password.status == http_status::bad_request);
    CHECK(password.body.find("new password are required") != std::string::npos);
    // The client is not connected, so an accepted request fails at the forward step.
    CHECK(door_answer(
              http_method::post, "/api/signup", R"({"principal":"a","password":"p","email":"e"})")
              .status == http_status::bad_gateway);
    CHECK(door_answer(http_method::get, "/api/password-policy").status == http_status::bad_gateway);
}
