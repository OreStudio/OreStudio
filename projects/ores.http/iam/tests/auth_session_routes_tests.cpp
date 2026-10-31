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
#include "ores.http/routes/iam/auth_session_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>

using namespace ores::http;
using namespace ores::http::routes::iam;
using namespace ores::logging;
namespace asio = boost::asio;

namespace {

domain::cookie_settings cookie_named(std::string name) {
    domain::cookie_settings settings;
    settings.name = std::move(name);
    return settings;
}

domain::http_request a_request_with(const std::string& cookie) {
    domain::http_request req;
    req.method = domain::http_method::delete_;
    if (!cookie.empty())
        req.headers["cookie"] = cookie;
    return req;
}

/// Runs an awaitable to completion without any I/O.
domain::http_response run(asio::awaitable<domain::http_response> work) {
    asio::io_context io;
    std::optional<domain::http_response> answer;
    asio::co_spawn(
        io,
        [&answer, work = std::move(work)]() mutable -> asio::awaitable<void> {
            answer = co_await std::move(work);
        },
        asio::detached);
    io.run();
    return answer.value_or(domain::http_response{});
}

}

TEST_CASE("the_session_door_registers_its_three_routes", "[auth_session]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the resources a browser signs in through";

    ores::nats::service::nats_client session;
    auto router = std::make_shared<net::router>();
    auto registry = std::make_shared<openapi::endpoint_registry>();
    auto store = std::make_shared<domain::session_store>(std::chrono::hours(1));
    auth_session_routes routes(session, store, nullptr, cookie_named("ores_web_session_test"));

    routes.register_routes(router, registry);

    const auto routes_registered = router->routes();
    REQUIRE(routes_registered.size() == 3);

    const auto find = [&](std::string_view pattern) {
        for (const auto& route : routes_registered)
            if (route.pattern == pattern)
                return route;
        return domain::route{};
    };

    const auto sign_in = find("/api/v1/auth/sessions");
    REQUIRE(sign_in.method == domain::http_method::post);
    // Signing in cannot require the thing it gives out.
    REQUIRE(!sign_in.requires_auth);

    const auto sign_out = find("/api/v1/auth/sessions");
    REQUIRE(sign_out.method == domain::http_method::post);

    const auto me = find("/api/v1/auth/sessions/me");
    REQUIRE(me.method == domain::http_method::get);
    REQUIRE(me.requires_auth);
}

TEST_CASE("me_answers_the_principal_and_never_the_token", "[auth_session]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing what the browser is told about itself";

    ores::nats::service::nats_client session;
    auto store = std::make_shared<domain::session_store>(std::chrono::hours(1));
    auth_session_routes routes(session, store, nullptr, cookie_named("ores_web_session_test"));

    domain::http_request req;
    domain::http_response unauthenticated = routes.me(req);
    REQUIRE(unauthenticated.status == domain::http_status::unauthorized);

    ores::security::jwt::jwt_claims claims;
    claims.subject = "the-account";
    claims.tenant_id = "the-tenant";
    claims.party_id = "the-party";
    claims.roles = {"reader", "writer"};
    req.authenticated_user = claims;

    const auto answered = routes.me(req);
    REQUIRE(answered.status == domain::http_status::ok);
    REQUIRE(answered.body.find(R"("subject":"the-account")") != std::string::npos);
    REQUIRE(answered.body.find(R"("tenant":"the-tenant")") != std::string::npos);
    REQUIRE(answered.body.find("reader") != std::string::npos);
    // The token is the server's, and does not travel.
    REQUIRE(answered.body.find("token") == std::string::npos);
}

TEST_CASE("signing_out_closes_the_session_and_clears_the_cookie", "[auth_session]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that signing out ends the session it names";

    ores::nats::service::nats_client session;
    auto store = std::make_shared<domain::session_store>(std::chrono::hours(1));
    const auto settings = cookie_named("ores_web_session_test");
    auth_session_routes routes(session, store, nullptr, settings);

    domain::session_record record;
    record.token = "the-token";
    record.claims.subject = "the-account";
    const auto identifier = store->open(record);
    REQUIRE(store->size() == 1);

    const auto answered = run(routes.sign_out(a_request_with(settings.name + "=" + identifier)));

    REQUIRE(answered.status == domain::http_status::ok);
    REQUIRE(store->size() == 0);
    REQUIRE(!store->touch(identifier).has_value());

    // The browser is told to drop the cookie even over a session it did not know.
    const auto clearing = answered.headers.find("Set-Cookie");
    REQUIRE(clearing != answered.headers.end());
    REQUIRE(clearing->second.find("Max-Age=0") != std::string::npos);
}
