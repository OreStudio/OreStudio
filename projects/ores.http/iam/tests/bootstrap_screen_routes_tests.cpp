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
#include "ores.http/routes/iam/bootstrap_screen_routes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <optional>
#include <string>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::net::router;
using ores::http::openapi::endpoint_registry;
using ores::http::routes::iam::bootstrap_screen_routes;
using ores::nats::service::nats_client;

namespace {

struct setup_route final {
    http_method method;
    std::string pattern;
    bool requires_auth;
};

http_response setup_answer(http_method method, const std::string& pattern, std::string body = {}) {
    nats_client session;
    auto table = std::make_shared<router>();
    bootstrap_screen_routes(session, {})
        .register_routes(table, std::make_shared<endpoint_registry>());
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

TEST_CASE("bootstrap_screen_routes_read_the_host_as_the_bff_did", "[bootstrap_screen]") {
    CHECK(bootstrap_screen_routes::hostname_of("acme.io:8443") == "acme.io");
    CHECK(bootstrap_screen_routes::hostname_of("acme.io") == "acme.io");
    CHECK(bootstrap_screen_routes::hostname_of("[::1]:443") == "[::1]");
    CHECK(bootstrap_screen_routes::hostname_of("").empty());
    CHECK(bootstrap_screen_routes::hostname_of("[::1").empty());
}

TEST_CASE("bootstrap_screen_routes_answer_in_the_screens_shapes", "[bootstrap_screen]") {
    ores::iam::messaging::bootstrap_status_response status;
    status.is_in_bootstrap_mode = true;
    status.message = "No \"admin\" yet";
    status.version = "0.0.30";
    CHECK(bootstrap_screen_routes::status_json(status, false, true, "acc-1", true) ==
          R"({"isInBootstrapMode":true,"hasTenant":false,"onboardingComplete":false,)"
          R"("onboardingTenantComplete":true,"accountId":"acc-1","sessionPresent":true,)"
          R"("message":"No \"admin\" yet","version":"0.0.30"})");

    ores::iam::messaging::registration_policy_response policy;
    policy.success = true;
    policy.signups_enabled = true;
    policy.tenant_id = "t-1";
    policy.tenant_name = "Acme";
    policy.usable_now = true;
    CHECK(bootstrap_screen_routes::policy_json(policy) ==
          R"({"success":true,"message":"","errorCode":"","signupsEnabled":true,)"
          R"("authorizationRequired":false,"tenantId":"t-1","tenantName":"Acme",)"
          R"("partyId":"","partyName":"","roleId":"","roleName":"","usableNow":true})");

    CHECK(bootstrap_screen_routes::setup_run_json(std::nullopt) ==
          R"({"instanceId":"","status":"","error":""})");
    ores::workflow::messaging::workflow_instance_summary run;
    run.id = "run-1";
    run.status = "failed";
    run.error = "step 2";
    CHECK(bootstrap_screen_routes::setup_run_json(run) ==
          R"({"instanceId":"run-1","status":"failed","error":"step 2"})");
}

TEST_CASE("bootstrap_screen_routes_open_only_what_needs_no_session", "[bootstrap_screen]") {
    nats_client session;
    auto table = std::make_shared<router>();
    bootstrap_screen_routes(session, {})
        .register_routes(table, std::make_shared<endpoint_registry>());
    const std::vector<setup_route> expected{
        {http_method::get, "/api/bootstrap", false},
        {http_method::post, "/api/bootstrap/administrator", false},
        {http_method::post, "/api/bootstrap/complete", true},
        {http_method::get, "/api/tenant-setup", true},
        {http_method::get, "/api/registration-policy", false}};
    for (const auto& e : expected) {
        INFO(e.pattern);
        const auto route = std::ranges::find_if(table->routes(), [&](const auto& r) {
            return r.method == e.method && r.pattern == e.pattern;
        });
        REQUIRE(route != table->routes().end());
        CHECK(route->requires_auth == e.requires_auth);
    }
}

TEST_CASE("bootstrap_screen_routes_refuse_an_incomplete_administrator_before_forwarding",
          "[bootstrap_screen]") {
    for (const auto* body : {"{}", R"({"principal":"a","password":"","email":"e"})", "not json"}) {
        INFO(body);
        const auto answer = setup_answer(http_method::post, "/api/bootstrap/administrator", body);
        CHECK(answer.status == http_status::bad_request);
        CHECK(answer.body.find("are required") != std::string::npos);
    }
    // The client is not connected, so an accepted request fails at the forward step.
    CHECK(setup_answer(http_method::post,
                       "/api/bootstrap/administrator",
                       R"({"principal":"a","password":"p","email":"e"})")
              .status == http_status::bad_gateway);
    for (const auto& [method, pattern] : std::vector<std::pair<http_method, std::string>>{
             {http_method::get, "/api/bootstrap"},
             {http_method::post, "/api/bootstrap/complete"},
             {http_method::get, "/api/tenant-setup"},
             {http_method::get, "/api/registration-policy"}}) {
        INFO(pattern);
        CHECK(setup_answer(method, pattern).status == http_status::bad_gateway);
    }
}
