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
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/iam/role_write_routes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <unordered_set>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::routes::iam::permission_changes_body;
using ores::http::routes::iam::role_write_routes;
using ores::nats::service::nats_client;

namespace {

const std::string role_id = "2f1c9a40-1e7b-4c2b-9d7a-5b8e3f4a6c10";

http_request role_request(std::map<std::string, std::string> path, std::string body = {}) {
    http_request req;
    for (auto& [k, v] : path)
        req.path_params[k] = v;
    req.body = std::move(body);
    return req;
}

http_response role_call(nats_client& session,
                        http_method method,
                        const std::string& pattern,
                        const http_request& req) {
    auto router = std::make_shared<ores::http::net::router>();
    role_write_routes::register_routes(
        router, std::make_shared<ores::http::openapi::endpoint_registry>(), session);
    const auto& routes = router->routes();
    const auto found = std::ranges::find_if(
        routes, [&](const auto& r) { return r.method == method && r.pattern == pattern; });
    REQUIRE(found != routes.end());
    std::optional<http_response> answer;
    boost::asio::io_context io;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> { answer = co_await found->handler(req); },
        boost::asio::detached);
    io.run();
    return answer.value_or(http_response{});
}

}

TEST_CASE("role_write_routes_read_each_body_within_the_bff_limits", "[role_write]") {
    CHECK(role_write_routes::read_give_role(R"({"roleId":")" + role_id + R"(","reasonCode":"x"})"));
    CHECK_FALSE(role_write_routes::read_give_role(R"({"roleId":"nope","reasonCode":"x"})"));
    CHECK_FALSE(
        role_write_routes::read_give_role(R"({"roleId":")" + role_id + R"(","reasonCode":""})"));

    const auto made = role_write_routes::read_new_role(R"({"name":"  Trader  "})");
    REQUIRE(made);
    CHECK(made->name == "Trader");
    CHECK_FALSE(role_write_routes::read_new_role(R"({"name":"   "})"));
    CHECK_FALSE(role_write_routes::read_edited_role(R"({"name":"Trader"})"));
    CHECK(role_write_routes::read_edited_role(
        R"({"name":"Trader","version":3,"registrationDefault":false,"requestable":true})"));

    CHECK_FALSE(role_write_routes::read_edited_role(
        R"({"name":"Trader","version":4294967296,"registrationDefault":false,"requestable":true})"));
    CHECK(role_write_routes::read_bundle(R"({"codes":["iam::roles:read"]})"));
    CHECK_FALSE(role_write_routes::read_bundle(R"({"codes":[""]})"));
    CHECK(role_write_routes::read_changes(R"({"add":["a"],"note":"why"})"));
    CHECK_FALSE(role_write_routes::read_changes(R"({"add":["a"],"note":""})"));
}

TEST_CASE("role_write_routes_apply_changes_as_the_bff_did", "[role_write]") {
    permission_changes_body changes;
    changes.add = std::vector<std::string>{"c", "a", "d"};
    changes.remove = std::vector<std::string>{"b"};
    changes.note = "why";
    CHECK(role_write_routes::apply_changes({"a", "b", "c"}, changes) ==
          std::vector<std::string>{"a", "c", "d"});
}

TEST_CASE("role_write_routes_answer_the_catalogues_in_the_screen_shapes", "[role_write]") {
    role_write_routes::role_summary trader;
    trader.role.name = "Trader";
    trader.role.is_requestable = true;
    trader.permission_codes = {"trading::trades:read"};
    role_write_routes::role_summary service;
    service.role.name = "IamService";
    const auto roles = role_write_routes::roles_json({trader, service});
    for (const auto* key : {"\"name\":\"Trader\"",
                            "\"service\":false",
                            "\"requestable\":true",
                            "\"registrationDefault\":false",
                            "\"permissionCodes\":[\"trading::trades:read\"]",
                            "\"service\":true"})
        CHECK(roles.find(key) != std::string::npos);

    ores::iam::domain::permission b, a;
    b.code = "b";
    a.code = "a";
    a.description = "first";
    CHECK(role_write_routes::permissions_json({b, a}) ==
          R"({"permissions":[{"code":"a","description":"first"},{"code":"b","description":""}]})");
}

TEST_CASE("role_write_routes_refuse_before_forwarding", "[role_write]") {
    nats_client session;
    const auto give = role_call(session,
                                http_method::post,
                                "/api/accounts/{accountId}/roles",
                                role_request({{"accountId", role_id}}, "{}"));
    CHECK(give.status == http_status::bad_request);
    CHECK(give.body.find("Choose a role and a reason.") != std::string::npos);
    CHECK(role_call(session,
                    http_method::delete_,
                    "/api/accounts/{accountId}/roles/{roleId}",
                    role_request({{"accountId", "x"}, {"roleId", role_id}}))
              .status == http_status::not_found);
    CHECK(role_call(session, http_method::post, "/api/roles", role_request({}, R"({"name":""})"))
              .status == http_status::bad_request);
    CHECK(role_call(session,
                    http_method::put,
                    "/api/roles/{roleId}",
                    role_request({{"roleId", role_id}}, R"({"name":"x"})"))
              .status == http_status::bad_request);
    CHECK(role_call(session,
                    http_method::post,
                    "/api/roles/{roleId}/permissions/changes",
                    role_request({{"roleId", role_id}}, R"({"note":""})"))
              .status == http_status::bad_request);
}

TEST_CASE("role_write_routes_forward_an_accepted_request", "[role_write]") {
    nats_client session;
    CHECK(role_call(session, http_method::get, "/api/roles", role_request({})).status ==
          http_status::bad_gateway);
    CHECK(role_call(session,
                    http_method::post,
                    "/api/accounts/{accountId}/lock",
                    role_request({{"accountId", role_id}}))
              .status == http_status::bad_gateway);
    CHECK(
        role_call(
            session, http_method::delete_, "/api/roles/{name}", role_request({{"name", "Trader"}}))
            .status == http_status::bad_gateway);
}

TEST_CASE("role_write_routes_offer_the_roles_that_would_give_a_permission", "[role_write]") {
    const auto role = [](std::string name, bool requestable, std::vector<std::string> codes) {
        role_write_routes::role_summary r;
        r.role.name = std::move(name);
        r.role.is_requestable = requestable;
        r.role.id = boost::uuids::random_generator()();
        r.permission_codes = std::move(codes);
        return r;
    };
    const auto exact = role("Trader", true, {"trading::trades:read"});
    const auto area = role("Desk", true, {"trading::*"});
    const auto everything = role("Admin", true, {"*"});
    const auto closed = role("Closed", false, {"trading::trades:read"});
    const auto service = role("TradingService", true, {"*"});
    const auto other = role("Ops", true, {"iam::roles:read"});
    const auto held = role("Held", true, {"trading::trades:read"});
    const std::unordered_set<std::string> mine{boost::uuids::to_string(held.role.id)};
    const auto ways = role_write_routes::ways_to_hold(
        "trading::trades:read", {exact, area, everything, closed, service, other, held}, mine);
    std::vector<std::string> names;
    for (const auto& w : ways)
        names.push_back(w.role.name);
    CHECK(names == std::vector<std::string>{"Trader", "Desk", "Admin"});
    CHECK(role_write_routes::ways_json({exact}).find("\"name\":\"Trader\"") != std::string::npos);
}

TEST_CASE("role_write_routes_refuse_a_permission_that_is_not_one", "[role_write]") {
    nats_client session;
    http_request req;
    req.query_params["permission"] = "not a code";
    const auto refused = role_call(session, http_method::get, "/api/me/ways-to-hold", req);
    CHECK(refused.status == http_status::bad_request);
    req.query_params["permission"] = "trading::trades:read";
    const auto none = role_call(session, http_method::get, "/api/me/ways-to-hold", req);
    CHECK(none.status == http_status::ok);
    CHECK(none.body == R"({"roles":[]})");
}
