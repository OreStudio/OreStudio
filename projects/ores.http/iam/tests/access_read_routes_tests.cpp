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
#include "ores.http/routes/iam/access_read_routes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <memory>
#include <optional>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::routes::iam::access_read_routes;
using ores::nats::service::nats_client;

namespace {

http_request access_request(std::map<std::string, std::string> query = {},
                            std::map<std::string, std::string> path = {}) {
    http_request req;
    for (auto& [k, v] : query)
        req.query_params[k] = v;
    for (auto& [k, v] : path)
        req.path_params[k] = v;
    return req;
}

http_response
access_call(nats_client& session, const std::string& pattern, const http_request& req) {
    auto router = std::make_shared<ores::http::net::router>();
    access_read_routes::register_routes(
        router, std::make_shared<ores::http::openapi::endpoint_registry>(), session);
    const auto& routes = router->routes();
    const auto found =
        std::ranges::find_if(routes, [&](const auto& r) { return r.pattern == pattern; });
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

const std::string some_id = "2f1c9a40-1e7b-4c2b-9d7a-5b8e3f4a6c10";

}

TEST_CASE("access_read_routes_register_the_screen_addresses", "[access]") {
    nats_client session;
    auto router = std::make_shared<ores::http::net::router>();
    access_read_routes::register_routes(
        router, std::make_shared<ores::http::openapi::endpoint_registry>(), session);
    const auto has = [&](const std::string& pattern) {
        return std::ranges::any_of(router->routes(), [&](const auto& r) {
            return r.method == http_method::get && r.pattern == pattern && r.requires_auth;
        });
    };
    CHECK(has("/api/me/access"));
    CHECK(has("/api/accounts/{accountId}/access"));
    CHECK(has("/api/me/permissions"));
    CHECK(has("/api/accounts/{accountId}/permissions"));
    CHECK(has("/api/roles/page"));
    CHECK(has("/api/roles/{roleId}/permissions"));
    CHECK(has("/api/roles/{roleId}/holders"));
}

TEST_CASE("access_read_routes_read_a_permission_page_within_the_bff_limits", "[access]") {
    const auto defaults = access_read_routes::read_permission_query(access_request());
    REQUIRE(defaults);
    CHECK(defaults->offset == 0);
    CHECK(defaults->limit == 15);
    CHECK(access_read_routes::read_permission_query(access_request({{"limit", "500"}})));
    CHECK_FALSE(access_read_routes::read_permission_query(access_request({{"limit", "501"}})));
    CHECK_FALSE(access_read_routes::read_permission_query(access_request({{"limit", "0"}})));
    CHECK_FALSE(access_read_routes::read_permission_query(access_request({{"offset", "-1"}})));
    CHECK_FALSE(access_read_routes::read_permission_query(access_request({{"offset", "2x"}})));
    CHECK_FALSE(access_read_routes::read_permission_query(
        access_request({{"area", std::string(101, 'a')}})));
}

TEST_CASE("access_read_routes_read_a_role_page_within_the_bff_limits", "[access]") {
    const auto page = access_read_routes::read_roles_query(
        access_request({{"roleId", some_id}, {"includeService", "true"}}));
    REQUIRE(page);
    CHECK(page->include_service);
    CHECK_FALSE(access_read_routes::read_roles_query(access_request({{"roleId", "nope"}})));
    CHECK_FALSE(access_read_routes::read_roles_query(access_request({{"includeService", "yes"}})));
}

TEST_CASE("access_read_routes_refuse_a_page_that_is_not_one_before_forwarding", "[access]") {
    nats_client session;
    CHECK(access_call(session,
                      "/api/accounts/{accountId}/access",
                      access_request({}, {{"accountId", "nope"}}))
              .status == http_status::not_found);
    for (const auto* pattern : {"/api/accounts/{accountId}/permissions"}) {
        const auto refused =
            access_call(session, pattern, access_request({}, {{"accountId", "nope"}}));
        CHECK(refused.status == http_status::bad_request);
        CHECK(refused.body.find("The page asked for is not one.") != std::string::npos);
    }
    CHECK(access_call(session,
                      "/api/roles/{roleId}/permissions",
                      access_request({{"includeUnheld", "maybe"}}, {{"roleId", some_id}}))
              .status == http_status::bad_request);
    CHECK(access_call(session,
                      "/api/roles/{roleId}/holders",
                      access_request({{"limit", "0"}}, {{"roleId", some_id}}))
              .status == http_status::bad_request);
    CHECK(access_call(session, "/api/roles/page", access_request({{"limit", "x"}})).status ==
          http_status::bad_request);
}

TEST_CASE("access_read_routes_forward_an_accepted_read", "[access]") {
    nats_client session;
    CHECK(access_call(session, "/api/me/access", access_request()).status ==
          http_status::bad_gateway);
    CHECK(access_call(
              session, "/api/roles/{roleId}/holders", access_request({}, {{"roleId", some_id}}))
              .status == http_status::bad_gateway);
}

TEST_CASE("access_read_routes_answer_in_the_screen_shapes", "[access]") {
    namespace m = ores::iam::messaging;

    m::get_account_roles_response roles;
    m::account_role_access held;
    held.role.name = "Trader";
    held.permission_codes = {"trading::trades:read"};
    held.assigned_by = "admin";
    held.change_reason_code = "system.new_record";
    roles.roles.push_back(held);
    const auto access = access_read_routes::access_json(roles);
    for (const auto* key : {"\"roles\":[",
                            "\"roleId\":",
                            "\"name\":\"Trader\"",
                            "\"permissionCodes\":[\"trading::trades:read\"]",
                            "\"givenBy\":\"admin\"",
                            "\"givenAt\":",
                            "\"reasonCode\":\"system.new_record\"",
                            "\"commentary\":"})
        CHECK(access.find(key) != std::string::npos);

    m::permission_page_response page;
    page.area = "iam";
    page.total_count = 3;
    page.area_whole = true;
    const auto permissions = access_read_routes::page_json(page);
    for (const auto* key : {"\"area\":\"iam\"",
                            "\"rows\":[]",
                            "\"totalCount\":3",
                            "\"areas\":[]",
                            "\"areaWhole\":true",
                            "\"everything\":false"})
        CHECK(permissions.find(key) != std::string::npos);

    m::role_page_response role_page;
    m::role_page_row row;
    row.id = some_id;
    row.registration_default = true;
    row.permission_count = 4;
    role_page.roles.push_back(row);
    role_page.service_hidden = 2;
    const auto page_of_roles = access_read_routes::roles_json(role_page);
    for (const auto* key : {"\"registrationDefault\":true",
                            "\"permissionCount\":4",
                            "\"serviceHidden\":2",
                            "\"totalCount\":0"})
        CHECK(page_of_roles.find(key) != std::string::npos);

    m::role_holders_response holders;
    holders.holders.push_back({some_id, "alice", "Alice", "", "admin"});
    holders.holders.push_back({some_id, "bob", "Bob", "img-1", "admin"});
    holders.total_count = 2;
    const auto who = access_read_routes::holders_json(holders);
    CHECK(who.find("\"imageId\":null") != std::string::npos);
    CHECK(who.find("\"imageId\":\"img-1\"") != std::string::npos);
    CHECK(who.find("\"fullName\":\"Alice\"") != std::string::npos);
    CHECK(who.find("\"totalCount\":2") != std::string::npos);
}

TEST_CASE("access_read_routes_escape_what_a_holder_states", "[access]") {
    ores::iam::messaging::role_holders_response holders;
    holders.holders.push_back({some_id, "o\"brien", "Zoë \\ Ångström", "", "admin"});
    const auto who = access_read_routes::holders_json(holders);
    CHECK(who.find(R"("username":"o\"brien")") != std::string::npos);
    CHECK(who.find("Zoë") != std::string::npos);
    CHECK(who.find(R"(\\)") != std::string::npos);
}

TEST_CASE("access_read_routes_count_characters_not_bytes", "[access]") {
    std::string accented;
    for (int i = 0; i < 100; ++i)
        accented += "é";
    CHECK(access_read_routes::read_permission_query(access_request({{"area", accented}})));
    CHECK_FALSE(
        access_read_routes::read_permission_query(access_request({{"area", accented + "é"}})));
}
