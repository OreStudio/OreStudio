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
#include "ores.http/routes/iam/account_view.hpp"
#include "ores.http/routes/iam/sign_in_routes.hpp"
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
#include <vector>

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::routes::iam::sign_in_routes;
using ores::nats::service::nats_client;
namespace account_view = ores::http::routes::iam::account_view;

namespace {

http_request sign_in_request(std::map<std::string, std::string> query,
                             std::map<std::string, std::string> path = {}) {
    http_request req;
    for (auto& [k, v] : query)
        req.query_params[k] = v;
    for (auto& [k, v] : path)
        req.path_params[k] = v;
    return req;
}

http_response
sign_in_call(nats_client& session, const std::string& pattern, const http_request& req) {
    auto router = std::make_shared<ores::http::net::router>();
    sign_in_routes::register_routes(
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

}

TEST_CASE("sign_in_routes_read_a_page_of_whole_numbers", "[sign_in]") {
    const auto defaults = sign_in_routes::read_page(sign_in_request({}));
    REQUIRE(defaults);
    CHECK(defaults->second == 100);
    CHECK(sign_in_routes::read_page(sign_in_request({{"limit", "1000"}})));
    CHECK_FALSE(sign_in_routes::read_page(sign_in_request({{"limit", "1001"}})));
    CHECK_FALSE(sign_in_routes::read_page(sign_in_request({{"offset", "1.5"}})));
}

TEST_CASE("sign_in_routes_refuse_before_forwarding", "[sign_in]") {
    nats_client session;
    const auto page = sign_in_call(session, "/api/login-info", sign_in_request({{"limit", "x"}}));
    CHECK(page.status == http_status::bad_request);
    CHECK(page.body.find("whole numbers") != std::string::npos);
    CHECK(sign_in_call(
              session, "/api/login-info/{accountId}", sign_in_request({}, {{"accountId", "nope"}}))
              .status == http_status::bad_request);
}

TEST_CASE("sign_in_routes_forward_an_accepted_read", "[sign_in]") {
    nats_client session;
    for (const auto* pattern : {"/api/sessions", "/api/sessions/active", "/api/me/sessions"})
        CHECK(sign_in_call(session, pattern, sign_in_request({})).status ==
              http_status::bad_gateway);
}

TEST_CASE("account_view_answers_the_sign_in_record_and_session_the_screens_read", "[sign_in]") {
    ores::iam::domain::login_info info;
    info.failed_logins = 2;
    info.locked = true;
    const auto record = account_view::json_of(info);
    for (const auto* key : {"\"failedLogins\":2",
                            "\"locked\":true",
                            "\"lastLogin\":null",
                            "\"lastIp\":",
                            "\"passwordResetRequired\":false"})
        CHECK(record.find(key) != std::string::npos);
    CHECK(account_view::json_of(std::optional<ores::iam::domain::login_info>{}) == "null");

    ores::iam::domain::session s;
    s.bytes_sent = 10;
    s.country_code = "PT";
    const auto sessions = account_view::json_of(std::vector<ores::iam::domain::session>{s});
    for (const auto* key : {"\"bytesSent\":10",
                            "\"countryCode\":\"PT\"",
                            "\"endTime\":null",
                            "\"startTime\":",
                            "\"clientIp\":",
                            "\"protocol\":\"binary\""})
        CHECK(sessions.find(key) != std::string::npos);
}

TEST_CASE("sign_in_routes_read_an_account_sign_ins_page_as_the_bff_did", "[sign_in]") {
    const auto defaults = sign_in_routes::read_sign_ins_page(sign_in_request({}));
    REQUIRE(defaults);
    CHECK(defaults->second == 20);
    CHECK(sign_in_routes::read_sign_ins_page(sign_in_request({{"limit", "200"}})));
    CHECK_FALSE(sign_in_routes::read_sign_ins_page(sign_in_request({{"limit", "201"}})));
    nats_client session;
    CHECK(sign_in_call(session,
                       "/api/accounts/{username}/sign-ins",
                       sign_in_request({{"limit", "0"}}, {{"username", "alice"}}))
              .status == http_status::bad_request);
    CHECK(sign_in_call(session,
                       "/api/accounts/{username}/sign-ins",
                       sign_in_request({}, {{"username", "alice"}}))
              .status == http_status::bad_gateway);
}
