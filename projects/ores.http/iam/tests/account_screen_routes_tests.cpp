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
#include "ores.http/routes/iam/account_screen_routes.hpp"
#include "ores.http/routes/iam/account_view.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <memory>
#include <optional>
#include <string>

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::routes::iam::account_screen_routes;
using ores::nats::service::nats_client;
namespace account_view = ores::http::routes::iam::account_view;

namespace {

http_request screen_request(std::map<std::string, std::string> query = {},
                            std::map<std::string, std::string> path = {},
                            std::string body = {}) {
    http_request req;
    for (auto& [k, v] : query)
        req.query_params[k] = v;
    for (auto& [k, v] : path)
        req.path_params[k] = v;
    req.body = std::move(body);
    return req;
}

http_response
screen_call(nats_client& session, const std::string& pattern, const http_request& req) {
    auto router = std::make_shared<ores::http::net::router>();
    account_screen_routes::register_routes(
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

TEST_CASE("account_screen_routes_read_a_page_within_the_bff_limits", "[account_screen]") {
    const auto defaults = account_screen_routes::read_page_query(screen_request());
    REQUIRE(defaults);
    CHECK(defaults->limit == 100);
    CHECK_FALSE(defaults->filter);
    const auto searched = account_screen_routes::read_page_query(screen_request(
        {{"search", "  ali  "}, {"sort", "full_name"}, {"descending", "true"}, {"limit", "1000"}}));
    REQUIRE(searched);
    CHECK(searched->filter->search == "ali");
    CHECK(searched->order.field == "full_name");
    CHECK(searched->order.descending);
    CHECK_FALSE(account_screen_routes::read_page_query(screen_request({{"limit", "1001"}})));
    CHECK_FALSE(account_screen_routes::read_page_query(screen_request({{"sort", "email"}})));
    CHECK_FALSE(account_screen_routes::read_page_query(screen_request({{"descending", "yes"}})));
    CHECK_FALSE(account_screen_routes::read_page_query(
        screen_request({{"search", std::string(257, 'a')}})));
}

TEST_CASE("account_screen_routes_refuse_before_forwarding", "[account_screen]") {
    nats_client session;
    const auto page = screen_call(session, "/api/accounts", screen_request({{"limit", "0"}}));
    CHECK(page.status == http_status::bad_request);
    CHECK(page.body.find("A page of people") != std::string::npos);
    const auto party = screen_call(session, "/api/me/default-party", screen_request({}, {}, "{}"));
    CHECK(party.status == http_status::bad_request);
    CHECK(party.body.find("A partyId is required.") != std::string::npos);
}

TEST_CASE("account_screen_routes_forward_an_accepted_request", "[account_screen]") {
    nats_client session;
    CHECK(screen_call(session, "/api/accounts", screen_request()).status ==
          http_status::bad_gateway);
    CHECK(screen_call(
              session, "/api/accounts/{username}", screen_request({}, {{"username", "alice"}}))
              .status == http_status::bad_gateway);
    CHECK(screen_call(session, "/api/me/default-party", screen_request({}, {}, R"({"partyId":""})"))
              .status == http_status::bad_gateway);
}

TEST_CASE("account_view_answers_the_account_the_screens_read", "[account_screen]") {
    ores::iam::domain::account a;
    a.username = "alice";
    a.full_name = "Alice \"Al\" Smith";
    a.account_type = "robot";
    a.reports_to_account_id = boost::uuids::uuid{};
    a.image_id = boost::uuids::string_generator()("2f1c9a40-1e7b-4c2b-9d7a-5b8e3f4a6c10");
    const auto text = account_view::json_of(a);
    for (const auto* key : {"\"username\":\"alice\"",
                            R"("fullName":"Alice \"Al\" Smith")",
                            "\"accountType\":\"user\"",
                            "\"reportsToAccountId\":null",
                            "\"defaultPartyId\":null",
                            "\"imageId\":\"2f1c9a40-1e7b-4c2b-9d7a-5b8e3f4a6c10\"",
                            "\"tenantId\":",
                            "\"recordedAt\":",
                            "\"changeReasonCode\":"})
        CHECK(text.find(key) != std::string::npos);
    CHECK(account_view::json_of(std::optional<ores::iam::domain::account>{}) == "null");

    ores::iam::messaging::list_accounts_response page;
    page.accounts.push_back(a);
    page.total = 7;
    const auto listed = account_screen_routes::page_json(page);
    CHECK(listed.starts_with("{\"accounts\":[{"));
    CHECK(listed.ends_with("],\"totalCount\":7}"));
}
