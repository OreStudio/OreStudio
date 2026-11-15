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
#include "ores.http/routes/iam/profile_routes.hpp"
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
using ores::http::routes::iam::profile_routes;
using ores::nats::service::nats_client;
namespace account_view = ores::http::routes::iam::account_view;

namespace {

const std::string profile_account = "2f1c9a40-1e7b-4c2b-9d7a-5b8e3f4a6c10";

http_request profile_request(std::map<std::string, std::string> path, std::string body = {}) {
    http_request req;
    for (auto& [k, v] : path)
        req.path_params[k] = v;
    req.body = std::move(body);
    return req;
}

http_response profile_call(nats_client& session,
                           http_method method,
                           const std::string& pattern,
                           const http_request& req) {
    auto router = std::make_shared<ores::http::net::router>();
    profile_routes::register_routes(
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

TEST_CASE("profile_routes_read_the_bodies_as_the_bff_did", "[profile]") {
    CHECK(profile_routes::read_profile(""));
    CHECK(profile_routes::read_profile(R"({"fullName":"Alice","expectedVersion":"3"})"));
    CHECK_FALSE(profile_routes::read_profile(R"({"fullName":3})"));
    CHECK(profile_routes::read_contact(R"({"city":"Lisbon","version":null})"));
    CHECK(profile_routes::read_contact(R"({"city":"Lisbon","version":2})"));
    CHECK_FALSE(profile_routes::read_contact(R"({"version":-1})"));
    CHECK_FALSE(profile_routes::read_contact(R"({"version":4294967296})"));
    CHECK_FALSE(profile_routes::read_contact(R"({"phone":true})"));
}

TEST_CASE("profile_routes_refuse_a_write_against_a_moved_record", "[profile]") {
    CHECK_FALSE(profile_routes::moved(3, std::nullopt));
    CHECK_FALSE(profile_routes::moved(3, std::string{}));
    CHECK_FALSE(profile_routes::moved(3, std::string{"3"}));
    CHECK(profile_routes::moved(4, std::string{"3"}));
}

TEST_CASE("profile_routes_refuse_before_forwarding", "[profile]") {
    nats_client session;
    CHECK(
        profile_call(
            session, http_method::put, "/api/me/profile", profile_request({}, R"({"jobTitle":1})"))
            .status == http_status::bad_request);
    CHECK(profile_call(session,
                       http_method::get,
                       "/api/accounts/{accountId}/contact-information",
                       profile_request({{"accountId", "nope"}}))
              .status == http_status::not_found);
    const auto contact =
        profile_call(session,
                     http_method::put,
                     "/api/accounts/{accountId}/contact-information",
                     profile_request({{"accountId", profile_account}}, R"({"version":-2})"));
    CHECK(contact.status == http_status::bad_request);
    CHECK(contact.body.find("the version the panel read") != std::string::npos);
}

TEST_CASE("profile_routes_forward_an_accepted_request", "[profile]") {
    nats_client session;
    CHECK(
        profile_call(session, http_method::get, "/api/me/contact-information", profile_request({}))
            .status == http_status::bad_gateway);
    CHECK(profile_call(session,
                       http_method::put,
                       "/api/accounts/{username}/profile",
                       profile_request({{"username", "alice"}}, "{}"))
              .status == http_status::bad_gateway);
}

TEST_CASE("account_view_answers_the_contact_record_the_screens_read", "[profile]") {
    ores::iam::domain::account_contact_information c;
    c.street_line_1 = "1 Rua";
    c.web_page = "https://example.org";
    const auto text = account_view::json_of(c);
    for (const auto* key : {"\"streetLine1\":\"1 Rua\"",
                            "\"streetLine2\":\"\"",
                            "\"accountId\":",
                            "\"countryCode\":",
                            "\"webPage\":\"https://example.org\"",
                            "\"recordedAt\":",
                            "\"performedBy\":"})
        CHECK(text.find(key) != std::string::npos);
    CHECK(account_view::json_of(std::optional<ores::iam::domain::account_contact_information>{}) ==
          "null");
}
