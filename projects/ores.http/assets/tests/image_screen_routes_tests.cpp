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
#include "ores.http.api/net/image_answer.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/assets/image_screen_routes.hpp"
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
using ores::http::routes::assets::image_screen_routes;
using ores::nats::service::nats_client;

namespace {

http_request image_request(std::map<std::string, std::string> query,
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

http_response image_call(nats_client& session,
                         http_method method,
                         const std::string& pattern,
                         const http_request& req) {
    auto router = std::make_shared<ores::http::net::router>();
    image_screen_routes::register_routes(
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

TEST_CASE("image_screen_routes_read_a_page_and_an_upload_as_the_bff_did", "[image_screen]") {
    const auto page = image_screen_routes::read_page(image_request({{"search", "  flag  "}}));
    REQUIRE(page);
    CHECK(page->limit == 100);
    CHECK(page->search == "flag");
    CHECK_FALSE(image_screen_routes::read_page(image_request({{"limit", "501"}})));
    CHECK(image_screen_routes::read_upload(R"({"mimeType":"image/png","data":"AAAA"})"));
    CHECK_FALSE(image_screen_routes::read_upload(R"({"mimeType":"","data":"AAAA"})"));
    CHECK_FALSE(image_screen_routes::read_upload(R"({"mimeType":"image/png","data":""})"));
}

TEST_CASE("image_screen_routes_refuse_before_forwarding", "[image_screen]") {
    nats_client session;
    CHECK(image_call(session, http_method::get, "/api/images", image_request({{"limit", "0"}}))
              .status == http_status::bad_request);
    CHECK(image_call(
              session, http_method::get, "/api/images/{id}", image_request({}, {{"id", "nope"}}))
              .status == http_status::not_found);
    CHECK(
        image_call(session, http_method::post, "/api/images", image_request({}, {}, "{}")).status ==
        http_status::bad_request);
}

TEST_CASE("image_screen_routes_forward_an_accepted_request", "[image_screen]") {
    nats_client session;
    CHECK(image_call(session, http_method::get, "/api/image-upload-policy", image_request({}))
              .status == http_status::bad_gateway);
}

TEST_CASE("image_answer_serves_only_an_image_type", "[image_screen]") {
    const auto svg = ores::http::net::image_answer("", "<svg/>");
    CHECK(svg.content_type == "image/svg+xml");
    CHECK(svg.headers.at("Content-Security-Policy") == "default-src 'none'; sandbox");
    CHECK(ores::http::net::image_answer("image/png", "x").content_type == "image/png");
    CHECK(ores::http::net::image_answer("text/html", "x").content_type ==
          "application/octet-stream");
}
