/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.http/net/router.hpp"
#include "ores.http/domain/http_response.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.http.tests");
const std::string tags("[router]");

}

using namespace ores::http::net;
using namespace ores::http::domain;
using namespace ores::logging;

TEST_CASE("router_matches_simple_path", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing simple path matching";

    router r;
    auto builder = r.get("/health")
        .summary("Health check")
        .handler([](const http_request&) -> boost::asio::awaitable<http_response> {
            co_return http_response::json(R"({"status":"ok"})");
        });
    r.add_route(builder.build());

    std::unordered_map<std::string, std::string> params;
    auto match = r.match(http_method::get, "/health", params);

    REQUIRE(match.has_value());
    REQUIRE(match->pattern == "/health");
    REQUIRE(params.empty());
}

TEST_CASE("router_matches_path_with_parameter", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing path parameter matching";

    router r;
    auto builder = r.get("/users/{id}")
        .summary("Get user")
        .handler([](const http_request&) -> boost::asio::awaitable<http_response> {
            co_return http_response::json(R"({"id":"123"})");
        });
    r.add_route(builder.build());

    std::unordered_map<std::string, std::string> params;
    auto match = r.match(http_method::get, "/users/123", params);

    REQUIRE(match.has_value());
    REQUIRE(match->pattern == "/users/{id}");
    REQUIRE(params.size() == 1);
    REQUIRE(params["id"] == "123");
}

TEST_CASE("router_matches_path_with_multiple_parameters", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing multiple path parameters";

    router r;
    auto builder = r.get("/users/{userId}/posts/{postId}")
        .summary("Get user post")
        .handler([](const http_request&) -> boost::asio::awaitable<http_response> {
            co_return http_response::json(R"({})");
        });
    r.add_route(builder.build());

    std::unordered_map<std::string, std::string> params;
    auto match = r.match(http_method::get, "/users/42/posts/100", params);

    REQUIRE(match.has_value());
    REQUIRE(params.size() == 2);
    REQUIRE(params["userId"] == "42");
    REQUIRE(params["postId"] == "100");
}

TEST_CASE("router_returns_empty_for_unmatched_path", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing unmatched path";

    router r;
    auto builder = r.get("/health")
        .handler([](const http_request&) -> boost::asio::awaitable<http_response> {
            co_return http_response::json(R"({})");
        });
    r.add_route(builder.build());

    std::unordered_map<std::string, std::string> params;
    auto match = r.match(http_method::get, "/unknown", params);

    REQUIRE_FALSE(match.has_value());
}

TEST_CASE("router_distinguishes_methods", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing method distinction";

    router r;
    r.add_route(r.get("/resource")
        .handler([](const http_request&) -> boost::asio::awaitable<http_response> {
            co_return http_response::json(R"({"method":"get"})");
        }).build());
    r.add_route(r.post("/resource")
        .handler([](const http_request&) -> boost::asio::awaitable<http_response> {
            co_return http_response::json(R"({"method":"post"})");
        }).build());

    std::unordered_map<std::string, std::string> params;

    auto get_match = r.match(http_method::get, "/resource", params);
    REQUIRE(get_match.has_value());

    auto post_match = r.match(http_method::post, "/resource", params);
    REQUIRE(post_match.has_value());

    auto put_match = r.match(http_method::put, "/resource", params);
    REQUIRE_FALSE(put_match.has_value());
}
