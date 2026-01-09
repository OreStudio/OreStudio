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
#include "ores.http/domain/route.hpp"

#include <regex>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.http.tests");
const std::string tags("[route]");

ores::http::domain::route make_route(const std::string& pattern,
    const std::regex& regex,
    const std::vector<std::string>& param_names) {
    ores::http::domain::route r;
    r.method = ores::http::domain::http_method::get;
    r.pattern = pattern;
    r.regex = regex;
    r.param_names = param_names;
    return r;
}

}

using namespace ores::http::domain;
using namespace ores::logging;

TEST_CASE("route_match_simple_path", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing route matching for simple path";

    auto sut = make_route("/health", std::regex("^/health$"), {});

    std::unordered_map<std::string, std::string> params;
    bool matched = sut.match("/health", params);

    CHECK(matched);
    CHECK(params.empty());
}

TEST_CASE("route_match_simple_path_no_match", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing route non-matching for different path";

    auto sut = make_route("/health", std::regex("^/health$"), {});

    std::unordered_map<std::string, std::string> params;
    bool matched = sut.match("/other", params);

    CHECK_FALSE(matched);
    CHECK(params.empty());
}

TEST_CASE("route_match_with_single_parameter", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing route matching with single path parameter";

    auto sut = make_route("/users/{id}", std::regex("^/users/([^/]+)$"), {"id"});

    std::unordered_map<std::string, std::string> params;
    bool matched = sut.match("/users/123", params);

    CHECK(matched);
    REQUIRE(params.size() == 1);
    CHECK(params["id"] == "123");
}

TEST_CASE("route_match_with_multiple_parameters", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing route matching with multiple path parameters";

    auto sut = make_route("/users/{userId}/posts/{postId}",
        std::regex("^/users/([^/]+)/posts/([^/]+)$"),
        {"userId", "postId"});

    std::unordered_map<std::string, std::string> params;
    bool matched = sut.match("/users/42/posts/100", params);

    CHECK(matched);
    REQUIRE(params.size() == 2);
    CHECK(params["userId"] == "42");
    CHECK(params["postId"] == "100");
}

TEST_CASE("route_match_with_uuid_parameter", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing route matching with UUID path parameter";

    auto sut = make_route("/accounts/{id}",
        std::regex("^/accounts/([^/]+)$"), {"id"});

    std::unordered_map<std::string, std::string> params;
    bool matched = sut.match("/accounts/550e8400-e29b-41d4-a716-446655440000", params);

    CHECK(matched);
    REQUIRE(params.size() == 1);
    CHECK(params["id"] == "550e8400-e29b-41d4-a716-446655440000");
}

TEST_CASE("route_match_partial_path_no_match", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing route non-matching for partial path";

    auto sut = make_route("/users/{id}", std::regex("^/users/([^/]+)$"), {"id"});

    std::unordered_map<std::string, std::string> params;
    bool matched = sut.match("/users", params);

    CHECK_FALSE(matched);
    CHECK(params.empty());
}

TEST_CASE("route_match_extra_segments_no_match", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing route non-matching for extra path segments";

    auto sut = make_route("/users/{id}", std::regex("^/users/([^/]+)$"), {"id"});

    std::unordered_map<std::string, std::string> params;
    bool matched = sut.match("/users/123/extra", params);

    CHECK_FALSE(matched);
    CHECK(params.empty());
}

TEST_CASE("route_default_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing route default construction";

    route sut;

    CHECK_FALSE(sut.requires_auth);
    CHECK(sut.required_roles.empty());
    CHECK(sut.summary.empty());
    CHECK(sut.description.empty());
    CHECK(sut.tags.empty());
    CHECK(sut.query_params.empty());
    CHECK_FALSE(sut.body_schema.has_value());
    CHECK_FALSE(sut.success_response_schema.has_value());
}

TEST_CASE("query_param_default_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing query_param default construction";

    query_param sut;

    CHECK(sut.type == "string");
    CHECK_FALSE(sut.required);
    CHECK(sut.format.empty());
    CHECK(sut.description.empty());
    CHECK_FALSE(sut.default_value.has_value());
}

TEST_CASE("request_body_schema_default_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing request_body_schema default construction";

    request_body_schema sut;

    CHECK(sut.content_type == "application/json");
    CHECK(sut.required);
    CHECK(sut.json_schema.empty());
    CHECK(sut.example_json.empty());
}

TEST_CASE("response_schema_default_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing response_schema default construction";

    response_schema sut;

    CHECK(sut.status_code == "200");
    CHECK(sut.description == "Successful response");
    CHECK(sut.content_type == "application/json");
    CHECK(sut.json_schema.empty());
    CHECK(sut.example_json.empty());
}
