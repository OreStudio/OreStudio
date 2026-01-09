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
#include "ores.http/domain/http_request.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.http.tests");
const std::string tags("[http_request]");

}

using namespace ores::http::domain;
using namespace ores::logging;

TEST_CASE("http_request_get_header_returns_value", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_header returns value for existing header";

    http_request sut;
    sut.headers["content-type"] = "application/json";

    CHECK(sut.get_header("content-type") == "application/json");
}

TEST_CASE("http_request_get_header_case_insensitive", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_header is case insensitive";

    http_request sut;
    sut.headers["content-type"] = "application/json";

    CHECK(sut.get_header("Content-Type") == "application/json");
    CHECK(sut.get_header("CONTENT-TYPE") == "application/json");
}

TEST_CASE("http_request_get_header_returns_empty_for_missing", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_header returns empty string for missing header";

    http_request sut;

    CHECK(sut.get_header("nonexistent").empty());
}

TEST_CASE("http_request_get_query_param_returns_value", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_query_param returns value for existing param";

    http_request sut;
    sut.query_params["page"] = "1";
    sut.query_params["limit"] = "10";

    CHECK(sut.get_query_param("page") == "1");
    CHECK(sut.get_query_param("limit") == "10");
}

TEST_CASE("http_request_get_query_param_returns_empty_for_missing", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_query_param returns empty for missing param";

    http_request sut;

    CHECK(sut.get_query_param("nonexistent").empty());
}

TEST_CASE("http_request_get_path_param_returns_value", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_path_param returns value for existing param";

    http_request sut;
    sut.path_params["id"] = "123";
    sut.path_params["userId"] = "456";

    CHECK(sut.get_path_param("id") == "123");
    CHECK(sut.get_path_param("userId") == "456");
}

TEST_CASE("http_request_get_path_param_returns_empty_for_missing", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_path_param returns empty for missing param";

    http_request sut;

    CHECK(sut.get_path_param("nonexistent").empty());
}

TEST_CASE("http_request_get_bearer_token_returns_token", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_bearer_token extracts token from Authorization header";

    http_request sut;
    sut.headers["authorization"] = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9";

    auto token = sut.get_bearer_token();

    REQUIRE(token.has_value());
    CHECK(*token == "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9");
}

TEST_CASE("http_request_get_bearer_token_returns_nullopt_for_missing_header", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_bearer_token returns nullopt when Authorization missing";

    http_request sut;

    auto token = sut.get_bearer_token();

    CHECK_FALSE(token.has_value());
}

TEST_CASE("http_request_get_bearer_token_returns_nullopt_for_non_bearer", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_bearer_token returns nullopt for non-Bearer auth";

    http_request sut;
    sut.headers["authorization"] = "Basic dXNlcjpwYXNz";

    auto token = sut.get_bearer_token();

    CHECK_FALSE(token.has_value());
}

TEST_CASE("http_request_get_bearer_token_handles_empty_auth_header", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_bearer_token handles empty Authorization header";

    http_request sut;
    sut.headers["authorization"] = "";

    auto token = sut.get_bearer_token();

    CHECK_FALSE(token.has_value());
}

TEST_CASE("http_request_default_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing http_request default construction";

    http_request sut;

    CHECK(sut.http_version_major == 1);
    CHECK(sut.http_version_minor == 1);
    CHECK(sut.target.empty());
    CHECK(sut.body.empty());
    CHECK(sut.headers.empty());
    CHECK(sut.query_params.empty());
    CHECK(sut.path_params.empty());
    CHECK_FALSE(sut.authenticated_user.has_value());
}
