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
#include "ores.http/domain/http_response.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.telemetry/log/make_logger.hpp"

namespace {

const std::string test_suite("ores.http.tests");
const std::string tags("[http_response]");

}

using namespace ores::http::domain;
using namespace ores::telemetry::log;

TEST_CASE("http_response_json_creates_json_response", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing json() creates proper JSON response";

    auto sut = http_response::json(R"({"status":"ok"})");

    CHECK(sut.status == http_status::ok);
    CHECK(sut.body == R"({"status":"ok"})");
    CHECK(sut.content_type == "application/json");
}

TEST_CASE("http_response_json_with_custom_status", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing json() with custom status code";

    auto sut = http_response::json(R"({"id":123})", http_status::created);

    CHECK(sut.status == http_status::created);
    CHECK(sut.body == R"({"id":123})");
    CHECK(sut.content_type == "application/json");
}

TEST_CASE("http_response_error_creates_error_response", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing error() creates error response with JSON body";

    auto sut = http_response::error(http_status::bad_request, "Invalid input");

    CHECK(sut.status == http_status::bad_request);
    CHECK(sut.content_type == "application/json");
    CHECK(sut.body == R"({"error":"Invalid input"})");
}

TEST_CASE("http_response_not_found_creates_404", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing not_found() creates 404 response";

    auto sut = http_response::not_found("Resource not found");

    CHECK(sut.status == http_status::not_found);
    CHECK(sut.content_type == "application/json");
    CHECK(sut.body == R"({"error":"Resource not found"})");
}

TEST_CASE("http_response_not_found_with_default_message", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing not_found() with default message";

    auto sut = http_response::not_found();

    CHECK(sut.status == http_status::not_found);
    CHECK(sut.body == R"({"error":"Not Found"})");
}

TEST_CASE("http_response_unauthorized_creates_401", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing unauthorized() creates 401 response";

    auto sut = http_response::unauthorized("Token expired");

    CHECK(sut.status == http_status::unauthorized);
    CHECK(sut.content_type == "application/json");
    CHECK(sut.body == R"({"error":"Token expired"})");
}

TEST_CASE("http_response_unauthorized_with_default_message", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing unauthorized() with default message";

    auto sut = http_response::unauthorized();

    CHECK(sut.status == http_status::unauthorized);
    CHECK(sut.body == R"({"error":"Unauthorized"})");
}

TEST_CASE("http_response_forbidden_creates_403", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing forbidden() creates 403 response";

    auto sut = http_response::forbidden("Access denied");

    CHECK(sut.status == http_status::forbidden);
    CHECK(sut.content_type == "application/json");
    CHECK(sut.body == R"({"error":"Access denied"})");
}

TEST_CASE("http_response_forbidden_with_default_message", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing forbidden() with default message";

    auto sut = http_response::forbidden();

    CHECK(sut.status == http_status::forbidden);
    CHECK(sut.body == R"({"error":"Forbidden"})");
}

TEST_CASE("http_response_bad_request_creates_400", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing bad_request() creates 400 response";

    auto sut = http_response::bad_request("Missing required field");

    CHECK(sut.status == http_status::bad_request);
    CHECK(sut.content_type == "application/json");
    CHECK(sut.body == R"({"error":"Missing required field"})");
}

TEST_CASE("http_response_internal_error_creates_500", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing internal_error() creates 500 response";

    auto sut = http_response::internal_error("Database connection failed");

    CHECK(sut.status == http_status::internal_server_error);
    CHECK(sut.content_type == "application/json");
    CHECK(sut.body == R"({"error":"Database connection failed"})");
}

TEST_CASE("http_response_set_header_adds_header", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing set_header() adds header";

    http_response sut;
    sut.set_header("X-Custom-Header", "custom-value");

    CHECK(sut.headers.size() == 1);
    CHECK(sut.headers["X-Custom-Header"] == "custom-value");
}

TEST_CASE("http_response_set_header_returns_self_for_chaining", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing set_header() returns self for method chaining";

    http_response sut;
    sut.set_header("Header1", "value1")
       .set_header("Header2", "value2")
       .set_header("Header3", "value3");

    CHECK(sut.headers.size() == 3);
    CHECK(sut.headers["Header1"] == "value1");
    CHECK(sut.headers["Header2"] == "value2");
    CHECK(sut.headers["Header3"] == "value3");
}

TEST_CASE("http_response_default_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing http_response default construction";

    http_response sut;

    CHECK(sut.status == http_status::ok);
    CHECK(sut.content_type == "application/json");
    CHECK(sut.body.empty());
    CHECK(sut.headers.empty());
}
