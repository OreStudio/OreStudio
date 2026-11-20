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
#include "ores.http/routes/reporting/ore_schema_zip.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <memory>
#include <optional>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::reporting::ore_schema_zip;
using ores::http::routes::reporting::schema_with_includes;
using ores::http::routes::reporting::zip_of;
using namespace ores::logging;

namespace {

const std::string schema_address = "/api/reporting/ore-files/ore/schema";

/// A schema whose only include is the one it states.
std::string schema_including(const std::string& location) {
    return R"(<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">)" +
           (location.empty() ? std::string() :
                               R"(<xs:include schemaLocation=")" + location + R"("/>)") +
           "</xs:schema>";
}

http_status schema_status_of(http_request req, const std::string& expected_pattern) {
    ores::nats::service::nats_client session;
    auto table = std::make_shared<ores::http::net::router>();
    ore_schema_zip::register_routes(
        table, std::make_shared<ores::http::openapi::endpoint_registry>(), session);
    const auto route = table->match(http_method::get, schema_address, req.path_params);
    REQUIRE(route.has_value());
    REQUIRE(route->pattern == expected_pattern);
    REQUIRE(route->requires_auth);
    boost::asio::io_context io;
    std::optional<ores::http::domain::http_response> answer;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> { answer = co_await route->handler(req); },
        boost::asio::detached);
    io.run();
    REQUIRE(answer.has_value());
    return answer->status;
}

}

TEST_CASE("a_schema_with_no_includes_is_itself_alone", "[ore_schema_zip]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a schema that includes nothing";

    const std::map<std::string, std::string> contents{{"ore.xsd", schema_including("")}};
    const auto bundle = schema_with_includes("ore.xsd", contents);
    REQUIRE(bundle.files.size() == 1);
    CHECK(bundle.files[0].name == "ore.xsd");
    CHECK(bundle.missing.empty());
}

TEST_CASE("an_included_schema_travels_with_the_one_that_includes_it", "[ore_schema_zip]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing one include";

    const std::map<std::string, std::string> contents{{"ore.xsd", schema_including("common.xsd")},
                                                      {"common.xsd", schema_including("")}};
    const auto bundle = schema_with_includes("ore.xsd", contents);
    REQUIRE(bundle.files.size() == 2);
    CHECK(bundle.files[0].name == "ore.xsd");
    CHECK(bundle.files[1].name == "common.xsd");
    CHECK(bundle.missing.empty());
}

TEST_CASE("an_include_of_an_include_travels_too", "[ore_schema_zip]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing an include through another";

    const std::map<std::string, std::string> contents{{"ore.xsd", schema_including("common.xsd")},
                                                      {"common.xsd", schema_including("base.xsd")},
                                                      {"base.xsd", schema_including("")}};
    const auto bundle = schema_with_includes("ore.xsd", contents);
    REQUIRE(bundle.files.size() == 3);
    CHECK(bundle.missing.empty());
}

TEST_CASE("a_schema_included_twice_travels_once", "[ore_schema_zip]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a repeated include";

    const std::map<std::string, std::string> contents{
        {"ore.xsd", schema_including("common.xsd") + schema_including("common.xsd")},
        {"common.xsd", schema_including("")}};
    const auto bundle = schema_with_includes("ore.xsd", contents);
    CHECK(bundle.files.size() == 2);
    CHECK(bundle.missing.empty());
}

TEST_CASE("an_include_the_version_does_not_store_is_named", "[ore_schema_zip]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a missing include";

    const std::map<std::string, std::string> contents{{"ore.xsd", schema_including("absent.xsd")}};
    const auto bundle = schema_with_includes("ore.xsd", contents);
    REQUIRE(bundle.files.size() == 1);
    REQUIRE(bundle.missing.size() == 1);
    CHECK(bundle.missing[0] == "absent.xsd");
}

TEST_CASE("a_schema_the_version_does_not_store_is_a_missing_root", "[ore_schema_zip]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a version that stores no root";

    const auto bundle = schema_with_includes("ore.xsd", {});
    CHECK(bundle.files.empty());
    REQUIRE(bundle.missing.size() == 1);
    CHECK(bundle.missing[0] == "ore.xsd");
}

TEST_CASE("the_archive_is_a_zip_of_the_files_it_is_given", "[ore_schema_zip]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the archive";

    const auto bytes = zip_of({{"ore.xsd", "<xs:schema/>"}, {"common.xsd", "<xs:schema/>"}});
    REQUIRE(bytes.size() > 4);
    CHECK(bytes[0] == 'P');
    CHECK(bytes[1] == 'K');
    CHECK(bytes[2] == 3);
    CHECK(bytes[3] == 4);
    const std::string text(bytes.begin(), bytes.end());
    CHECK(text.find("ore.xsd") != std::string::npos);
    CHECK(text.find("common.xsd") != std::string::npos);
}

TEST_CASE("a_schema_request_refuses_a_query_it_does_not_read", "[ore_schema_zip]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the schema route's query";

    http_request req;
    req.query_params["version"] = "1.8.17.0";
    req.query_params["as_of"] = "now";
    CHECK(schema_status_of(req, "/api/reporting/ore-files/{code}/schema") ==
          http_status::bad_request);
}

TEST_CASE("a_schema_request_refuses_an_address_that_names_no_version", "[ore_schema_zip]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the schema route's version";

    http_request req;
    req.query_params["version"] = "";
    CHECK(schema_status_of(req, "/api/reporting/ore-files/{code}/schema") ==
          http_status::bad_request);
}

TEST_CASE("a_schema_request_with_no_service_is_refused_as_upstream", "[ore_schema_zip]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the schema route with no service behind it";

    http_request req;
    req.query_params["version"] = "1.8.17.0";
    CHECK(schema_status_of(req, "/api/reporting/ore-files/{code}/schema") ==
          http_status::bad_gateway);
}
