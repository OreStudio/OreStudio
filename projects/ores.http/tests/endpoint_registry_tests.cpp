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
#include "ores.http/openapi/endpoint_registry.hpp"
#include "ores.http/domain/route.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.telemetry/log/make_logger.hpp"

namespace {

const std::string test_suite("ores.http.tests");
const std::string tags("[endpoint_registry]");

ores::http::domain::route make_test_route(
    ores::http::domain::http_method method,
    const std::string& pattern,
    const std::string& summary = "",
    const std::string& description = "",
    bool requires_auth = false) {

    ores::http::domain::route r;
    r.method = method;
    r.pattern = pattern;
    r.summary = summary;
    r.description = description;
    r.requires_auth = requires_auth;
    return r;
}

}

using namespace ores::http::openapi;
using namespace ores::http::domain;
using namespace ores::telemetry::log;

TEST_CASE("endpoint_registry_generates_minimal_openapi_spec", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing minimal OpenAPI spec generation";

    endpoint_registry registry;
    auto json = registry.generate_openapi_json();

    REQUIRE_FALSE(json.empty());
    REQUIRE(json.find("\"openapi\":\"3.0.3\"") != std::string::npos);
}

TEST_CASE("endpoint_registry_generates_spec_with_info", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing OpenAPI spec with info";

    endpoint_registry registry;
    api_info info;
    info.title = "Test API";
    info.description = "A test API for unit testing";
    info.version = "2.0.0";
    registry.set_info(info);

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"title\":\"Test API\"") != std::string::npos);
    REQUIRE(json.find("\"description\":\"A test API for unit testing\"") != std::string::npos);
    REQUIRE(json.find("\"version\":\"2.0.0\"") != std::string::npos);
}

TEST_CASE("endpoint_registry_generates_spec_with_contact", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing OpenAPI spec with contact info";

    endpoint_registry registry;
    api_info info;
    info.title = "Test API";
    info.contact_name = "John Doe";
    info.contact_email = "john@example.com";
    registry.set_info(info);

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"name\":\"John Doe\"") != std::string::npos);
    REQUIRE(json.find("\"email\":\"john@example.com\"") != std::string::npos);
}

TEST_CASE("endpoint_registry_generates_spec_with_license", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing OpenAPI spec with license info";

    endpoint_registry registry;
    api_info info;
    info.title = "Test API";
    info.license_name = "MIT";
    info.license_url = "https://opensource.org/licenses/MIT";
    registry.set_info(info);

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("MIT") != std::string::npos);
    REQUIRE(json.find("https://opensource.org/licenses/MIT") != std::string::npos);
}

TEST_CASE("endpoint_registry_generates_spec_with_servers", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing OpenAPI spec with servers";

    endpoint_registry registry;
    registry.add_server({"https://api.example.com", "Production server"});
    registry.add_server({"https://staging.example.com", "Staging server"});

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("https://api.example.com") != std::string::npos);
    REQUIRE(json.find("Production server") != std::string::npos);
    REQUIRE(json.find("https://staging.example.com") != std::string::npos);
    REQUIRE(json.find("Staging server") != std::string::npos);
}

TEST_CASE("endpoint_registry_generates_default_server_when_none_added", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing OpenAPI spec with default server";

    endpoint_registry registry;
    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"/\"") != std::string::npos);
    REQUIRE(json.find("Default server") != std::string::npos);
}

TEST_CASE("endpoint_registry_registers_get_route", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing GET route registration";

    endpoint_registry registry;
    registry.register_route(make_test_route(http_method::get, "/health", "Health check"));

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"/health\"") != std::string::npos);
    REQUIRE(json.find("\"get\"") != std::string::npos);
    REQUIRE(json.find("\"summary\":\"Health check\"") != std::string::npos);
}

TEST_CASE("endpoint_registry_registers_post_route", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing POST route registration";

    endpoint_registry registry;
    registry.register_route(make_test_route(http_method::post, "/users", "Create user"));

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"/users\"") != std::string::npos);
    REQUIRE(json.find("\"post\"") != std::string::npos);
}

TEST_CASE("endpoint_registry_registers_put_route", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing PUT route registration";

    endpoint_registry registry;
    registry.register_route(make_test_route(http_method::put, "/users/{id}", "Update user"));

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"/users/{id}\"") != std::string::npos);
    REQUIRE(json.find("\"put\"") != std::string::npos);
}

TEST_CASE("endpoint_registry_registers_patch_route", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing PATCH route registration";

    endpoint_registry registry;
    registry.register_route(make_test_route(http_method::patch, "/users/{id}", "Patch user"));

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"patch\"") != std::string::npos);
}

TEST_CASE("endpoint_registry_registers_delete_route", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing DELETE route registration";

    endpoint_registry registry;
    registry.register_route(make_test_route(http_method::delete_, "/users/{id}", "Delete user"));

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"delete\"") != std::string::npos);
}

TEST_CASE("endpoint_registry_registers_head_route", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing HEAD route registration";

    endpoint_registry registry;
    registry.register_route(make_test_route(http_method::head, "/users", "Check users"));

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"head\"") != std::string::npos);
}

TEST_CASE("endpoint_registry_registers_options_route", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing OPTIONS route registration";

    endpoint_registry registry;
    registry.register_route(make_test_route(http_method::options, "/users", "Options for users"));

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"options\"") != std::string::npos);
}

TEST_CASE("endpoint_registry_includes_path_parameters", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing path parameter inclusion";

    endpoint_registry registry;

    route r;
    r.method = http_method::get;
    r.pattern = "/users/{userId}/posts/{postId}";
    r.param_names = {"userId", "postId"};
    registry.register_route(r);

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"name\":\"userId\"") != std::string::npos);
    REQUIRE(json.find("\"name\":\"postId\"") != std::string::npos);
    REQUIRE(json.find("\"in\":\"path\"") != std::string::npos);
    REQUIRE(json.find("\"required\":true") != std::string::npos);
}

TEST_CASE("endpoint_registry_includes_authentication_requirement", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing authentication requirement inclusion";

    endpoint_registry registry;
    registry.register_route(make_test_route(
        http_method::get, "/protected", "Protected endpoint", "", true));

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"bearerAuth\"") != std::string::npos);
    REQUIRE(json.find("\"401\"") != std::string::npos);
    REQUIRE(json.find("\"403\"") != std::string::npos);
}

TEST_CASE("endpoint_registry_includes_tags", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing tags inclusion";

    endpoint_registry registry;

    route r;
    r.method = http_method::get;
    r.pattern = "/users";
    r.tags = {"Users", "Admin"};
    registry.register_route(r);

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"tags\"") != std::string::npos);
    REQUIRE(json.find("\"Users\"") != std::string::npos);
    REQUIRE(json.find("\"Admin\"") != std::string::npos);
}

TEST_CASE("endpoint_registry_includes_description", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing description inclusion";

    endpoint_registry registry;
    registry.register_route(make_test_route(
        http_method::get, "/users", "Get all users",
        "Returns a paginated list of all users in the system"));

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"description\":\"Returns a paginated list of all users in the system\"")
        != std::string::npos);
}

TEST_CASE("endpoint_registry_includes_standard_responses", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing standard response inclusion";

    endpoint_registry registry;
    registry.register_route(make_test_route(http_method::get, "/health", "Health check"));

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"200\"") != std::string::npos);
    REQUIRE(json.find("\"500\"") != std::string::npos);
}

TEST_CASE("endpoint_registry_generates_security_components", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing security components generation";

    endpoint_registry registry;
    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"securitySchemes\"") != std::string::npos);
    REQUIRE(json.find("\"bearerAuth\"") != std::string::npos);
    REQUIRE(json.find("\"type\":\"http\"") != std::string::npos);
    REQUIRE(json.find("\"scheme\":\"bearer\"") != std::string::npos);
    REQUIRE(json.find("\"bearerFormat\":\"JWT\"") != std::string::npos);
}

TEST_CASE("endpoint_registry_multiple_methods_same_path", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing multiple methods on same path";

    endpoint_registry registry;
    registry.register_route(make_test_route(http_method::get, "/users", "List users"));
    registry.register_route(make_test_route(http_method::post, "/users", "Create user"));

    auto json = registry.generate_openapi_json();

    REQUIRE(json.find("\"/users\"") != std::string::npos);
    REQUIRE(json.find("\"get\"") != std::string::npos);
    REQUIRE(json.find("\"post\"") != std::string::npos);
}

TEST_CASE("endpoint_registry_generates_swagger_ui_html", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing Swagger UI HTML generation";

    endpoint_registry registry;
    api_info info;
    info.title = "My Test API";
    registry.set_info(info);

    auto html = registry.generate_swagger_ui_html("/api/openapi.json");

    REQUIRE_FALSE(html.empty());
    REQUIRE(html.find("<!DOCTYPE html>") != std::string::npos);
    REQUIRE(html.find("swagger-ui") != std::string::npos);
    REQUIRE(html.find("/api/openapi.json") != std::string::npos);
    REQUIRE(html.find("My Test API") != std::string::npos);
}

TEST_CASE("endpoint_registry_generates_swagger_ui_html_default_spec_url", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing Swagger UI HTML with default spec URL";

    endpoint_registry registry;
    auto html = registry.generate_swagger_ui_html();

    REQUIRE(html.find("/openapi.json") != std::string::npos);
}
