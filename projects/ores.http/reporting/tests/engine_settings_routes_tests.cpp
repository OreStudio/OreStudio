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
#include "ores.http/routes/reporting/engine_settings_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <optional>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::reporting::engine_settings_routes;
using namespace ores::logging;

namespace {

const std::string documentation_address = "/api/reporting/engine-settings/documentation";

/**
 * @brief The answer the documentation route gives a request, on a client that is
 * not connected, so any forward fails.
 */
http_status documentation_status_of(http_request req) {
    ores::nats::service::nats_client session;
    auto table = std::make_shared<ores::http::net::router>();
    engine_settings_routes::register_documentation_route(
        table, std::make_shared<ores::http::openapi::endpoint_registry>(), session);
    const auto route = table->match(http_method::get, documentation_address, req.path_params);
    REQUIRE(route.has_value());
    REQUIRE(route->pattern == documentation_address);
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

/**
 * @brief The answer a read route gives a request at an address, on a client that
 * is not connected, so any forward fails.
 */
http_status reads_status_of(http_method method,
                            const std::string& address,
                            http_request req,
                            const std::string& expected_pattern) {
    ores::nats::service::nats_client session;
    auto table = std::make_shared<ores::http::net::router>();
    engine_settings_routes::register_read_routes(
        table, std::make_shared<ores::http::openapi::endpoint_registry>(), session);
    const auto route = table->match(method, address, req.path_params);
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

TEST_CASE("a_documentation_page_refuses_a_query_it_does_not_read", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the documentation page's query";

    http_request req;
    req.query_params["scope"] = "Setup";
    CHECK(documentation_status_of(req) == http_status::bad_request);
}

TEST_CASE("a_documentation_page_with_no_service_is_refused_as_upstream", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a documentation page with no service behind it";

    CHECK(documentation_status_of(http_request{}) == http_status::bad_gateway);
}

TEST_CASE("a_page_of_settings_refuses_a_query_it_does_not_read", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the page of settings' query";

    http_request req;
    req.query_params["party"] = "acme";
    CHECK(reads_status_of(http_method::get,
                          "/api/reporting/engine-settings",
                          req,
                          "/api/reporting/engine-settings") == http_status::bad_request);
}

TEST_CASE("a_page_of_settings_with_no_service_is_refused_as_upstream", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a page of settings with no service behind it";

    CHECK(reads_status_of(http_method::get,
                          "/api/reporting/engine-settings",
                          http_request{},
                          "/api/reporting/engine-settings") == http_status::bad_gateway);
}

TEST_CASE("one_setting_refuses_an_id_that_is_not_an_id", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing one setting's id";

    CHECK(reads_status_of(http_method::get,
                          "/api/reporting/engine-settings/not-an-id",
                          http_request{},
                          "/api/reporting/engine-settings/{id}") == http_status::bad_request);
}

TEST_CASE("one_setting_with_no_service_is_refused_as_upstream", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing one setting with no service behind it";

    CHECK(reads_status_of(http_method::get,
                          "/api/reporting/engine-settings/00000000-0000-0000-0000-0000000000e1",
                          http_request{},
                          "/api/reporting/engine-settings/{id}") == http_status::bad_gateway);
}

TEST_CASE("a_history_refuses_an_id_that_is_not_an_id", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a history's id";

    CHECK(reads_status_of(http_method::get,
                          "/api/reporting/engine-settings/not-an-id/history",
                          http_request{},
                          "/api/reporting/engine-settings/{id}/history") ==
          http_status::bad_request);
}

TEST_CASE("a_history_with_no_service_is_refused_as_upstream", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a history with no service behind it";

    CHECK(reads_status_of(
              http_method::get,
              "/api/reporting/engine-settings/00000000-0000-0000-0000-0000000000e1/history",
              http_request{},
              "/api/reporting/engine-settings/{id}/history") == http_status::bad_gateway);
}

TEST_CASE("the_documentation_address_is_not_read_as_a_configuration_id", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the documentation address beside the reads";

    ores::nats::service::nats_client session;
    auto table = std::make_shared<ores::http::net::router>();
    auto registry = std::make_shared<ores::http::openapi::endpoint_registry>();
    engine_settings_routes::register_documentation_route(table, registry, session);
    engine_settings_routes::register_read_routes(table, registry, session);

    http_request doc_req;
    const auto doc = table->match(http_method::get, documentation_address, doc_req.path_params);
    REQUIRE(doc.has_value());
    CHECK(doc->pattern == documentation_address);

    http_request id_req;
    const auto one =
        table->match(http_method::get,
                     "/api/reporting/engine-settings/00000000-0000-0000-0000-0000000000e1",
                     id_req.path_params);
    REQUIRE(one.has_value());
    CHECK(one->pattern == "/api/reporting/engine-settings/{id}");
}

/// The answer a write route gives a request at an address, on a client that is not connected.
http_status writes_status_of(http_method method,
                             const std::string& address,
                             http_request req,
                             const std::string& expected_pattern) {
    ores::nats::service::nats_client session;
    auto table = std::make_shared<ores::http::net::router>();
    engine_settings_routes::register_write_routes(
        table, std::make_shared<ores::http::openapi::endpoint_registry>(), session);
    const auto route = table->match(method, address, req.path_params);
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

TEST_CASE("a_create_refuses_a_member_it_does_not_read", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a create's members";

    http_request req;
    req.body = R"({"name":"alpha","description":"","intent":{"reason_code":"seed"},"extra":1})";
    CHECK(writes_status_of(http_method::post,
                           "/api/reporting/engine-settings",
                           req,
                           "/api/reporting/engine-settings") == http_status::bad_request);
}

TEST_CASE("a_create_refuses_a_name_it_cannot_use", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a create's name";

    http_request req;
    req.body = R"({"name":"  ","description":"","intent":{"reason_code":"seed"}})";
    CHECK(writes_status_of(http_method::post,
                           "/api/reporting/engine-settings",
                           req,
                           "/api/reporting/engine-settings") == http_status::bad_request);
}

TEST_CASE("a_create_with_no_party_to_hold_it_is_refused", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a create that names no party";

    // A configuration belongs to a party: the body may name one, and a caller
    // whose token names none has nothing to fall back on.
    http_request req;
    req.body = R"({"name":"alpha","description":"","intent":{"reason_code":"seed"}})";
    CHECK(writes_status_of(http_method::post,
                           "/api/reporting/engine-settings",
                           req,
                           "/api/reporting/engine-settings") == http_status::bad_request);
}

TEST_CASE("a_create_with_no_service_is_refused_as_upstream", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a create with no service behind it";

    http_request req;
    req.body = R"({"name":"alpha","description":"","intent":{"reason_code":"seed"}})";
    ores::security::jwt::jwt_claims claims;
    claims.party_id = std::string("00000000-0000-0000-0000-0000000000e1");
    req.authenticated_user = claims;
    CHECK(writes_status_of(http_method::post,
                           "/api/reporting/engine-settings",
                           req,
                           "/api/reporting/engine-settings") == http_status::bad_gateway);
}

TEST_CASE("an_update_refuses_an_id_that_is_not_an_id", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing an update's id";

    http_request req;
    req.body = R"({"name":"alpha","description":"","version":1,"intent":{"reason_code":"seed"}})";
    CHECK(writes_status_of(http_method::put,
                           "/api/reporting/engine-settings/not-an-id",
                           req,
                           "/api/reporting/engine-settings/{id}") == http_status::bad_request);
}

TEST_CASE("an_update_refuses_a_version_it_was_not_read_at", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing an update's version";

    http_request req;
    req.body = R"({"name":"alpha","description":"","version":0,"intent":{"reason_code":"seed"}})";
    CHECK(writes_status_of(http_method::put,
                           "/api/reporting/engine-settings/00000000-0000-0000-0000-0000000000e1",
                           req,
                           "/api/reporting/engine-settings/{id}") == http_status::bad_request);
}

TEST_CASE("a_group_write_refuses_an_id_that_is_not_an_id", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a group write's id";

    http_request req;
    CHECK(writes_status_of(http_method::put,
                           "/api/reporting/engine-settings/not-an-id/groups/execution",
                           req,
                           "/api/reporting/engine-settings/{id}/groups/{group}") ==
          http_status::bad_request);
}

TEST_CASE("a_group_write_with_no_service_is_refused_as_upstream", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a group write with no service behind it";

    http_request req;
    req.body = R"({"version":1,"values":{"n_threads":4},"intent":{"reason_code":"seed"}})";
    CHECK(
        writes_status_of(
            http_method::put,
            "/api/reporting/engine-settings/00000000-0000-0000-0000-0000000000e1/groups/execution",
            req,
            "/api/reporting/engine-settings/{id}/groups/{group}") == http_status::bad_gateway);
}

TEST_CASE("a_removal_refuses_an_id_that_is_not_an_id", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a removal's id";

    http_request req;
    req.body = R"({"intent":{"reason_code":"seed"}})";
    CHECK(writes_status_of(http_method::delete_,
                           "/api/reporting/engine-settings/not-an-id",
                           req,
                           "/api/reporting/engine-settings/{id}") == http_status::bad_request);
}

TEST_CASE("a_removal_with_no_service_is_refused_as_upstream", "[engine_settings]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a removal with no service behind it";

    http_request req;
    req.body = R"({"intent":{"reason_code":"seed"}})";
    CHECK(writes_status_of(http_method::delete_,
                           "/api/reporting/engine-settings/00000000-0000-0000-0000-0000000000e1",
                           req,
                           "/api/reporting/engine-settings/{id}") == http_status::bad_gateway);
}
