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
#include "ores.http/routes/refdata/curve_configuration_screen_routes.hpp"
#include "ores.logging/logger.hpp"
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
using ores::http::routes::refdata::curve_configuration_screen_routes;
using namespace ores::logging;

namespace {

const std::string configurations_address = "/api/refdata/curve-configurations";
const std::string configuration_address =
    "/api/refdata/curve-configurations/00000000-0000-0000-0000-000000000001";

/**
 * @brief The answer the curve configuration page gives a request, on a client
 * that is not connected, so any forward fails.
 */
http_status configuration_status_of(http_request req,
                                    std::string address = configurations_address,
                                    std::string pattern = configurations_address,
                                    http_method method = http_method::get) {
    ores::nats::service::nats_client session;
    auto table = std::make_shared<ores::http::net::router>();
    curve_configuration_screen_routes::register_routes(
        table, std::make_shared<ores::http::openapi::endpoint_registry>(), session);
    const auto route = table->match(method, address, req.path_params);
    REQUIRE(route.has_value());
    REQUIRE(route->pattern == pattern);
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

TEST_CASE("a_page_of_curve_configurations_refuses_a_query_it_does_not_read",
          "[curve_configurations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the curve configuration page's query";

    http_request req;
    req.query_params["party"] = "acme";
    CHECK(configuration_status_of(req) == http_status::bad_request);
}

TEST_CASE("a_page_of_curve_configurations_with_no_service_is_refused_as_upstream",
          "[curve_configurations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the curve configuration page with no service behind it";

    CHECK(configuration_status_of(http_request{}) == http_status::bad_gateway);
}

TEST_CASE("one_curve_configuration_refuses_an_identifier_that_is_not_one",
          "[curve_configurations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the curve configuration document's identifier";

    CHECK(configuration_status_of(http_request{},
                                  "/api/refdata/curve-configurations/not-an-id",
                                  "/api/refdata/curve-configurations/{id}") ==
          http_status::bad_request);
}

TEST_CASE("one_curve_configuration_refuses_a_party_that_is_not_one", "[curve_configurations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the curve configuration document's party";

    http_request req;
    req.query_params["party"] = "acme";
    CHECK(configuration_status_of(
              req, configuration_address, "/api/refdata/curve-configurations/{id}") ==
          http_status::bad_request);
}

TEST_CASE("one_curve_configuration_refuses_a_query_it_does_not_read", "[curve_configurations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the curve configuration document's query";

    http_request req;
    req.query_params["asOf"] = "2026-01-01";
    CHECK(configuration_status_of(
              req, configuration_address, "/api/refdata/curve-configurations/{id}") ==
          http_status::bad_request);
}

TEST_CASE("one_curve_configuration_with_no_service_is_refused_as_upstream",
          "[curve_configurations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the curve configuration document with no service behind it";

    CHECK(configuration_status_of(
              http_request{}, configuration_address, "/api/refdata/curve-configurations/{id}") ==
          http_status::bad_gateway);
}

TEST_CASE("a_curve_amendment_refuses_a_body_it_cannot_read", "[curve_configurations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the curve amendment's body";

    http_request req;
    req.body = "not a body";
    CHECK(configuration_status_of(req,
                                  configuration_address,
                                  "/api/refdata/curve-configurations/{id}",
                                  http_method::put) == http_status::bad_request);

    req.body = R"({"version":3,"reasonCode":"desk.change","changes":[]})";
    CHECK(configuration_status_of(req,
                                  configuration_address,
                                  "/api/refdata/curve-configurations/{id}",
                                  http_method::put) == http_status::bad_request);
}

TEST_CASE("a_curve_amendment_refuses_a_reason_it_cannot_state", "[curve_configurations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the curve amendment's reason";

    http_request req;
    req.body = R"({"version":3,"reasonCode":"  ","changes":[)"
               R"({"entity":"yield_curve_config","entryId":"00000000-0000-0000-0000-00000000000a",)"
               R"("column":"currency","value":"EUR"}]})";
    CHECK(configuration_status_of(req,
                                  configuration_address,
                                  "/api/refdata/curve-configurations/{id}",
                                  http_method::put) == http_status::bad_request);
}

TEST_CASE("a_curve_amendment_with_no_service_is_refused_as_upstream", "[curve_configurations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a curve amendment with no service behind it";

    http_request req;
    req.body = R"({"version":3,"reasonCode":"desk.change","changes":[)"
               R"({"entity":"yield_curve_config","entryId":"00000000-0000-0000-0000-00000000000a",)"
               R"("column":"currency","value":"EUR"}]})";
    CHECK(configuration_status_of(req,
                                  configuration_address,
                                  "/api/refdata/curve-configurations/{id}",
                                  http_method::put) == http_status::bad_gateway);
}
