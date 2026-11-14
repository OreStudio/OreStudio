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
#include "ores.http/routes/reporting/pricing_engine_routes.hpp"
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
using ores::http::routes::reporting::pricing_engine_routes;
using namespace ores::logging;

namespace {

const std::string engines_address = "/api/reporting/pricing-engines";

/**
 * @brief The answer a pricing engine read gives a request, on a client that is
 * not connected, so any forward fails.
 */
http_status pricing_status_of(http_method method,
                              const std::string& address,
                              http_request req,
                              const std::string& expected_pattern) {
    ores::nats::service::nats_client session;
    auto table = std::make_shared<ores::http::net::router>();
    pricing_engine_routes::register_read_routes(
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

TEST_CASE("a_page_of_pricing_engines_refuses_a_query_it_does_not_read", "[pricing_engines]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the page of pricing engines' query";

    http_request req;
    req.query_params["party"] = "acme";
    CHECK(pricing_status_of(http_method::get, engines_address, req, engines_address) ==
          http_status::bad_request);
}

TEST_CASE("a_page_of_pricing_engines_with_no_service_is_refused_as_upstream",
          "[pricing_engines]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a page of pricing engines with no service behind it";

    CHECK(pricing_status_of(http_method::get, engines_address, http_request{}, engines_address) ==
          http_status::bad_gateway);
}

TEST_CASE("one_pricing_engine_refuses_an_id_that_is_not_an_id", "[pricing_engines]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing one pricing engine's id";

    CHECK(pricing_status_of(http_method::get,
                            "/api/reporting/pricing-engines/not-an-id",
                            http_request{},
                            "/api/reporting/pricing-engines/{id}") == http_status::bad_request);
}

TEST_CASE("one_pricing_engine_with_no_service_is_refused_as_upstream", "[pricing_engines]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing one pricing engine with no service behind it";

    CHECK(pricing_status_of(
              http_method::get,
              "/api/reporting/pricing-engines/00000000-0000-0000-0000-0000000000e1",
              http_request{},
              "/api/reporting/pricing-engines/{id}") == http_status::bad_gateway);
}
