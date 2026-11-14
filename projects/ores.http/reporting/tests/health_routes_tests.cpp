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
#include "ores.http/routes/reporting/reporting_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <optional>
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::reporting::reporting_routes;
using namespace ores::logging;

namespace {

const std::string health_address = "/api/reporting/report-health";

/**
 * @brief The answer the health route gives a request, on a client that is not
 * connected, so any forward fails.
 */
http_status status_of(http_request req) {
    ores::nats::service::nats_client session;
    auto table = std::make_shared<ores::http::net::router>();
    reporting_routes::register_health_routes(
        table, std::make_shared<ores::http::openapi::endpoint_registry>(), session);
    const auto route = table->match(http_method::get, health_address, req.path_params);
    REQUIRE(route.has_value());
    REQUIRE(route->pattern == health_address);
    REQUIRE(route->requires_auth);
    boost::asio::io_context io;
    std::optional<ores::http::domain::http_response> answer;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> {
            answer = co_await route->handler(req);
        },
        boost::asio::detached);
    io.run();
    REQUIRE(answer.has_value());
    return answer->status;
}

}

TEST_CASE("a_health_tally_refuses_a_party_that_is_not_an_id", "[report_health]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the health tally's party";

    http_request req;
    req.query_params["party_id"] = "not-an-id";
    CHECK(status_of(req) == http_status::bad_request);
}

TEST_CASE("a_health_tally_refuses_a_search_it_does_not_read", "[report_health]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the health tally's search";

    http_request too_long;
    too_long.query_params["search"] = std::string(257, 'x');
    CHECK(status_of(too_long) == http_status::bad_request);

    http_request unknown;
    unknown.query_params["health"] = "failed";
    CHECK(status_of(unknown) == http_status::bad_request);
}

TEST_CASE("a_health_tally_with_no_service_is_refused_as_upstream", "[report_health]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a health tally with no service behind it";

    CHECK(status_of(http_request{}) == http_status::bad_gateway);

    http_request named;
    named.query_params["party_id"] = "00000000-0000-0000-0000-0000000000e1";
    named.query_params["search"] = "close";
    CHECK(status_of(named) == http_status::bad_gateway);
}
