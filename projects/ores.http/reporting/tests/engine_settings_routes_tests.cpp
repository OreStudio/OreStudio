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
