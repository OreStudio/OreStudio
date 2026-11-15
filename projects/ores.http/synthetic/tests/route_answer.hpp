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
#ifndef ORES_HTTP_SYNTHETIC_TESTS_ROUTE_ANSWER_HPP
#define ORES_HTTP_SYNTHETIC_TESTS_ROUTE_ANSWER_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <optional>
#include <string>

namespace ores::http::synthetic::tests {

/**
 * @brief The answer a unit's route gives a request at an address, matched
 * through the router as a browser reaches it, on a client that is not
 * connected, so any forward fails.
 *
 * The address must reach the expected pattern, and the route must require a
 * session.
 */
template <typename Routes>
ores::http::domain::http_response answer_at(ores::http::domain::http_method method,
                                            const std::string& path,
                                            ores::http::domain::http_request req,
                                            const std::string& expected_pattern) {
    ores::nats::service::nats_client session;
    auto table = std::make_shared<ores::http::net::router>();
    Routes::register_routes(
        table, std::make_shared<ores::http::openapi::endpoint_registry>(), session);
    const auto route = table->match(method, path, req.path_params);
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
    return *answer;
}

}

#endif
