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
#include "ores.http.server/app/application.hpp"

#include <boost/asio/signal_set.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include "ores.http/net/http_server.hpp"

namespace ores::http_server::app {

using namespace ores::telemetry::log;
namespace asio = boost::asio;

boost::asio::awaitable<void> application::run(asio::io_context& io_ctx,
    const config::options& cfg) {

    BOOST_LOG_SEV(lg(), info) << "Starting HTTP server application...";
    BOOST_LOG_SEV(lg(), debug) << "Configuration: " << cfg;

    // Create HTTP server
    http::net::http_server server(io_ctx, cfg.server);

    // Get router for adding custom routes
    auto router = server.get_router();
    auto registry = server.get_registry();

    // Set API info for OpenAPI
    http::openapi::api_info api_info;
    api_info.title = "OreStudio REST API";
    api_info.description = "RESTful API for OreStudio risk management platform";
    api_info.version = "1.0.0";
    api_info.contact_name = "Marco Craveiro";
    api_info.contact_email = "marco.craveiro@gmail.com";
    api_info.license_name = "GPL-3.0";
    registry->set_info(api_info);

    BOOST_LOG_SEV(lg(), info) << "Registering API routes...";

    // Example: Add a simple API route
    auto api_info_builder = router->get("/api/v1/info")
        .summary("API Information")
        .description("Returns information about the API")
        .tags({"info"})
        .handler([](const http::domain::http_request&) -> asio::awaitable<http::domain::http_response> {
            co_return http::domain::http_response::json(
                R"({"name":"OreStudio API","version":"1.0.0","status":"running"})");
        });
    router->add_route(api_info_builder.build());
    registry->register_route(api_info_builder.build());

    // Register routes with OpenAPI registry
    for (const auto& route : router->routes()) {
        registry->register_route(route);
    }

    BOOST_LOG_SEV(lg(), info) << "API routes registered, starting server...";
    BOOST_LOG_SEV(lg(), info) << "OpenAPI spec available at /openapi.json";
    BOOST_LOG_SEV(lg(), info) << "Swagger UI available at /swagger";

    // Setup signal handling
    asio::signal_set signals(io_ctx, SIGINT, SIGTERM);
    signals.async_wait([&](auto, auto) {
        BOOST_LOG_SEV(lg(), info) << "Shutdown signal received";
        server.stop();
    });

    // Run the server
    co_await server.run();

    BOOST_LOG_SEV(lg(), info) << "HTTP server application stopped";
}

}
