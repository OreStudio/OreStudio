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
#include "ores.database/domain/context.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.variability/service/system_flags_service.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.http.server/routes/iam_routes.hpp"
#include "ores.http.server/routes/risk_routes.hpp"
#include "ores.http.server/routes/variability_routes.hpp"
#include "ores.http.server/routes/assets_routes.hpp"

namespace ores::http_server::app {

using namespace ores::telemetry::log;
namespace asio = boost::asio;

boost::asio::awaitable<void> application::run(asio::io_context& io_ctx,
    const config::options& cfg) {

    BOOST_LOG_SEV(lg(), info) << "Starting HTTP server application...";
    BOOST_LOG_SEV(lg(), debug) << "Configuration: " << cfg;

    // Initialize database context
    BOOST_LOG_SEV(lg(), info) << "Initializing database connection...";
    database::service::context_factory factory;
    auto ctx = factory.create(cfg.database);

    // Initialize shared services
    BOOST_LOG_SEV(lg(), info) << "Initializing shared services...";
    auto system_flags = std::make_shared<variability::service::system_flags_service>(ctx);
    auto sessions = std::make_shared<comms::service::auth_session_service>();
    auto auth_service = std::make_shared<iam::service::authorization_service>(ctx);

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

    // Register API info endpoint
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

    // Register IAM routes (accounts, auth, roles, sessions)
    routes::iam_routes iam(ctx, system_flags, sessions, auth_service);
    iam.register_routes(router, registry);

    // Register Risk routes (currencies)
    routes::risk_routes risk(ctx, sessions);
    risk.register_routes(router, registry);

    // Register Variability routes (feature flags)
    routes::variability_routes variability(system_flags, sessions);
    variability.register_routes(router, registry);

    // Register Assets routes (images)
    routes::assets_routes assets(ctx, sessions);
    assets.register_routes(router, registry);

    BOOST_LOG_SEV(lg(), info) << "API routes registered, starting server...";
    BOOST_LOG_SEV(lg(), info) << "Total endpoints: " << router->routes().size();
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
