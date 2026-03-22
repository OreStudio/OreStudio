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
#include <boost/uuid/string_generator.hpp>
#include "ores.http/net/http_server.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.database/service/service_accounts.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.variability/service/system_settings_service.hpp"
#include "ores.iam.core/service/auth_session_service.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.iam.core/repository/session_repository.hpp"
#include "ores.http.server/routes/iam_routes.hpp"
#include "ores.http.server/routes/risk_routes.hpp"
#include "ores.http.server/routes/variability_routes.hpp"
#include "ores.http.server/routes/assets_routes.hpp"
#include "ores.http.server/routes/compute_routes.hpp"
#include "ores.http.server/messaging/registrar.hpp"
#include "ores.geo/service/geolocation_service.hpp"
#include "ores.eventing/service/event_bus.hpp"
#include "ores.eventing/service/postgres_event_source.hpp"
#include "ores.eventing/service/registrar.hpp"
#include "ores.variability/eventing/system_setting_changed_event.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.utility/version/version.hpp"

namespace ores::http_server::app {

using namespace ores::logging;
namespace asio = boost::asio;

boost::asio::awaitable<void> application::run(asio::io_context& io_ctx,
    const config::options& cfg) {

    BOOST_LOG_SEV(lg(), info) << utility::version::format_startup_message(
        "ORE Studio HTTP Server");
    BOOST_LOG_SEV(lg(), debug) << "Configuration: " << cfg;

    // Determine the HTTP base URL to advertise via service discovery
    const std::string http_base_url = cfg.http_base_url.empty()
        ? "http://localhost:" + std::to_string(cfg.server.port)
        : cfg.http_base_url;

    // Connect to NATS for service discovery
    BOOST_LOG_SEV(lg(), info) << "Connecting to NATS: " << cfg.nats.url;
    nats::service::client nats(cfg.nats);
    nats.connect();
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS, registering handlers...";
    auto nats_subs = http_server::messaging::registrar::register_handlers(
        nats, http_base_url);

    // Initialize database context
    BOOST_LOG_SEV(lg(), info) << "Initializing database connection...";
    database::context_factory::configuration db_cfg {
        .database_options = cfg.database,
        .pool_size = 4,
        .num_attempts = 10,
        .wait_time_in_seconds = 1,
        .service_account = std::string(ores::database::service::service_accounts::http)
    };
    auto ctx = database::context_factory::make_context(db_cfg);

    // Initialize shared services
    BOOST_LOG_SEV(lg(), info) << "Initializing shared services...";
    auto system_flags = std::make_shared<variability::service::system_settings_service>(
        ctx, database::service::tenant_context::system_tenant_id);
    system_flags->refresh();
    auto sessions = std::make_shared<iam::service::auth_session_service>();
    auto auth_service = std::make_shared<iam::service::authorization_service>(ctx);
    auto geo_service = std::make_shared<geo::service::geolocation_service>(ctx);

    // Initialize event bus for cross-service cache invalidation
    BOOST_LOG_SEV(lg(), info) << "Initializing event bus...";
    eventing::service::event_bus event_bus;
    eventing::service::postgres_event_source event_source(ctx, event_bus);

    // Register system settings mapping for system settings cache invalidation
    eventing::service::registrar::register_mapping<
        variability::eventing::system_setting_changed_event>(
        event_source, "ores.variability.system_setting", "ores_system_settings");

    // Subscribe to system settings changes to refresh system settings cache
    auto flags_sub = event_bus.subscribe<variability::eventing::system_setting_changed_event>(
        [&system_flags](const variability::eventing::system_setting_changed_event& e) {
            BOOST_LOG_SEV(lg(), info) << "System settings changed notification received, "
                                      << "refreshing settings cache ("
                                      << e.setting_names.size() << " settings changed)";
            system_flags->refresh();
        });

    // Start the event source to begin listening for database notifications
    event_source.start();

    // Create HTTP server
    http::net::http_server server(io_ctx, cfg.server);

    // Set up session bytes tracking callback
    auto session_repo = std::make_shared<iam::repository::session_repository>(ctx);
    server.set_session_bytes_callback(
        [session_repo](const std::string& session_id_str,
            std::chrono::system_clock::time_point start_time,
            std::size_t bytes_sent,
            std::size_t bytes_received) {
            try {
                boost::uuids::string_generator gen;
                auto session_id = gen(session_id_str);
                session_repo->update_bytes(session_id, start_time,
                    bytes_sent, bytes_received);
            } catch (const std::exception& e) {
                // Log but don't throw - bytes tracking is not critical
                BOOST_LOG_SEV(lg(), warn) << "Failed to update session bytes: "
                                          << e.what();
            }
        });

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

    // Register IAM routes (accounts, auth, roles, sessions)
    routes::iam_routes iam(ctx, system_flags, sessions, auth_service,
        server.get_authenticator(), geo_service);
    iam.register_routes(router, registry);

    // Register Risk routes (currencies)
    routes::risk_routes risk(ctx, sessions);
    risk.register_routes(router, registry);

    // Register Variability routes (system settings)
    routes::variability_routes variability(ctx, system_flags, sessions);
    variability.register_routes(router, registry);

    // Register Assets routes (images)
    routes::assets_routes assets(ctx, sessions);
    assets.register_routes(router, registry);

    // Register Compute routes (packages, inputs, outputs)
    routes::compute_routes compute(cfg.compute_storage_dir);
    compute.register_routes(router, registry);

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

    BOOST_LOG_SEV(lg(), info) << "HTTP base URL advertised via NATS: " << http_base_url;

    // Run the server
    co_await server.run();

    // Stop the event source
    event_source.stop();

    BOOST_LOG_SEV(lg(), info) << "HTTP server application stopped";
}

}
