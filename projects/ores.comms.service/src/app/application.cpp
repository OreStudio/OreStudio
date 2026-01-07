/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 */
#include "ores.comms.service/app/application.hpp"

#include <boost/throw_exception.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include "ores.risk/messaging/registrar.hpp"
#include "ores.risk/eventing/currency_changed_event.hpp"
#include "ores.iam/messaging/registrar.hpp"
#include "ores.iam/eventing/account_changed_event.hpp"
#include "ores.assets/eventing/assets_changed_event.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.variability/messaging/registrar.hpp"
#include "ores.assets/messaging/registrar.hpp"
#include "ores.telemetry/messaging/registrar.hpp"
#include "ores.variability/service/system_flags_service.hpp"
#include "ores.iam/service/bootstrap_mode_service.hpp"
#include "ores.eventing/service/event_bus.hpp"
#include "ores.eventing/service/postgres_event_source.hpp"
#include "ores.eventing/service/registrar.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.database/service/health_monitor.hpp"
#include "ores.comms/net/server.hpp"
#include "ores.comms/service/subscription_manager.hpp"
#include "ores.comms/service/subscription_handler.hpp"
#include "ores.geo/service/geolocation_service.hpp"
#include "ores.comms.service/app/application_exception.hpp"

namespace ores::comms::service::app {
using namespace ores::telemetry::log;

database::context application::make_context(
    const std::optional<database::database_options>& db_opts) {
    using database::context_factory;

    if (!db_opts.has_value()) {
        BOOST_THROW_EXCEPTION(
            application_exception("Database configuration is required."));
    }

    context_factory::configuration cfg {
        .database_options = db_opts.value(),
        .pool_size = 4,
        .num_attempts = 10,
        .wait_time_in_seconds = 1
    };

    return context_factory::make_context(cfg);
}

application::application() = default;

boost::asio::awaitable<void> application::
run(boost::asio::io_context& io_ctx, const config::options& cfg) const {
    BOOST_LOG_SEV(lg(), info) << "Starting ORE Studio Service v" << ORES_VERSION;

    // Create database health monitor
    database::health_monitor db_health_monitor(
        cfg.database, std::chrono::seconds(5));

    // Perform initial database connectivity check
    BOOST_LOG_SEV(lg(), info) << "Checking database connectivity...";
    bool db_initially_available = db_health_monitor.check_health();

    if (!db_initially_available) {
        BOOST_LOG_SEV(lg(), warn) << "================================================";
        BOOST_LOG_SEV(lg(), warn) << "DATABASE CONNECTIVITY CHECK FAILED";
        BOOST_LOG_SEV(lg(), warn) << "Error: " << db_health_monitor.last_error();
        BOOST_LOG_SEV(lg(), warn) << "Waiting for database to become available...";
        BOOST_LOG_SEV(lg(), warn) << "================================================";

        // Wait for database to become available before proceeding
        while (!db_health_monitor.check_health()) {
            BOOST_LOG_SEV(lg(), info) << "Database still unavailable, retrying in 5 seconds...";
            boost::asio::steady_timer timer(co_await boost::asio::this_coro::executor);
            timer.expires_after(std::chrono::seconds(5));
            co_await timer.async_wait(boost::asio::use_awaitable);
        }

        BOOST_LOG_SEV(lg(), info) << "Database connectivity restored!";
    } else {
        BOOST_LOG_SEV(lg(), info) << "Database connectivity check PASSED";
    }

    auto ctx = make_context(cfg.database);

    // Create shared authorization service for RBAC checks
    // (Permissions and roles are seeded via SQL scripts in the database template)
    auto auth_service = std::make_shared<iam::service::authorization_service>(ctx);

    // Create shared system flags service and refresh cache from database
    // (System flags are seeded via SQL scripts in the database template)
    auto system_flags = std::make_shared<variability::service::system_flags_service>(ctx);
    system_flags->refresh();

    // Initialize and check bootstrap mode
    iam::service::bootstrap_mode_service bootstrap_svc(ctx, auth_service);
    bootstrap_svc.initialize_bootstrap_state();

    // Refresh system flags cache after bootstrap state initialization
    system_flags->refresh();

    const bool in_bootstrap_mode = system_flags->is_bootstrap_mode_enabled();

    if (in_bootstrap_mode) {
        BOOST_LOG_SEV(lg(), warn) << "================================================";
        BOOST_LOG_SEV(lg(), warn) << "!!!  SYSTEM IN BOOTSTRAP MODE  !!!";
        BOOST_LOG_SEV(lg(), warn) << "================================================";
        BOOST_LOG_SEV(lg(), warn) << "Security Status: INSECURE - No admin account exists";
        BOOST_LOG_SEV(lg(), warn) << "Available Endpoints: create-initial-admin ONLY";
        BOOST_LOG_SEV(lg(), warn) << "Access Restriction: localhost (127.0.0.1) only";
        BOOST_LOG_SEV(lg(), warn) << "Action Required: Create initial admin account";
        BOOST_LOG_SEV(lg(), warn) << "================================================";
    } else {
        BOOST_LOG_SEV(lg(), info) << "System in SECURE MODE";
        BOOST_LOG_SEV(lg(), info) << "Authentication and authorization enforcement enabled";
    }

    // Initialize the event bus and event sources
    eventing::service::event_bus event_bus;
    eventing::service::postgres_event_source event_source(ctx, event_bus);

    // Register entity-to-event mappings for each component
    eventing::service::registrar::register_mapping<
        risk::eventing::currency_changed_event>(
        event_source, "ores.risk.currency", "ores_currencies");
    eventing::service::registrar::register_mapping<
        iam::eventing::account_changed_event>(
        event_source, "ores.iam.account", "ores_accounts");
    eventing::service::registrar::register_mapping<
        assets::eventing::assets_changed_event>(
        event_source, "ores.assets.currency_image", "ores_currency_images");

    // Start the event source to begin listening for database notifications
    event_source.start();

    // Create subscription manager for client notifications
    auto subscription_mgr = std::make_shared<comms::service::subscription_manager>();

    // Bridge event bus to subscription manager - when domain events occur,
    // notify all clients subscribed to those event types
    auto currency_sub = event_bus.subscribe<risk::eventing::currency_changed_event>(
        [&subscription_mgr](const risk::eventing::currency_changed_event& e) {
            using traits = eventing::domain::event_traits<
                risk::eventing::currency_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.iso_codes);
        });

    auto account_sub = event_bus.subscribe<iam::eventing::account_changed_event>(
        [&subscription_mgr](const iam::eventing::account_changed_event& e) {
            using traits = eventing::domain::event_traits<
                iam::eventing::account_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.account_ids);
        });

    auto assets_sub = event_bus.subscribe<assets::eventing::assets_changed_event>(
        [&subscription_mgr](const assets::eventing::assets_changed_event& e) {
            using traits = eventing::domain::event_traits<
                assets::eventing::assets_changed_event>;
            subscription_mgr->notify(std::string{traits::name}, e.timestamp,
                                     e.iso_codes);
        });

    // Create server with subscription manager
    auto srv = std::make_shared<ores::comms::net::server>(cfg.server, subscription_mgr);

    // Create geolocation service using PostgreSQL geoip tables
    auto geo_service = std::make_shared<geo::service::geolocation_service>(ctx);

    // Register subsystem handlers
    ores::risk::messaging::registrar::register_handlers(*srv, ctx, system_flags);
    ores::iam::messaging::registrar::register_handlers(*srv, ctx, system_flags, auth_service,
        geo_service);
    ores::variability::messaging::registrar::register_handlers(*srv, ctx);
    ores::assets::messaging::registrar::register_handlers(*srv, ctx);
    ores::telemetry::messaging::registrar::register_handlers(*srv, ctx, srv->sessions());

    // Register subscription handler for subscribe/unsubscribe messages
    auto subscription_handler =
        std::make_shared<comms::service::subscription_handler>(subscription_mgr);
    srv->register_handler(
        {comms::messaging::CORE_SUBSYSTEM_MIN, comms::messaging::CORE_SUBSYSTEM_MAX},
        subscription_handler);

    // Set up database health monitor callback to broadcast status changes to clients
    db_health_monitor.set_status_change_callback(
        [&srv](bool available, const std::string& error_message) {
            BOOST_LOG_SEV(lg(), info)
                << "Database status changed: "
                << (available ? "AVAILABLE" : "UNAVAILABLE");
            srv->broadcast_database_status(available, error_message);
        });

    // Start the database health monitor to continuously poll
    boost::asio::co_spawn(io_ctx,
        [&db_health_monitor, &io_ctx]() -> boost::asio::awaitable<void> {
            co_await db_health_monitor.run(io_ctx);
        },
        boost::asio::detached);

    co_await srv->run(io_ctx);

    // Stop the database health monitor
    db_health_monitor.stop();

    // Stop the event source
    event_source.stop();

    // Shutdown logging
    if (system_flags->is_bootstrap_mode_enabled()) {
        BOOST_LOG_SEV(lg(), warn) << "================================================";
        BOOST_LOG_SEV(lg(), warn) << "ORES Service stopped - STILL IN BOOTSTRAP MODE";
        BOOST_LOG_SEV(lg(), warn) << "Initial admin account was NOT created";
        BOOST_LOG_SEV(lg(), warn) << "================================================";
    } else {
        BOOST_LOG_SEV(lg(), info) << "ORES Service stopped normally (SECURE MODE).";
    }
}

}
