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
#include "ores.service/app/application.hpp"

#include <boost/throw_exception.hpp>
#include "ores.risk/messaging/registrar.hpp"
#include "ores.risk/domain/events/currency_changed_event.hpp"
#include "ores.accounts/messaging/registrar.hpp"
#include "ores.accounts/domain/events/account_changed_event.hpp"
#include "ores.variability/messaging/registrar.hpp"
#include "ores.variability/service/flag_initializer.hpp"
#include "ores.variability/service/system_flags_service.hpp"
#include "ores.accounts/service/bootstrap_mode_service.hpp"
#include "ores.eventing/service/event_bus.hpp"
#include "ores.eventing/service/postgres_event_source.hpp"
#include "ores.eventing/service/registrar.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.utility/database/context_factory.hpp"
#include "ores.comms/net/server.hpp"
#include "ores.service/app/application_exception.hpp"

namespace ores::service::app {
using namespace ores::utility::log;

utility::database::context application::make_context(
    const std::optional<utility::database::database_options>& db_opts) {
    using utility::database::context_factory;

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

    auto ctx = make_context(cfg.database);

    // Ensure all system flags exist in the database before any component queries them
    variability::service::flag_initializer flag_init(ctx);
    flag_init.ensure_system_flags_exist();

    // Create shared system flags service and refresh cache from database
    auto system_flags = std::make_shared<variability::service::system_flags_service>(ctx);
    system_flags->refresh();

    // Initialize and check bootstrap mode
    accounts::service::bootstrap_mode_service bootstrap_svc(ctx);
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
        risk::domain::events::currency_changed_event>(
        event_source, "ores.risk.currency", "ores_currencies");
    eventing::service::registrar::register_mapping<
        accounts::domain::events::account_changed_event>(
        event_source, "ores.accounts.account", "ores_accounts");

    // Start the event source to begin listening for database notifications
    event_source.start();

    auto srv = std::make_shared<ores::comms::net::server>(cfg.server);
    ores::risk::messaging::registrar::register_handlers(*srv, ctx, system_flags);
    ores::accounts::messaging::registrar::register_handlers(*srv, ctx, system_flags);
    ores::variability::messaging::registrar::register_handlers(*srv, ctx);

    co_await srv->run(io_ctx);

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
