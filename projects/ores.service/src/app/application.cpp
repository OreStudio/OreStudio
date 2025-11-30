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
#include "ores.accounts/messaging/registrar.hpp"
#include "ores.accounts/service/bootstrap_mode_service.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.utility/repository/context_factory.hpp"
#include "ores.comms/net/server.hpp"
#include "ores.service/app/application_exception.hpp"

namespace ores::service::app {
using namespace ores::utility::log;

utility::repository::context application::make_context(
    const std::optional<utility::database::database_options>& db_opts) {
    using utility::repository::context_factory;

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

    // Initialize and check bootstrap mode
    accounts::service::bootstrap_mode_service bootstrap_svc(ctx);
    bootstrap_svc.initialize_bootstrap_state();

    const bool in_bootstrap_mode = bootstrap_svc.is_in_bootstrap_mode();
    ctx.set_bootstrap_mode(in_bootstrap_mode);

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

    auto srv = std::make_shared<ores::comms::net::server>(cfg.server);
    ores::risk::messaging::registrar::register_handlers(*srv, ctx);
    ores::accounts::messaging::registrar::register_handlers(*srv, ctx);

    co_await srv->run(io_ctx);

    // Shutdown logging
    if (bootstrap_svc.is_in_bootstrap_mode()) {
        BOOST_LOG_SEV(lg(), warn) << "================================================";
        BOOST_LOG_SEV(lg(), warn) << "ORES Service stopped - STILL IN BOOTSTRAP MODE";
        BOOST_LOG_SEV(lg(), warn) << "Initial admin account was NOT created";
        BOOST_LOG_SEV(lg(), warn) << "================================================";
    } else {
        BOOST_LOG_SEV(lg(), info) << "ORES Service stopped normally (SECURE MODE).";
    }
}

}
