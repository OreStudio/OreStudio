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
#include <boost/throw_exception.hpp>
#include "ores.risk/messaging/registrar.hpp"
#include "ores.accounts/messaging/registrar.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.utility/repository/context_factory.hpp"
#include "ores.comms/server.hpp"
#include "ores.service/app/application.hpp"
#include "ores.service/app/application_exception.hpp"

namespace ores::service::app {
using namespace ores::utility::log;

utility::repository::context application::make_context(
    const std::optional<config::database_options>& db_opts) {
    using utility::repository::context_factory;

    if (!db_opts.has_value()) {
        BOOST_THROW_EXCEPTION(
            application_exception("Database configuration is required."));
    }

    const auto& db(db_opts.value());
    context_factory::configuration cfg {
        .user = db.user,
        .password = db.password,
        .host = db.host,
        .database = db.database,
        .port = db.port,
        .pool_size = 4,
        .num_attempts = 10,
        .wait_time_in_seconds = 1
    };

    return context_factory::make_context(cfg);
}

application::application() = default;

boost::asio::awaitable<void> application::
run(boost::asio::io_context& io_ctx, const config::options& cfg) const {
    BOOST_LOG_SEV(lg(), info) << "Starting ORE Studio Service v"
                              << ORES_VERSION;

    // Configure server from parsed options
    ores::comms::server_config server_cfg;
    server_cfg.port = cfg.server.port;
    server_cfg.max_connections = cfg.server.max_connections;
    server_cfg.certificate_file = cfg.server.certificate_file;
    server_cfg.private_key_file = cfg.server.private_key_file;
    server_cfg.server_identifier = cfg.server.server_identifier;

    // Create database context from configuration
    auto ctx = make_context(cfg.database);

    // Create server and register message handlers
    ores::comms::server srv(server_cfg);
    ores::risk::messaging::registrar::register_handlers(srv, ctx);
    ores::accounts::messaging::registrar::register_handlers(srv, ctx);

    // Run server
    co_await srv.run(io_ctx);

    BOOST_LOG_SEV(lg(), info) << "ORES Service stopped normally";
}

}
