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
#include <iostream>
#include <boost/asio/io_context.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/signal_set.hpp>
#include <openssl/crypto.h>
#include "ores.comms/server.hpp"
#include "ores.utility/log/logger.hpp"
#include "ores.utility/log/scoped_lifecycle_manager.hpp"
#include "ores.utility/repository/context_factory.hpp"
#include "ores.service/config/parser.hpp"
#include "ores.service/config/parser_exception.hpp"
#include "ores.risk/messaging/registration.hpp"
#include "ores.accounts/messaging/registration.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.service"));
const std::string force_terminate("Service was forced to terminate.");

boost::asio::awaitable<int> async_main(int argc, char** argv, boost::asio::io_context& io_ctx) {
    using ores::utility::log::scoped_lifecycle_manager;
    using ores::service::config::parser;
    using ores::service::config::parser_exception;

    scoped_lifecycle_manager slm;
    try {
        // Parse command line arguments
        const auto args(std::vector<std::string>(argv + 1, argv + argc));
        parser p;
        const auto ocfg(p.parse(args, std::cout, std::cerr));

        // If no configuration returned, exit (help or version was displayed)
        if (!ocfg.has_value())
            co_return EXIT_SUCCESS;

        const auto& cfg(*ocfg);

        // Initialize logging if configured
        if (cfg.logging.has_value())
            slm.initialise(cfg.logging.value());

        BOOST_LOG_SEV(lg, info) << "Starting ORES Service";
        BOOST_LOG_SEV(lg, debug) << "Configuration: " << cfg;

        // Configure server from parsed options
        ores::comms::server_config server_cfg;
        server_cfg.port = cfg.server.port;
        server_cfg.max_connections = cfg.server.max_connections;
        server_cfg.certificate_file = cfg.server.certificate_file;
        server_cfg.private_key_file = cfg.server.private_key_file;
        server_cfg.server_identifier = cfg.server.server_identifier;

        // Create database context
        // TODO: Add database configuration to service options
        using ores::utility::repository::context_factory;
        context_factory::configuration db_cfg{
            .user = "ores",
            .password = "ahV6aehuij6eingohsiajaiT0",
            .host = "localhost",
            .database = "oresdb",
            .port = 5434,
            .pool_size = 4,
            .num_attempts = 10,
            .wait_time_in_seconds = 1
        };
        auto ctx = context_factory::make_context(db_cfg);

        // Create server and register message handlers
        ores::comms::server srv(server_cfg);
        ores::risk::messaging::register_risk_handlers(srv, ctx);
        ores::accounts::messaging::register_accounts_handlers(srv, ctx);

        // Run server
        co_await srv.run(io_ctx);

        BOOST_LOG_SEV(lg, info) << "ORES Service stopped normally";

    } catch (const parser_exception& /*e*/) {
        // Parser has already reported errors to console
        co_return EXIT_FAILURE;
    } catch (const std::exception& e) {
        if (slm.is_initialised())
            BOOST_LOG_SEV(lg, error) << "Server error: " << e.what();
        else
            std::cerr << "Server error: " << e.what() << std::endl;
        co_return EXIT_FAILURE;
    } catch (...) {
        std::cerr << force_terminate << std::endl;
        co_return EXIT_FAILURE;
    }

    co_return EXIT_SUCCESS;
}

}

int main(int argc, char** argv) {
    boost::asio::io_context io_ctx;

    // Setup signal handling for graceful shutdown
    boost::asio::signal_set signals(io_ctx, SIGINT, SIGTERM);
    signals.async_wait([&](auto, auto) {
        BOOST_LOG_SEV(lg, info) << "Shutdown signal received, stopping server...";
        io_ctx.stop();
    });

    int result = EXIT_FAILURE;
    boost::asio::co_spawn(
        io_ctx,
        [&]() -> boost::asio::awaitable<void> {
            result = co_await async_main(argc, argv, io_ctx);
        },
        boost::asio::detached);

    io_ctx.run();
    OPENSSL_cleanup();
    return result;
}
