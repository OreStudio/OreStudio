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
#include "ores.service/app/host.hpp"
#include "ores.service/config/parser_exception.hpp"

namespace {

boost::asio::awaitable<int>
async_main(int argc, char** argv, boost::asio::io_context& io_ctx) {
    using ores::service::app::host;
    using ores::service::config::parser_exception;

    try {
        const auto args(std::vector<std::string>(argv + 1, argv + argc));
        co_return co_await host::execute(args, std::cout, std::cerr, io_ctx);
    } catch (const parser_exception& /*e*/) {
        /*
         * Reporting of these types of errors to the console has
         * already been handled by the parser itself.
         */
        co_return EXIT_FAILURE;
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        std::cerr << "Failed to execute command." << std::endl;
        co_return EXIT_FAILURE;
    } catch (...) {
        std::cerr << "Application was forced to terminate." << std::endl;
        co_return EXIT_FAILURE;
    }
}

}

int main(int argc, char** argv) {
    boost::asio::io_context io_ctx;

    // Install signal handlers to allow graceful shutdown
    // This prevents the default SIGINT/SIGTERM handlers from terminating
    // the process immediately, giving the server time to clean up
    boost::asio::signal_set signals(io_ctx, SIGINT, SIGTERM);
    signals.async_wait([&io_ctx](const boost::system::error_code&, int signal) {
        std::cout << "\nReceived signal " << signal << ", initiating shutdown..." << std::endl;
        // Don't stop io_context here - let the server's signal handler do the cleanup
        // The server will stop the io_context when it's done
    });

    int result = EXIT_FAILURE;
    boost::asio::co_spawn(io_ctx, [&]() -> boost::asio::awaitable<void> {
            result = co_await async_main(argc, argv, io_ctx);
            // Stop io_context when application completes (success or error)
            io_ctx.stop();
        }, boost::asio::detached);

    io_ctx.run();

    OPENSSL_cleanup();
    return result;
}
