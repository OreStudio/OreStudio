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
#include "ores.cli/app/host.hpp"
#include "ores.cli/config/parser_exception.hpp"
#include "ores.utility/log/scoped_lifecycle_manager.hpp"

namespace {

const std::string force_terminate("Application was forced to terminate.");

boost::asio::awaitable<int> async_main(int argc, char** argv) {
    using ores::cli::app::host;
    using ores::cli::config::parser_exception;
    using ores::utility::log::scoped_lifecycle_manager;

    scoped_lifecycle_manager slm;
    try {
        const auto args(std::vector<std::string>(argv + 1, argv + argc));
        co_return co_await host::execute(args, slm);
    } catch (const parser_exception& /*e*/) {
        /*
         * Reporting of these types of errors to the console has
         * already been handled by the parser itself.
         */
        co_return EXIT_FAILURE;
    } catch (const std::exception& e) {
        host::report_exception(slm.is_initialised(), e);
        co_return EXIT_FAILURE;
    } catch (...) {
        std::cerr << force_terminate << std::endl;
        co_return EXIT_FAILURE;
    }
}

}

int main(int argc, char** argv) {
    boost::asio::io_context io_ctx;

    int result = EXIT_FAILURE;
    boost::asio::co_spawn(
        io_ctx,
        [&]() -> boost::asio::awaitable<void> {
            result = co_await async_main(argc, argv);
        },
        boost::asio::detached);

    io_ctx.run();
    return result;
}
