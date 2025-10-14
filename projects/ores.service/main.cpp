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
#include <boost/cobalt/main.hpp>
#include "ores.comms/server.hpp"
#include "ores.utility/log/logger.hpp"
#include "ores.utility/log/scoped_lifecycle_manager.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.service"));
namespace cobalt = boost::cobalt;
const std::string force_terminate("Service was forced to terminate.");

}

cobalt::main co_main(int argc, char** argv) {
    using ores::utility::log::scoped_lifecycle_manager;

    scoped_lifecycle_manager slm;
    try {
        BOOST_LOG_SEV(lg, info) << "Starting ORES Service";

        // Configure server
        ores::comms::server_config config;
        config.port = 55555;
        config.max_connections = 10;
        config.certificate_file = "server.crt";
        config.private_key_file = "server.key";
        config.server_identifier = "ores-service-v1";

        // Create and run server
        ores::comms::server srv(config);
        co_await srv.run();

        BOOST_LOG_SEV(lg, info) << "ORES Service stopped normally";

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
