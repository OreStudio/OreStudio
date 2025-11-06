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
#include <cstdlib>
#include <boost/exception/diagnostic_information.hpp>
#include "ores.utility/log/lifecycle_manager.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.client/app/application.hpp"
#include "ores.client/config/parser.hpp"
#include "ores.client/app/host.hpp"

namespace ores::client::app {

using namespace ores::utility::log;
using ores::client::config::parser;
using ores::utility::log::lifecycle_manager;

int host::execute(const std::vector<std::string>& args,
    std::ostream& std_output, std::ostream& error_output) {
    /*
     * Create the configuration from command line options.
     */
    parser p;
    const auto ocfg(p.parse(args, std_output, error_output));

    /*
     * If we have no configuration, then there is nothing to do. This can only
     * happen if the user requested some valid options such as help or version;
     * any errors at the command line level are treated as exceptions, and all
     * other cases must result in a configuration object.
     */
    if (!ocfg)
        return EXIT_SUCCESS;

    /*
     * Since we have a configuration, we can now attempt to initialise the
     * logging subsystem.
     */
    const auto& cfg(*ocfg);
    lifecycle_manager lm(cfg.logging);

    /*
     * Log the configuration and command line arguments.
     */
    BOOST_LOG_SEV(lg(), info) << "Command line arguments: " << args;
    BOOST_LOG_SEV(lg(), debug) << "Configuration: " << cfg;

    /*
     * Execute the application.
     */
    try {
        ores::client::app::application app;
        app.run();
        return EXIT_SUCCESS;
    } catch (const std::exception& e) {
        /*
         * Try to dump useful, but less user-friendly information by
         * interrogating the exception. Note that we must catch by
         * std::exception and cast the boost exception; if we were to catch
         * boost exception, we would not have access to the what() method and
         * thus could not provide the exception message to the console.
         */
        const auto *const be(dynamic_cast<const boost::exception* const>(&e));
        if (be == nullptr)
            throw;

        using boost::diagnostic_information;
        BOOST_LOG_SEV(lg(), error) << "Error: " << diagnostic_information(*be);
        BOOST_LOG_SEV(lg(), error) << "Failed to execute command.";
        throw;
    }
}

}
