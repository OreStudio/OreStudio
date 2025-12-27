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
#include <exception>
#include "ores.telemetry/log/lifecycle_manager.hpp"
#include "ores.comms.analyser/config/parser.hpp"
#include "ores.comms.analyser/app/application.hpp"

int main(int argc, const char* argv[]) {
    try {
        // Parse command-line arguments before initializing logging
        auto opts = ores::comms::analyser::config::parser::parse(argc, argv);
        if (!opts) {
            // --help or --version was requested, exit cleanly
            return 0;
        }

        // Initialize logging
        ores::telemetry::log::lifecycle_manager::initialise("ores.comms.analyser");

        // Run the application
        ores::comms::analyser::app::application app(std::move(*opts));
        return app.run();

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }
}
