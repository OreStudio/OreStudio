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
#include <vector>
#include <openssl/crypto.h>
#include "ores.client/config/parser.hpp"
#include "ores.client/config/parser_exception.hpp"
#include "ores.client/app/application.hpp"
#include "ores.utility/log/scoped_lifecycle_manager.hpp"

namespace {

const std::string force_terminate("Application was forced to terminate.");

int run(int argc, char** argv) {
    using ores::client::config::parser;
    using ores::client::config::parser_exception;
    using ores::client::app::application;
    using ores::utility::log::scoped_lifecycle_manager;

    scoped_lifecycle_manager slm;
    try {
        const auto args(std::vector<std::string>(argv + 1, argv + argc));
        parser p;
        const auto o(p.parse(args, std::cout, std::cerr));

        if (!o)
            return EXIT_SUCCESS;

        slm.initialise(o->logging);

        application app;
        app.run();
        return EXIT_SUCCESS;
    } catch (const parser_exception& /*e*/) {
        /*
         * Reporting of these types of errors to the console has
         * already been handled by the parser itself.
         */
        return EXIT_FAILURE;
    } catch (const std::exception& e) {
        if (slm.is_initialised()) {
            using namespace ores::utility::log;
            auto lg(make_logger("main"));
            BOOST_LOG_SEV(lg, error) << "Unexpected error: " << e.what();
        } else {
            std::cerr << "Unexpected error: " << e.what() << std::endl;
        }
        return EXIT_FAILURE;
    } catch (...) {
        std::cerr << force_terminate << std::endl;
        return EXIT_FAILURE;
    }
}

}

int main(int argc, char** argv) {
    const int result = run(argc, argv);
    OPENSSL_cleanup();
    return result;
}
