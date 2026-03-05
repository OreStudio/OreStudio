/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.mq.broker/app/host.hpp"
#include "ores.mq.broker/config/parser_exception.hpp"

int main(int argc, char** argv) {
    using ores::mq::broker::app::host;
    using ores::mq::broker::config::parser_exception;

    try {
        const auto args(std::vector<std::string>(argv + 1, argv + argc));
        return host::execute(args, std::cout, std::cerr);
    } catch (const parser_exception& /*e*/) {
        // Reporting already done by the parser.
        return EXIT_FAILURE;
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return EXIT_FAILURE;
    } catch (...) {
        std::cerr << "Application was forced to terminate." << std::endl;
        return EXIT_FAILURE;
    }
}
