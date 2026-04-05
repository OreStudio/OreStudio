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
#include "ores.controller.service/app/host.hpp"

#include <cstdlib>
#include <boost/exception/diagnostic_information.hpp>
#include "ores.telemetry/log/lifecycle_manager.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.controller.service/app/application.hpp"
#include "ores.controller.service/config/parser.hpp"

namespace ores::controller::service::app {

using namespace ores::logging;
using ores::controller::service::config::parser;
using ores::telemetry::log::lifecycle_manager;

boost::asio::awaitable<int>
host::execute(const std::vector<std::string>& args, std::ostream& std_output,
    std::ostream& error_output, boost::asio::io_context& io_ctx) {
    parser p;
    const auto ocfg(p.parse(args, std_output, error_output));

    if (!ocfg)
        co_return EXIT_SUCCESS;

    const auto& cfg(*ocfg);
    lifecycle_manager lm(cfg.logging);

    BOOST_LOG_SEV(lg(), info) << "Command line arguments: " << args;
    BOOST_LOG_SEV(lg(), debug) << "Configuration: " << cfg;

    try {
        application app;
        co_await app.run(io_ctx, cfg);
        co_return EXIT_SUCCESS;
    } catch (const std::exception& e) {
        const auto *const be(dynamic_cast<const boost::exception* const>(&e));
        if (be != nullptr) {
            using boost::diagnostic_information;
            BOOST_LOG_SEV(lg(), error) << "Error: " << diagnostic_information(*be);
        } else {
            BOOST_LOG_SEV(lg(), error) << "Error: " << e.what();
        }
        BOOST_LOG_SEV(lg(), error) << "Failed to execute command.";
        throw;
    }
}

}
