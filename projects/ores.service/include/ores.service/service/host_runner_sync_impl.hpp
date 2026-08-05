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
#ifndef ORES_SERVICE_SERVICE_HOST_RUNNER_SYNC_IMPL_HPP
#define ORES_SERVICE_SERVICE_HOST_RUNNER_SYNC_IMPL_HPP

#include "ores.telemetry.core/log/lifecycle_manager.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/exception/diagnostic_information.hpp>
#include <cstdlib>

namespace ores::service::service {

template <typename Parser, typename EarlyExit, typename Runner>
int run_host_sync(const std::vector<std::string>& args,
                  std::ostream& std_output,
                  std::ostream& error_output,
                  ores::logging::logger_t& lg,
                  EarlyExit early_exit,
                  Runner run_application,
                  host_runner_sync_options opts) {
    using namespace ores::logging;
    using ores::telemetry::log::lifecycle_manager;

    Parser p;
    const auto ocfg(p.parse(args, std_output, error_output));

    if (!ocfg)
        return EXIT_SUCCESS;

    const auto& cfg(*ocfg);
    lifecycle_manager lm(cfg.logging);

    if (opts.on_before_log)
        opts.on_before_log();

    BOOST_LOG_SEV(lg, info) << "Command line arguments: " << args;
    BOOST_LOG_SEV(lg, debug) << "Configuration: " << cfg;

    if (const auto exit_code = early_exit(cfg))
        return *exit_code;

    try {
        run_application(cfg);

        if (opts.on_success)
            opts.on_success();

        return EXIT_SUCCESS;
    } catch (const std::exception& e) {
        const auto* const be(dynamic_cast<const boost::exception* const>(&e));
        if (be != nullptr) {
            using boost::diagnostic_information;
            BOOST_LOG_SEV(lg, error) << "Error: " << diagnostic_information(*be);
        } else {
            BOOST_LOG_SEV(lg, error) << "Error: " << e.what();
        }
        BOOST_LOG_SEV(lg, error) << opts.failure_message;
        throw;
    }
}

}

#endif
