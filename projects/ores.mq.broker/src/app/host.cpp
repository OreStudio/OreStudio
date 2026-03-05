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
#include "ores.mq.broker/app/host.hpp"

#include <csignal>
#include <cstdlib>
#include "ores.logging/lifecycle_manager.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.utility/version/version.hpp"
#include "ores.mq/broker/broker_config.hpp"
#include "ores.mq/broker/nng_broker.hpp"
#include "ores.mq.broker/config/parser.hpp"
#include "ores.mq.broker/config/parser_exception.hpp"

namespace ores::mq::broker::app {

using namespace ores::logging;

int host::execute(const std::vector<std::string>& args,
    std::ostream& std_output, std::ostream& error_output) {

    config::parser p;
    const auto cfg_opt = p.parse(args, std_output, error_output);
    if (!cfg_opt)
        return EXIT_SUCCESS;

    const auto& cfg = *cfg_opt;

    // Initialise logging before anything else.
    lifecycle_manager lm(cfg.logging);

    BOOST_LOG_SEV(lg(), info)  << "Command line arguments: " << args;
    BOOST_LOG_SEV(lg(), debug) << "Configuration: " << cfg;
    BOOST_LOG_SEV(lg(), info)  << utility::version::format_startup_message(
        "ORE Studio MQ Broker");

    // Build broker_config from CLI options.
    ores::mq::broker::broker_config broker_cfg;
    broker_cfg.frontend_endpoint = cfg.frontend_endpoint;
    broker_cfg.backend_endpoint  = cfg.backend_endpoint;

    auto broker = std::make_shared<ores::mq::broker::nng_broker>(
        std::move(broker_cfg));

    // Install signal handler: SIGINT and SIGTERM both call broker->stop().
    // We use a static pointer since signal handlers must be plain functions.
    static std::weak_ptr<ores::mq::broker::nng_broker> g_broker;
    g_broker = broker;

    auto handler = [](int /*sig*/) {
        if (auto b = g_broker.lock())
            b->stop();
    };
    std::signal(SIGINT,  handler);
    std::signal(SIGTERM, handler);

    try {
        broker->run();  // blocks until stop() is called
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Broker terminated with error: " << e.what();
        return EXIT_FAILURE;
    }

    BOOST_LOG_SEV(lg(), info) << "Broker shut down cleanly";
    return EXIT_SUCCESS;
}

}
