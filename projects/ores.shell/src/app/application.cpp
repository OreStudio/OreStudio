/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 */
#include "ores.shell/app/application.hpp"

#include "ores.shell/app/repl.hpp"

namespace ores::client::app {

using namespace ores::utility::log;

application::application(std::optional<comms::net::client_options> connection_config,
                         std::optional<config::login_options> login_config)
    : connection_config_(std::move(connection_config)),
      login_config_(std::move(login_config)) {
}

void application::run() const {
    BOOST_LOG_SEV(lg(), info) << "Starting client REPL";

    try {
        repl client_repl(connection_config_, login_config_);
        client_repl.run();
        BOOST_LOG_SEV(lg(), info) << "Client REPL session ended";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Client REPL error: " << e.what();
        throw;
    }
}

}
