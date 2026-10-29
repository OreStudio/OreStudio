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
#ifndef ORES_SHELL_APP_COMMANDS_ADAPTER_STATUS_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_ADAPTER_STATUS_COMMANDS_HPP

#include "ores.nats/service/nats_client.hpp"
#include <ostream>
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief The one status of the market data path: demand and ingest together.
 *
 * For each adapter it prints the routes that name its source, the ids the
 * service demanded of it and the ones it accepted, the ticks the ingest
 * received in the last minute and the age of the last tick. An adapter that
 * is up with routes and no demand is the shape of a silent feed, so the
 * command is the first place to look when no tick arrives.
 *
 *   marketdata adapter-status show
 */
class adapter_status_commands {
public:
    /// Register the =adapters= submenu.
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /// =status= — read the service's adapter status and print one row each.
    static void process_status(std::ostream& out,
                               ores::nats::service::nats_client& session,
                               const std::vector<std::string>& args);
};

}

#endif
