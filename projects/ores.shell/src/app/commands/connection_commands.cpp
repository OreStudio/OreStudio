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
#include "ores.shell/app/commands/connection_commands.hpp"

#include <ostream>
#include <functional>
#include <cli/cli.h>

namespace ores::shell::app::commands::connection {

using namespace ores::utility::log;

void connection_commands::
register_commands(cli::Menu& root_menu, client_manager& client_manager) {
    root_menu.Insert("connect", [&client_manager](std::ostream & out,
            std::string host, std::string port, std::string identifier) {
        process_connect(std::ref(out), std::ref(client_manager),
            std::move(host),
                std::move(port), std::move(identifier));
        }, "Connect to server (optional: host port identifier)");

    root_menu.Insert("disconnect", [&client_manager](std::ostream& out) {
        process_disconnect(std::ref(out), std::ref(client_manager));
    }, "Disconnect from server");
}

void connection_commands::
process_connect(std::ostream& /*out*/, client_manager& client_manager,
    std::string host, std::string port, std::string identifier) {
    client_manager.connect(host, port, identifier);
}

void connection_commands::
process_disconnect(std::ostream& /*out*/, client_manager& client_manager) {
    client_manager.disconnect();
}

}
