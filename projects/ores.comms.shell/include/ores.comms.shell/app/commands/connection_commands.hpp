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
#ifndef ORES_COMMS_SHELL_APP_COMMANDS_CONNECTION_COMMANDS_HPP
#define ORES_COMMS_SHELL_APP_COMMANDS_CONNECTION_COMMANDS_HPP

#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.comms/net/client_session.hpp"

namespace cli {

class Menu;

}

namespace ores::comms::shell::app::commands {

/**
 * @brief Manages commands related to connections.
 */
class connection_commands {
private:
    inline static std::string_view logger_name =
        "ores.comms.shell.app.commands.connection";

    auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register connection management commands.
     *
     * Adds connect and disconnect commands to the root menu.
     *
     * @param root The root menu to add commands to
     * @param session Client session for connectivity.
     */
    static void register_commands(cli::Menu& root,
        comms::net::client_session& session);

    /**
     * @brief Process a connection request.
     *
     * Handles the async connection workflow including configuration updates
     * and existing connection cleanup.
     *
     * @param out Output stream for user feedback.
     * @param session Client session for connectivity.
     * @param host New host (empty to keep current).
     * @param port New port (empty to keep current).
     * @param identifier New client identifier (empty to keep current).
     */
    static void process_connect(std::ostream& out,
        comms::net::client_session& session,
        std::string host, std::string port, std::string identifier);

    /**
     * @brief Process a disconnect request.
     *
     * Cleanly disconnects from the server if connected.
     *
     * @param out Output stream for user feedback.
     * @param session Client session for connectivity.
     */
    static void process_disconnect(std::ostream& out,
        comms::net::client_session& session);
};

}

#endif
