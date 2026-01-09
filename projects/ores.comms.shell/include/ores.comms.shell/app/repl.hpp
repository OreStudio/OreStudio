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
#ifndef ORES_COMMS_SHELL_APP_REPL_HPP
#define ORES_COMMS_SHELL_APP_REPL_HPP

#include <memory>
#include "ores.logging/make_logger.hpp"
#include "ores.comms/net/client_session.hpp"

namespace cli {

class Cli;
class Menu;

}

namespace ores::comms::shell::app {

/**
 * @brief Interactive REPL (Read-Eval-Print Loop) for the ORE Studio client.
 *
 * Provides a command-line interface for interacting with the ORE Studio server,
 * including commands for connection management and data retrieval.
 */
class repl final {
private:
    inline static std::string_view logger_name =
        "ores.comms.shell.app.repl";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct a REPL instance with configuration.
     *
     * @param session Reference to a client session.
     */
    explicit repl(comms::net::client_session& session);

    repl(const repl&) = delete;
    repl& operator=(const repl&) = delete;
    repl(repl&&) = delete;
    repl& operator=(repl&&) = delete;

    /**
     * @brief Run the REPL session.
     *
     * Starts the I/O thread, displays the welcome message, and enters
     * the interactive command loop. Blocks until the user exits.
     */
    void run();

private:
    /**
     * @brief Setup the command menu structure.
     *
     * Creates the root menu and all submenus, wiring them together.
     *
     * @return The configured CLI instance
     */
    std::unique_ptr<::cli::Cli> setup_menus();

    /**
     * @brief Display the welcome message.
     */
    void display_welcome() const;

    /**
     * @brief Perform cleanup on REPL exit.
     *
     * Sends a logout request if logged in and disconnects cleanly from
     * the server to avoid abrupt connection termination.
     */
    void cleanup();

    comms::net::client_session& session_;
};

}

#endif
