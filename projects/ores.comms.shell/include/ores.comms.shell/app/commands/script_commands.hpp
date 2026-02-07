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
#ifndef ORES_COMMS_SHELL_APP_COMMANDS_SCRIPT_COMMANDS_HPP
#define ORES_COMMS_SHELL_APP_COMMANDS_SCRIPT_COMMANDS_HPP

namespace cli {

class Menu;
class CliSession;

}

namespace ores::comms::shell::app::commands {

/**
 * @brief Commands for loading and executing shell scripts.
 */
class script_commands {
public:
    /**
     * @brief Register script-related commands.
     *
     * Adds the 'load' command which reads a .ores script file and
     * executes each line as a shell command via CliSession::Feed().
     *
     * @param root The root menu to add commands to.
     * @param active_session Pointer to the active CLI session, set
     *        by repl::run() before the session loop starts.
     */
    static void register_commands(cli::Menu& root,
        cli::CliSession*& active_session);
};

}

#endif
