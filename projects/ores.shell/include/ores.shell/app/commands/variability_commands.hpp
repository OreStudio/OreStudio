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
#ifndef ORES_SHELL_APP_COMMANDS_VARIABILITY_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_VARIABILITY_COMMANDS_HPP

#include <string>
#include "ores.utility/log/make_logger.hpp"
#include "ores.shell/app/client_manager.hpp"

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Manages commands related to variability subsystem.
 *
 * Handles feature flags, configuration toggles, and other
 * variability-related operations.
 */
class variability_commands {
private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.shell.app.commands.variability");
        return instance;
    }

public:
    /**
     * @brief Register variability-related commands.
     *
     * Creates the variability submenu and adds variability operations.
     */
    static void register_commands(cli::Menu& root_menu, client_manager& client_manager);

    /**
     * @brief Process a list feature flags request.
     *
     * Retrieves all feature flags from the server and displays them.
     *
     * @param out Output stream for results
     * @param client_manager Manager for client connectivity.
     */
    static void process_list_feature_flags(std::ostream& out, client_manager& client_manager);
};

}

#endif
