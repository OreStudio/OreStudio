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
#ifndef ORES_SHELL_APP_COMMANDS_CURRENCIES_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_CURRENCIES_COMMANDS_HPP

#include "ores.utility/log/make_logger.hpp"
#include "ores.shell/app/client_manager.hpp"

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Manages commands related to CURRENCIES.
 */
class currencies_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.currencies_commands";

    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register currency-related commands.
     *
     * Creates the currencies submenu and adds currency operations.
     */
    static void register_commands(cli::Menu& root_menu, client_manager& client_manager);

    /**
     * @brief Process a get currencies request.
     *
     * Retrieves all currencies from the server and displays them.
     *
     * @param out Output stream for results
     */
    static void process_get_currencies(std::ostream& out, client_manager& client_manager);
};

}

#endif
