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
#ifndef ORES_SHELL_APP_COMMANDS_MARKETDATA_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_MARKETDATA_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Commands for market data import.
 *
 * Gives ores.shell a non-interactive entry point into
 * import_market_data_request, the same request the Qt
 * ImportTradeDialog sends, so imports can run scripted or at
 * provisioning time without the GUI.
 */
class marketdata_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.marketdata_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register market data commands.
     *
     * Creates the marketdata submenu with the import operation.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief Import market data: marketdata import [--file <path>]
     * [--fixings <path>] [--source <tag>].
     *
     * Reads the ORE market.txt/fixings.txt file(s) named by --file
     * and --fixings (at least one is required) and sends their
     * content, along with the optional --source tag, as a single
     * import_market_data_request over NATS.
     */
    static void process_import(std::ostream& out,
                               ores::nats::service::nats_client& session,
                               const std::vector<std::string>& args);
};

}

#endif
