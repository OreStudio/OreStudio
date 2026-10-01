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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_shell_command_aggregator_header.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SHELL_APP_COMMANDS_TRADING_TRADING_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_TRADING_TRADING_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Registers every trading command unit.
 *
 * The units are generated, one per trading model that opts in to the
 * shell-command facet through its properties drawer, plus the local units the
 * component's shell overview declares. Each owns a submenu. This aggregator is
 * the one entry the host calls, and its list is rendered from the component's
 * declaration, so a unit that joins or leaves the surface changes the
 * registration without an edit here.
 */
class trading_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.trading.trading_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);
};

}

#endif
