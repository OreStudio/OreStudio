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
#ifndef ORES_SHELL_APP_COMMANDS_LEI_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_LEI_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <string>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief The GLEIF country browser.
 *
 * The command-line replacement for the GUI's root-LEI entity picker. It lists
 * the countries that have entities, and the reads that narrow a country and
 * that search by name or LEI are generated units under the
 * lei_entity_summary menu. The LEI it names is the one bundle publication
 * takes as --root-lei.
 */
class lei_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.lei_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register LEI-related commands.
     *
     * Creates the lei submenu with the country browser.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief List the countries that have LEI entities.
     *
     * The country list is a grouping of one summary response, which is why it
     * is hand-written: no model declares a distinct-country read.
     */
    static void process_countries(std::ostream& out, ores::nats::service::nats_client& session);
};

}

#endif
