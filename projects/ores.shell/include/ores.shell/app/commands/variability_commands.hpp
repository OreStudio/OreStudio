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

#include "ores.logging/make_logger.hpp"
#include "ores.shell/service/nats_session.hpp"

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Manages commands related to variability subsystem.
 *
 * Handles system settings, configuration toggles, and other
 * variability-related operations.
 */
class variability_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.variability";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register variability-related commands.
     *
     * Creates the variability submenu and adds variability operations.
     */
    static void register_commands(cli::Menu& root_menu,
        service::nats_session& session);

    /**
     * @brief Process a list settings request.
     *
     * Retrieves all system settings from the server and displays them.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     */
    static void process_list_settings(std::ostream& out,
        service::nats_session& session);

    /**
     * @brief Process a save setting request.
     *
     * Creates or updates a system setting with the provided details.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param name Unique setting name
     * @param value Setting value
     * @param data_type Setting data type (boolean, integer, string, json)
     * @param description Human-readable description
     * @param change_reason_code Code identifying the reason for the change
     * @param change_commentary Free-text commentary
     */
    static void process_save_setting(std::ostream& out,
        service::nats_session& session,
        std::string name, std::string value, std::string data_type,
        std::string description,
        std::string change_reason_code, std::string change_commentary);

    /**
     * @brief Process a delete setting request.
     *
     * Deletes a system setting by its name. Requires authentication.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param name Name of the setting to delete
     */
    static void process_delete_setting(std::ostream& out,
        service::nats_session& session,
        std::string name);

    /**
     * @brief Process a get setting history request.
     *
     * Retrieves the version history for a system setting by its name.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param name Name of the setting
     */
    static void process_get_setting_history(std::ostream& out,
        service::nats_session& session,
        std::string name);
};

}

#endif
