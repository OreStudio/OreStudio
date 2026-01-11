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
#ifndef ORES_COMMS_SHELL_APP_COMMANDS_VARIABILITY_COMMANDS_HPP
#define ORES_COMMS_SHELL_APP_COMMANDS_VARIABILITY_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.comms/net/client_session.hpp"

namespace cli {

class Menu;

}

namespace ores::comms::shell::app::commands {

/**
 * @brief Manages commands related to variability subsystem.
 *
 * Handles feature flags, configuration toggles, and other
 * variability-related operations.
 */
class variability_commands {
private:
    inline static std::string_view logger_name =
        "ores.comms.shell.app.commands.variability";

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
        comms::net::client_session& session);

    /**
     * @brief Process a list feature flags request.
     *
     * Retrieves all feature flags from the server and displays them.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     */
    static void process_list_feature_flags(std::ostream& out,
        comms::net::client_session& session);

    /**
     * @brief Process an add feature flag request.
     *
     * Creates a new feature flag with the provided details.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param name Unique flag name
     * @param enabled Whether the flag is enabled ("true" or "false")
     * @param description Human-readable description
     * @param change_reason_code Code identifying the reason for the change
     * @param change_commentary Free-text commentary
     */
    static void process_add_feature_flag(std::ostream& out,
        comms::net::client_session& session,
        std::string name, std::string enabled, std::string description,
        std::string change_reason_code, std::string change_commentary);

    /**
     * @brief Process a delete feature flag request.
     *
     * Deletes a feature flag by its name. Requires authentication.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param name Name of the feature flag to delete
     */
    static void process_delete_feature_flag(std::ostream& out,
        comms::net::client_session& session,
        std::string name);

    /**
     * @brief Process a get feature flag history request.
     *
     * Retrieves the version history for a feature flag by its name.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param name Name of the feature flag
     */
    static void process_get_feature_flag_history(std::ostream& out,
        comms::net::client_session& session,
        std::string name);
};

}

#endif
