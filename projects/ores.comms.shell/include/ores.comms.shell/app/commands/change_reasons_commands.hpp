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
#ifndef ORES_COMMS_SHELL_APP_COMMANDS_CHANGE_REASONS_COMMANDS_HPP
#define ORES_COMMS_SHELL_APP_COMMANDS_CHANGE_REASONS_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.comms.shell/app/pagination_context.hpp"

namespace cli {

class Menu;

}

namespace ores::comms::shell::app::commands {

/**
 * @brief Manages commands related to change reasons.
 */
class change_reasons_commands {
private:
    inline static std::string_view logger_name =
        "ores.comms.shell.app.commands.change_reasons_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register change reason-related commands.
     *
     * Creates the change-reasons submenu and adds operations.
     *
     * Note: Change reasons protocol doesn't support pagination yet, but the
     * pagination_context is passed for API consistency.
     */
    static void register_commands(cli::Menu& root_menu,
        comms::net::client_session& session,
        pagination_context& pagination);

    /**
     * @brief Process a get change reasons request.
     *
     * Retrieves all change reasons from the server and displays them.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     */
    static void process_get_change_reasons(std::ostream& out,
        comms::net::client_session& session);

    /**
     * @brief Process an add change reason request.
     *
     * Creates a new change reason with the provided details.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param code Unique reason code (e.g., "static_data.correction")
     * @param description Human-readable description
     * @param category_code Category this reason belongs to
     * @param change_commentary Free-text commentary explaining the change
     */
    static void process_add_change_reason(std::ostream& out,
        comms::net::client_session& session,
        std::string code, std::string description,
        std::string category_code, std::string change_commentary);

    /**
     * @brief Process a delete change reason request.
     *
     * Deletes a change reason by its code. Requires authentication.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param code Code of the change reason to delete
     */
    static void process_delete_change_reason(std::ostream& out,
        comms::net::client_session& session,
        std::string code);

    /**
     * @brief Process a get change reason history request.
     *
     * Retrieves the version history for a change reason by its code.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param code Code of the change reason
     */
    static void process_get_change_reason_history(std::ostream& out,
        comms::net::client_session& session,
        std::string code);
};

}

#endif
