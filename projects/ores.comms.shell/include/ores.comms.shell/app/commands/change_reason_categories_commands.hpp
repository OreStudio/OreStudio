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
#ifndef ORES_COMMS_SHELL_APP_COMMANDS_CHANGE_REASON_CATEGORIES_COMMANDS_HPP
#define ORES_COMMS_SHELL_APP_COMMANDS_CHANGE_REASON_CATEGORIES_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.comms/net/client_session.hpp"

namespace cli {

class Menu;

}

namespace ores::comms::shell::app::commands {

/**
 * @brief Manages commands related to change reason categories.
 */
class change_reason_categories_commands {
private:
    inline static std::string_view logger_name =
        "ores.comms.shell.app.commands.change_reason_categories_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register change reason category-related commands.
     *
     * Creates the change-reason-categories submenu and adds operations.
     */
    static void register_commands(cli::Menu& root_menu,
        comms::net::client_session& session);

    /**
     * @brief Process a get change reason categories request.
     *
     * Retrieves all change reason categories from the server.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     */
    static void process_get_categories(std::ostream& out,
        comms::net::client_session& session);

    /**
     * @brief Process an add change reason category request.
     *
     * Creates a new change reason category with the provided details.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param code Unique category code (e.g., "static_data")
     * @param description Human-readable description
     * @param change_commentary Free-text commentary explaining the change
     */
    static void process_add_category(std::ostream& out,
        comms::net::client_session& session,
        std::string code, std::string description,
        std::string change_commentary);

    /**
     * @brief Process a delete change reason category request.
     *
     * Deletes a category by its code. Requires authentication.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param code Code of the category to delete
     */
    static void process_delete_category(std::ostream& out,
        comms::net::client_session& session,
        std::string code);

    /**
     * @brief Process a get category history request.
     *
     * Retrieves the version history for a category by its code.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param code Code of the category
     */
    static void process_get_category_history(std::ostream& out,
        comms::net::client_session& session,
        std::string code);
};

}

#endif
