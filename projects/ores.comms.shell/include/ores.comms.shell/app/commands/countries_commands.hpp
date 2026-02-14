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
#ifndef ORES_COMMS_SHELL_APP_COMMANDS_COUNTRIES_COMMANDS_HPP
#define ORES_COMMS_SHELL_APP_COMMANDS_COUNTRIES_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.comms/net/client_session.hpp"
#include "ores.comms.shell/app/pagination_context.hpp"

namespace cli {

class Menu;

}

namespace ores::comms::shell::app::commands {

/**
 * @brief Manages commands related to countries.
 */
class countries_commands {
private:
    inline static std::string_view logger_name =
        "ores.comms.shell.app.commands.countries_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register country-related commands.
     *
     * Creates the countries submenu and adds country operations.
     */
    static void register_commands(cli::Menu& root_menu,
        comms::net::client_session& session,
        pagination_context& pagination);

    /**
     * @brief Process a get countries request.
     *
     * Retrieves countries from the server with pagination and displays them.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param pagination Pagination context for state management.
     */
    static void process_get_countries(std::ostream& out,
        comms::net::client_session& session,
        pagination_context& pagination);

    /**
     * @brief Process an add country request.
     *
     * Creates a new country with the provided details. The modified_by field
     * is automatically set from the logged-in user's session.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param alpha2_code ISO 3166-1 alpha-2 code (e.g., "US")
     * @param alpha3_code ISO 3166-1 alpha-3 code (e.g., "USA")
     * @param numeric_code ISO 3166-1 numeric code (e.g., "840")
     * @param name Short name of the country
     * @param official_name Official name of the country
     * @param change_reason_code Code identifying the reason for the change
     * @param change_commentary Free-text commentary explaining the change
     */
    static void process_add_country(std::ostream& out,
        comms::net::client_session& session,
        std::string alpha2_code, std::string alpha3_code,
        std::string numeric_code, std::string name,
        std::string official_name, std::string change_reason_code,
        std::string change_commentary);

    /**
     * @brief Process a delete country request.
     *
     * Deletes a country by its alpha-2 code. Requires authentication.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param alpha2_code ISO 3166-1 alpha-2 code of the country to delete
     */
    static void process_delete_country(std::ostream& out,
        comms::net::client_session& session,
        std::string alpha2_code);

    /**
     * @brief Process a get country history request.
     *
     * Retrieves the version history for a country by its alpha-2 code.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param alpha2_code ISO 3166-1 alpha-2 code of the country
     */
    static void process_get_country_history(std::ostream& out,
        comms::net::client_session& session,
        std::string alpha2_code);
};

}

#endif
