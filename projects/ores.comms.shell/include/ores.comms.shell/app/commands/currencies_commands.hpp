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
#ifndef ORES_COMMS_SHELL_APP_COMMANDS_CURRENCIES_COMMANDS_HPP
#define ORES_COMMS_SHELL_APP_COMMANDS_CURRENCIES_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.comms/net/client_session.hpp"

namespace cli {

class Menu;

}

namespace ores::comms::shell::app::commands {

/**
 * @brief Manages commands related to CURRENCIES.
 */
class currencies_commands {
private:
    inline static std::string_view logger_name =
        "ores.comms.shell.app.commands.currencies_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register currency-related commands.
     *
     * Creates the currencies submenu and adds currency operations.
     */
    static void register_commands(cli::Menu& root_menu,
        comms::net::client_session& session);

    /**
     * @brief Process a get currencies request.
     *
     * Retrieves all currencies from the server and displays them.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     */
    static void process_get_currencies(std::ostream& out,
        comms::net::client_session& session);

    /**
     * @brief Process an add currency request.
     *
     * Creates a new currency with the provided details. The modified_by field
     * is automatically set from the logged-in user's session.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param iso_code ISO 4217 alphabetic code (e.g., "USD")
     * @param name Full name of the currency
     * @param numeric_code ISO 4217 numeric code (optional)
     * @param symbol Currency symbol (optional)
     * @param fractions_per_unit Number of fractional units (default: 100)
     */
    static void process_add_currency(std::ostream& out,
        comms::net::client_session& session,
        std::string iso_code, std::string name,
        std::string numeric_code, std::string symbol,
        std::string fractions_per_unit);
};

}

#endif
