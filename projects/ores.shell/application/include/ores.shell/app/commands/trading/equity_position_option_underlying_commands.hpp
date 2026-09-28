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
#ifndef ORES_SHELL_APP_COMMANDS_TRADING_EQUITY_POSITION_OPTION_UNDERLYING_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_TRADING_EQUITY_POSITION_OPTION_UNDERLYING_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <string>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Manages the entry rows of an EQUITY POSITION's option.
 *
 * The generated equity-position command writes the parent instrument. Its
 * option entries are child rows, and the generated surface has no argument
 * for them, so this hand-written verb takes the entries as one readable,
 * comma-separated argument and writes each row.
 */
class equity_position_option_underlying_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.trading.equity_position_option_underlying_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the equity-position entry commands.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief Write the entry rows of one equity position instrument.
     *
     * The entries argument is a comma-separated list. Each entry states, in
     * the child table's column order,
     * =name:strike:long_short[:weight[:option_type[:exercise_type[:settlement_type]]]]=.
     * A dash states no entries.
     */
    static void process_set_underlyings(std::ostream& out,
                                        ores::nats::service::nats_client& session,
                                        std::string trade_id,
                                        std::string entries);
};

}

#endif
