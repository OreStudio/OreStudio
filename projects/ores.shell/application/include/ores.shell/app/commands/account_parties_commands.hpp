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
#ifndef ORES_SHELL_APP_COMMANDS_ACCOUNT_PARTIES_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_ACCOUNT_PARTIES_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <string>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Commands for account-to-party associations.
 *
 * Granting an account access to a party is the step the tenant
 * provisioning wizard performs for the tenant admin against every
 * operational party.
 */
class account_parties_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.account_parties_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register account-party commands.
     *
     * Creates the account-parties submenu with list and add
     * operations.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief List existing account-party associations.
     */
    static void process_list(std::ostream& out, ores::nats::service::nats_client& session);

    /**
     * @brief Associate an account with a party (both UUIDs).
     *
     * The party is looked up to validate it exists and to source the
     * tenant id, as the wizard does.
     */
    static void process_add(std::ostream& out,
                            ores::nats::service::nats_client& session,
                            const std::string& account_id,
                            const std::string& party_id);
};

}

#endif
