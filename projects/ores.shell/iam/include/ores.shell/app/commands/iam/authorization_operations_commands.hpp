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
 * Template: cpp_shell_operation_header.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SHELL_APP_COMMANDS_AUTHORIZATION_OPERATIONS_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_AUTHORIZATION_OPERATIONS_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <ostream>
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief The operations authorization declares that no entity's CRUD verbs state.
 *
 * One command per message the protocol declares with a subject and a response,
 * so the REPL surface and the protocol stay one declaration.
 */
class authorization_operations_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.iam.authorization_operations_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the authorization operations on the root menu.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief assign-role <account_id> <role_id>
     */
    static void process_assign_role(std::ostream& out,
                                    ores::nats::service::nats_client& session,
                                    const std::vector<std::string>& args);

    /**
     * @brief assign-role-by-name <principal> <role_name>
     */
    static void process_assign_role_by_name(std::ostream& out,
                                            ores::nats::service::nats_client& session,
                                            const std::vector<std::string>& args);

    /**
     * @brief revoke-role <account_id> <role_id>
     */
    static void process_revoke_role(std::ostream& out,
                                    ores::nats::service::nats_client& session,
                                    const std::vector<std::string>& args);

    /**
     * @brief revoke-role-by-name <principal> <role_name>
     */
    static void process_revoke_role_by_name(std::ostream& out,
                                            ores::nats::service::nats_client& session,
                                            const std::vector<std::string>& args);

    /**
     * @brief get-account-roles <account_id>
     */
    static void process_get_account_roles(std::ostream& out,
                                          ores::nats::service::nats_client& session,
                                          const std::vector<std::string>& args);

    /**
     * @brief get-account-permissions <account_id>
     */
    static void process_get_account_permissions(std::ostream& out,
                                                ores::nats::service::nats_client& session,
                                                const std::vector<std::string>& args);

    /**
     * @brief get-role-permissions <role_id>
     */
    static void process_get_role_permissions(std::ostream& out,
                                             ores::nats::service::nats_client& session,
                                             const std::vector<std::string>& args);

    /**
     * @brief suggest-role-commands <username> <tenant_id> <hostname>
     */
    static void process_suggest_role_commands(std::ostream& out,
                                              ores::nats::service::nats_client& session,
                                              const std::vector<std::string>& args);
};

}

#endif
