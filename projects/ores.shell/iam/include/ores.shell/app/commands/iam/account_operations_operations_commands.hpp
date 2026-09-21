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
#ifndef ORES_SHELL_APP_COMMANDS_ACCOUNT_OPERATIONS_OPERATIONS_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_ACCOUNT_OPERATIONS_OPERATIONS_COMMANDS_HPP

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
 * @brief The operations account_operations declares that no entity's CRUD verbs state.
 *
 * One command per message the protocol declares with a subject and a response,
 * so the REPL surface and the protocol stay one declaration.
 */
class account_operations_operations_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.iam.account_operations_operations_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the account_operations operations on the root menu.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief save-account <principal> <password> <totp_secret> <email> <account_type>
     */
    static void process_save_account(std::ostream& out,
                                     ores::nats::service::nats_client& session,
                                     const std::vector<std::string>& args);

    /**
     * @brief update-account <account_id> <email> <full_name> <default_party_id> <job_title>
     * <reports_to_account_id> <image_id> <change_reason_code> <change_commentary>
     */
    static void process_update_account(std::ostream& out,
                                       ores::nats::service::nats_client& session,
                                       const std::vector<std::string>& args);

    /**
     * @brief delete-account <account_id>
     */
    static void process_delete_account(std::ostream& out,
                                       ores::nats::service::nats_client& session,
                                       const std::vector<std::string>& args);

    /**
     * @brief lock-account <account_ids>
     */
    static void process_lock_account(std::ostream& out,
                                     ores::nats::service::nats_client& session,
                                     const std::vector<std::string>& args);

    /**
     * @brief unlock-account <account_ids>
     */
    static void process_unlock_account(std::ostream& out,
                                       ores::nats::service::nats_client& session,
                                       const std::vector<std::string>& args);

    /**
     * @brief list-login-info
     */
    static void process_list_login_info(std::ostream& out,
                                        ores::nats::service::nats_client& session,
                                        const std::vector<std::string>& args);

    /**
     * @brief reset-password <account_ids> <new_password>
     */
    static void process_reset_password(std::ostream& out,
                                       ores::nats::service::nats_client& session,
                                       const std::vector<std::string>& args);

    /**
     * @brief update-my-email <email>
     */
    static void process_update_my_email(std::ostream& out,
                                        ores::nats::service::nats_client& session,
                                        const std::vector<std::string>& args);

    /**
     * @brief set-my-default-party <party_id>
     */
    static void process_set_my_default_party(std::ostream& out,
                                             ores::nats::service::nats_client& session,
                                             const std::vector<std::string>& args);

    /**
     * @brief select-party <party_id>
     */
    static void process_select_party(std::ostream& out,
                                     ores::nats::service::nats_client& session,
                                     const std::vector<std::string>& args);

    /**
     * @brief switch-party <party_id>
     */
    static void process_switch_party(std::ostream& out,
                                     ores::nats::service::nats_client& session,
                                     const std::vector<std::string>& args);

    /**
     * @brief change-password <current_password> <new_password>
     */
    static void process_change_password(std::ostream& out,
                                        ores::nats::service::nats_client& session,
                                        const std::vector<std::string>& args);
};

}

#endif
