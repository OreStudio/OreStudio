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
#ifndef ORES_SHELL_APP_COMMANDS_SESSION_SAMPLES_OPERATIONS_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_SESSION_SAMPLES_OPERATIONS_COMMANDS_HPP

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
 * @brief The operations session_samples declares that no entity's CRUD verbs state.
 *
 * One command per message the protocol declares with a subject and a response,
 * so the REPL surface and the protocol stay one declaration.
 */
class session_samples_operations_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.iam.session_samples_operations_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the session_samples operations on the root menu.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief get-session-samples <session_id>
     */
    static void process_get_session_samples(std::ostream& out,
                                            ores::nats::service::nats_client& session,
                                            const std::vector<std::string>& args);
};

}

#endif
