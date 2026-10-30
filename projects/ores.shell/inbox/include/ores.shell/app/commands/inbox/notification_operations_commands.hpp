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
#ifndef ORES_SHELL_APP_COMMANDS_NOTIFICATION_OPERATIONS_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_NOTIFICATION_OPERATIONS_COMMANDS_HPP

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
 * @brief The one operation ores.inbox.notification_operations declares that
 * no entity's CRUD verbs state: raising a notification.
 *
 * This unit is hand-written and its model does not opt into the shell facet.
 * The generator fills a list field from one comma-separated token, and the
 * raise takes a list of named values -- one name and one value per message
 * argument -- which that token cannot spell. Opting the model in fails the
 * generator's own refusal of a silent gap rather than producing a command.
 * The command stays the shape the generator would have given it: the same
 * menu, the same positional-then-flag order, the same `__none__` token for an
 * empty list, and the same JSON answer printed whole.
 *
 * The link id is a flag and not a positional, where the generator would have
 * made it one, because the tokenizer drops an empty argument: a notification
 * about a whole screen carries an empty link id, and an absent flag is the
 * only way this command line can say so.
 */
class notification_operations_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.inbox.notification_operations_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the notification_operations operations on the root menu.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief raise-notification <kind_code> <link_route> <account_ids>
     * [--link_id <v>] [--argument <name=value>] [--audience_permission_code <v>]
     */
    static void process_raise_notification(std::ostream& out,
                                           ores::nats::service::nats_client& session,
                                           const std::vector<std::string>& args);
};

}

#endif
