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
#ifndef ORES_SHELL_APP_COMMANDS_HISTORY_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_HISTORY_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Reads any entity's version history, whatever component owns it.
 *
 * One generic subject serves every entity: a service registers a provider per
 * entity type on a shared dispatch table, and answers for all of them at
 * =<component>.v1.history.get=. So the client needs no per-entity protocol and
 * this unit needs no per-entity command -- the entity type is an argument.
 *
 * That matters because the generated per-entity history commands cannot be
 * relied on. The protocol generator emits no history message, so an entity whose
 * protocol has been regenerated has no =history= command while one whose
 * protocol predates the change still does. Those that remain are stale rather
 * than working, and regenerating them removes the command. This reads the
 * subject the component actually serves.
 *
 * @see ores::history::messaging::get_entity_history_request
 */
class history_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.history_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the history submenu.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief List an entity's versions, newest first, or diff one of them.
     *
     * Usage: history get <entity_type> <entity_id> [--diff] [--version <n>]
     *
     * entity_type is the dotted name the service registered its provider under,
     * such as =ores.workflow.workflow_instance=; the component segment of it
     * selects the subject to ask, so no other argument is needed to address a
     * different component.
     */
    static void process_get(std::ostream& out,
                            ores::nats::service::nats_client& session,
                            const std::vector<std::string>& args);
};

}

#endif
