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
#ifndef ORES_SHELL_APP_COMMANDS_WORKFLOW_WORKFLOW_WAIT_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_WORKFLOW_WORKFLOW_WAIT_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <chrono>
#include <cstddef>
#include <ostream>
#include <string>
#include <string_view>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Blocking on a dispatched instance, which is not an entity verb.
 *
 * The two generated units beside this one answer the entities' own
 * derivation: they read and write workflow_instances and workflow_steps
 * rows. Waiting for a run to finish is not one of those verbs -- it polls
 * the engine's step query until the instance reaches a terminal state --
 * so generation has no unit for it and this one stays hand-written.
 *
 * It is registered because callers outside the REPL reach for it: bundle
 * publication, ORE import and party provisioning each dispatch a workflow
 * and then block on it, and a run that chooses not to block tells the
 * operator to follow progress with `workflow wait`. wait_for_instance is
 * public beside the command so those callers block by the same rules
 * rather than a second copy of them.
 *
 * P04 records the operation model that would let generation take this
 * over, as it does for iam's generated *_operations_commands units.
 */
class workflow_wait_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.workflow.workflow_wait_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the workflow submenu, which holds the wait verb.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief Block until a workflow instance reaches a terminal state.
     *
     * Polls the instance's steps every three seconds, printing each step's
     * status transitions. A step in status "failed" is a terminal failure;
     * all steps completed, with or without warnings, is terminal success.
     * Steps appear incrementally as the workflow progresses, so a caller
     * that knows the expected count -- bundle publication returns
     * datasets_dispatched -- passes expected_steps, and success then also
     * requires that many steps to exist.
     *
     * Transport and parse errors are tolerated for a few consecutive
     * polls, because a long wait routinely survives a network blip.
     *
     * @return true when the instance completed successfully.
     */
    static bool wait_for_instance(std::ostream& out,
                                  ores::nats::service::nats_client& session,
                                  const std::string& instance_id,
                                  std::chrono::seconds timeout,
                                  std::size_t expected_steps = 0);
};

}

#endif
