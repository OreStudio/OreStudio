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
#ifndef ORES_SHELL_APP_COMMANDS_WORKFLOW_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_WORKFLOW_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <chrono>
#include <string>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Commands for observing workflow instances.
 *
 * Bundle publication and other long-running operations dispatch a
 * workflow and return an instance id; these commands poll the
 * workflow engine until the instance reaches a terminal state, as
 * the GUI's WorkflowStepsWidget does.
 */
class workflow_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.workflow_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register workflow-related commands.
     *
     * Creates the workflow submenu with steps and wait operations.
     */
    static void register_commands(cli::Menu& root_menu,
                                  ores::nats::service::nats_client& session);

    /**
     * @brief Display the current steps of a workflow instance.
     */
    static void process_steps(std::ostream& out,
                              ores::nats::service::nats_client& session,
                              const std::string& instance_id);

    /**
     * @brief Block until a workflow instance reaches a terminal state.
     *
     * Polls workflow.v1.instances.steps every three seconds, printing
     * each step's status transitions. A step in status "failed" is a
     * terminal failure; all steps "completed" (with or without
     * warnings) is terminal success. Marks command failure on
     * workflow failure or timeout.
     *
     * @return true when the instance completed successfully.
     */
    static bool wait_for_instance(std::ostream& out,
                                  ores::nats::service::nats_client& session,
                                  const std::string& instance_id,
                                  std::chrono::seconds timeout);
};

}

#endif
