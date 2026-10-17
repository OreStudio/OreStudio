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
#ifndef ORES_SHELL_APP_COMMANDS_WORKFLOW_WORKFLOW_RUN_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_WORKFLOW_WORKFLOW_RUN_COMMANDS_HPP

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
 * @brief The workflow verbs that are not messages: wait for a run, and start one.
 *
 * Every other workflow verb is generated from ores.workflow.workflow_messages
 * into workflow_operations_commands, which owns the workflow menu. These two
 * stay hand-written because each does work around its message. Waiting polls
 * the steps read until the run reaches a terminal state. Starting checks the
 * type against the registered definitions, generates the instance id the
 * caller follows, and publishes on the stream, where no reply comes back.
 *
 * Bundle publication, ORE import and tenant provisioning dispatch a workflow
 * and then block on it, so wait_for_instance is public beside the verb and
 * they block by the same rules.
 */
class workflow_run_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.workflow.workflow_run_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Add the wait and start verbs to the generated workflow menu.
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
     * polls, because a long wait routinely survives a network blip. An
     * instance that is not found yet is the same kind of transient: the
     * engine creates the row after the request that names it is answered,
     * so the first poll of a fresh run can legitimately miss it, and that
     * first miss is not reported as a warning. A miss that persists is
     * still reported, and the retry budget still runs out loudly.
     *
     * @param expected_state When non-empty, the wait asserts the instance's own
     * terminal state instead of step completion. A run that is meant to fail
     * never completes its steps, so this is the only way a script can assert
     * one: "completed", "failed" or "compensated". Reaching a different
     * terminal state fails.
     *
     * @return true when the instance reached the asserted state, or completed
     * successfully when no state was named.
     */
    static bool wait_for_instance(std::ostream& out,
                                  ores::nats::service::nats_client& session,
                                  const std::string& instance_id,
                                  std::chrono::seconds timeout,
                                  std::size_t expected_steps = 0,
                                  const std::string& expected_state = {});

    /**
     * @brief Follow the run a completed instance handed its work to.
     *
     * A run finishes its own steps by starting another: its last step records
     * that run's id as @c child_instance_id in the step's result. Followed
     * only to the handover, the command that started the first run reports work
     * as done while the second run is still doing it. This reads the result of
     * the run's last completed step, and follows the child to completion by the
     * same rules as any other wait, so both are reported the same way.
     *
     * A run whose steps hand nothing on returns immediately, and so does a step
     * whose result names no child. A result that cannot be read is reported as
     * a failure: reporting success for work the run may have handed on is the
     * defect this exists to prevent.
     *
     * @return true when nothing was handed on, or when the child run completed;
     * false after reporting a child that failed, timed out, or could not be
     * followed.
     */
    static bool follow_child_instance(std::ostream& out,
                                      ores::nats::service::nats_client& session,
                                      const std::string& instance_id,
                                      std::chrono::seconds timeout);

    /**
     * @brief Start a workflow by type and print the instance id to follow.
     *
     * Usage: workflow start <type> <request_json>
     *
     * The instance id is generated here rather than by the engine, because the
     * start is fire-and-forget: the reply is an acknowledgement, not the result,
     * so the caller needs the id in hand to wait on it. Same reason the
     * commissioners pre-generate it.
     */
    static void process_start(std::ostream& out,
                              ores::nats::service::nats_client& session,
                              const std::vector<std::string>& args);
};

}

#endif
