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
#include <expected>
#include <optional>
#include <ostream>
#include <string>
#include <string_view>
#include <vector>

namespace cli {

class Menu;

}

/**
 * @brief The wire messages this class reads.
 *
 * Declared rather than included so that a command header does not pull the
 * workflow protocol into every translation unit that includes it.
 */
namespace ores::workflow::messaging {

struct get_step_result_response;
struct get_workflow_steps_response;
struct workflow_step_summary;

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
     * @brief How a command's work ended when a run may hand some of it on.
     */
    enum class follow_result {
        /// Every run that does the work finished.
        complete,
        /// The run handed the rest of the work to a run this session cannot
        /// read, because that run belongs to another tenant. It continues
        /// there, and the command has no way to watch it.
        continues_elsewhere,
        /// Something failed and has been reported.
        failed
    };

    /**
     * @brief A run the last completed step of a finished run handed its work to.
     */
    struct handoff final {
        /// The child run's instance id, empty when the step handed nothing on.
        std::string instance_id;
        /// The tenant the child run belongs to, empty when the result names none.
        std::string tenant_id;
    };

    /**
     * @brief Block until a workflow instance reaches a terminal state.
     *
     * Polls the instance's steps every three seconds, printing each step's
     * status transitions. A step in status "failed" is a terminal failure, and
     * so is an instance whose own status is "failed" or "compensated". The run
     * succeeds once its status says "completed", or once every step it planned
     * has completed with or without warnings. The status is read because the
     * steps read answers a run this session may not see as a success with an
     * empty list, and a completed run must not be waited on for rows it will
     * never receive. The plan's size comes from the reply's step_count, the
     * total the instance was started with, so a run is never called finished
     * while a step it owes is still missing. A caller that knows a floor --
     * bundle publication returns datasets_dispatched -- passes expected_steps,
     * and success then also requires that many steps.
     *
     * Transport and parse errors are tolerated for a few consecutive polls,
     * because a long wait routinely survives a network blip. An unsuccessful
     * reply is tolerated the same way: the engine answers the start before it
     * creates the instance row, so the first poll of a fresh run is refused.
     * That one refusal is the poll this class expects, and it is logged at
     * info rather than warned about. Every later failure is printed, and the
     * retry budget runs out loudly either way.
     *
     * @param expected_steps A floor on the number of steps the run must show.
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
     * @brief Follow the run a finished instance handed its work to.
     *
     * A run can finish its own steps by starting another: its last completed
     * step records that run's id as @c child_instance_id in the step's result,
     * and names the tenant that run belongs to. Followed only to the handover,
     * the command that started the first run reports work as done while the
     * second run is still doing it. This reads that one step's result and
     * follows the child by the same rules as any other wait. Steps before the
     * last are not read: a handoff is the last thing a run does, so a child
     * named by an earlier step belongs to work the run already accounted for.
     *
     * A run whose last completed step names no child is the whole job, and so
     * is a run that has completed no step. A result that cannot be read is a
     * failure, because then the handoff is unknown. A result that proves
     * readable but names no child is no handoff: many steps complete without
     * recording anything.
     *
     * A child in another tenant is not read at all. This session cannot see
     * that tenant's steps, so the run is reported as continuing there and the
     * work is left to whoever administers that tenant. A child in this
     * session's tenant, or in any tenant when the session is the system
     * tenant, is followed. A handoff that names no tenant, or one that is not
     * a tenant id, is not followed either: a tenant the code cannot name must
     * not grant access to a run.
     *
     * @param timeout How long this may spend following the child. One budget
     * covers a whole command, so a caller passes what its own run left it.
     *
     * @return complete when nothing was handed on or the child finished,
     * continues_elsewhere when the child runs in a tenant this session cannot
     * read, and failed after reporting a child that failed, timed out, or
     * could not be followed.
     */
    static follow_result follow_child_instance(std::ostream& out,
                                               ores::nats::service::nats_client& session,
                                               const std::string& instance_id,
                                               std::chrono::seconds timeout);

    /**
     * @brief Whether a step's status says it did its work.
     *
     * A step that warned did the work and left something to report, so both
     * completed statuses count. Every reader of step status asks this, so the
     * pair is spelled in one place.
     */
    static bool is_step_complete(const ores::workflow::messaging::workflow_step_summary& step);

    /**
     * @brief The completed step with the highest index, or nullptr when none
     * completed.
     */
    static const ores::workflow::messaging::workflow_step_summary*
    last_completed_step(const std::vector<ores::workflow::messaging::workflow_step_summary>& steps);

    /**
     * @brief How many steps a run must show before it counts as finished.
     *
     * The reply's step_count is the run's plan, which is larger than the list
     * while a step is still to be created; the caller's floor covers a reply
     * that carries no plan. The larger of the two is what the run owes.
     */
    static std::size_t
    steps_expected(const ores::workflow::messaging::get_workflow_steps_response& reply,
                   std::size_t expected_steps);

    /**
     * @brief Whether every step the run planned has completed.
     *
     * An instance whose own status says "completed" is finished whatever its
     * step list holds, because a caller that cannot see the run receives an
     * empty list. Otherwise every planned step must be present and complete.
     */
    static bool run_completed(const ores::workflow::messaging::get_workflow_steps_response& reply,
                              std::size_t expected_steps);

    /**
     * @brief The work a step's result says its run handed on.
     *
     * A readable result that names no child leaves both fields empty. A result
     * the read did not produce, or one the server reports as absent, is an
     * error: the handoff is unknown, and treating it as "nothing handed on"
     * would report work as finished that another run may still be doing.
     */
    static std::expected<handoff, std::string>
    handoff_of_result(const ores::workflow::messaging::get_step_result_response& reply);

    /**
     * @brief Whether this session may follow the run a handoff names.
     *
     * A handoff that names no child is nothing to follow. A child in a tenant
     * this session cannot read is not followed; can_read_run_of says which
     * tenants those are.
     */
    static bool is_handoff_followed(std::string_view caller_tenant, const handoff& work);

    /**
     * @brief What is left of a command's budget once @p spent has passed.
     *
     * Never negative: a command whose first run used the whole budget has no
     * time left, and the run it handed to gets none.
     */
    static std::chrono::seconds remaining_budget(std::chrono::seconds timeout,
                                                 std::chrono::seconds spent);

    /**
     * @brief Whether this session may read a run that belongs to @p tenant_id.
     *
     * A run in this session's own tenant is readable. So is any run when the
     * session is the system tenant, which reads across tenants by design. A
     * tenant that is empty or not a tenant id is not readable: a field the code
     * elsewhere treats as unknown must not grant access.
     */
    static bool can_read_run_of(std::string_view caller_tenant, std::string_view run_tenant);

    /**
     * @brief Whether a failed poll is worth printing.
     *
     * The first refusal of a fresh run is the one expected poll; wait_for_instance
     * says why the engine refuses it. The retry budget absorbs it, and printing
     * it would mark a healthy command broken. Every later failure is printed,
     * and so is a transport error, which is never that race.
     *
     * @param consecutive_failures How many polls in a row have failed, this one
     * included.
     * @param replied Whether the server answered at all.
     */
    static bool report_poll_failure(int consecutive_failures, bool replied);

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
