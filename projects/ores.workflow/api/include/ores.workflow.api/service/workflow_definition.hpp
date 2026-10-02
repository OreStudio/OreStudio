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
#ifndef ORES_WORKFLOW_API_SERVICE_WORKFLOW_DEFINITION_HPP
#define ORES_WORKFLOW_API_SERVICE_WORKFLOW_DEFINITION_HPP

#include <chrono>
#include <cstdint>
#include <functional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::workflow::service {

// These types are header-only: every member is defined here and the
// component's library compiles no translation unit for them. A DLL import
// attribute on such a type makes every other component that constructs one
// reference a symbol this component's library never defines, which clang-cl
// reports as an unresolved import of the implicit default constructor when
// it links a consumer. The header therefore carries no export attribute.

/**
 * @brief The result of one completed step, named by the step that produced it.
 */
struct workflow_step_result {
    /**
     * @brief The step's name, as declared in its definition.
     */
    std::string name;

    /**
     * @brief The step's response payload. Empty when the step answered nothing.
     */
    std::string response_json;
};

/**
 * @brief The results of the steps completed so far, in step order.
 *
 * A step addresses a result by the name of the step that produced it, never by
 * position. A chain whose length depends on the run's configuration, or a step
 * that completes without answering, would otherwise shift what every later step
 * reads.
 */
using workflow_step_results = std::vector<workflow_step_result>;

/**
 * @brief The response produced by the named step, or nullptr if it has none.
 */
[[nodiscard]] inline const std::string* find_step_result(const workflow_step_results& results,
                                                         std::string_view step_name) {
    for (const auto& r : results) {
        if (r.name == step_name && !r.response_json.empty())
            return &r.response_json;
    }
    return nullptr;
}

/**
 * @brief Declarative definition of one step within a workflow.
 *
 * The workflow engine uses these descriptors to build and dispatch commands
 * without needing bespoke executor classes per workflow type.
 */
struct workflow_step_def {
    /**
     * @brief The step's identity, stored in workflow_step.name.
     *
     * Results are addressed by this name and a definition that renamed a step
     * would orphan every result read from it, so it is an identifier even when
     * it reads as words.
     */
    std::string name;

    /**
     * @brief The step's name in a person's words.
     *
     * A screen shows this where it shows the step, because the name above is
     * the one the engine and the logs use and a rail of identifiers reads as
     * nothing at all. Empty means the step has no better name than its
     * identity, and a reader falls back to that.
     */
    std::string label;

    /**
     * @brief What this step does, in a person's words.
     */
    std::string description;

    /**
     * @brief NATS subject to which the step command is published.
     *
     * E.g. "refdata.v1.parties.save"
     */
    std::string command_subject;

    /**
     * @brief NATS subject for the compensation command.
     *
     * Empty string means this step has no compensation action.
     * E.g. "refdata.v1.parties.delete"
     */
    std::string compensation_subject;

    /**
     * @brief How long the step may run before the engine declares it dead.
     *
     * The definition states it, because how long a step may take is knowledge
     * the step's author has and no default is right for both a write that
     * answers in milliseconds and an import that publishes tens of thousands
     * of rows. A step states no deadline and the engine refuses the run: a
     * command published into silence is the one failure nothing else in the
     * engine can see.
     *
     * A run that waits on another run budgets more than the steps it waits
     * for, so the inner deadline is what fails and the outer wait sees a
     * reason rather than running out of its own patience first.
     */
    std::chrono::seconds timeout{};

    /**
     * @brief Builds the step command payload.
     *
     * @param request_json  The workflow instance's originating request JSON.
     * @param step_results  The results of the steps completed so far, each
     *                      named by the step that produced it.
     * @return Serialised JSON to be published as the command body.
     */
    std::function<std::string(const std::string& request_json,
                              const workflow_step_results& step_results)>
        build_command;

    /**
     * @brief Builds the compensation command payload.
     *
     * Called when compensation is triggered after this step completed.
     *
     * @param command_json  The original command payload sent for this step.
     * @param result_json   The result payload received from the domain service.
     * @return Serialised JSON to be published as the compensation command body.
     */
    std::function<std::string(const std::string& command_json, const std::string& result_json)>
        build_compensation;
};

/**
 * @brief Serialisable snapshot of one step's metadata for a specific instance.
 *
 * Persisted as JSON in workflow_instance.materialised_steps_json so that
 * the step sequence is preserved across service restarts, even when
 * build_steps is non-deterministic.
 */
struct materialised_step {
    std::string name;
    std::string label;
    std::string description;
    std::string command_subject;
    std::string compensation_subject;
    /**
     * @brief The deadline the run was started with, in seconds.
     *
     * Carried with the run rather than read from the definition, because the
     * engine must be able to say when a step should have answered for a run
     * that a later build started: a definition that shortened a deadline would
     * otherwise expire runs that were started under a longer one.
     */
    std::uint32_t timeout_seconds = 0;
};

/**
 * @brief The budget for a step whose work is publishing or importing data.
 *
 * Such a step hands its work to another run and waits for it, so its budget is
 * minutes rather than seconds. A run that waits on another run states more than
 * the steps it waits for, so the inner deadline is the one that fails and the
 * outer wait sees a reason instead of running out of patience first.
 */
inline constexpr std::chrono::seconds data_step_timeout{900};

/**
 * @brief The budget for a step that waits on another run.
 *
 * A step that hands its work to another run states more than the steps inside
 * that run do. Its deadline is the second clock on the same work, and if the
 * two are equal the outer one can fire first and report that it did not finish
 * -- which is the least useful thing it could say, because the cause is one
 * level down: the step inside failed, and it knew why. The margin covers the
 * dispatch and the reporting either side of the inner deadline.
 *
 * A live proof of this rule's absence is in the task that introduced the
 * deadline: a party stage and the bundle run it waited on both stated the data
 * budget, and the party stage expired one second after the step that actually
 * had something to say.
 */
inline constexpr std::chrono::seconds orchestrating_step_timeout{1200};

/**
 * @brief The budget for a step whose work is a write or a handshake.
 *
 * Such a step answers in milliseconds when it answers at all, so a minute-scale
 * budget is already generous: the deadline is there to catch a service that has
 * gone, not to police a slow one.
 */
inline constexpr std::chrono::seconds write_step_timeout{120};

/**
 * @brief What a definition asks the engine to do when one of its steps fails.
 *
 * The policy belongs to the definition because it follows from what a
 * completed step is worth. A saga of writes leaves a partial result that is a
 * liability, so it rolls back; a definition whose completed steps are
 * published data a person expects to keep stops on the failed step and waits
 * for a retry, because every step is idempotent and undoing costs more than
 * resuming.
 *
 * The declaration carries no export attribute: the macro expands to a DLL
 * import or export attribute, which does not apply to an enum, and clang-cl
 * rejects it with -Wignored-attributes, an error under -Werror. An enum needs
 * no export attribute to travel across a binary boundary.
 */
enum class failure_policy : std::uint8_t {
    /// Roll the completed steps back and end in compensated. The default.
    compensate = 0,
    /// Stop on the failed step, keep every completed step, and end in failed.
    stop = 1
};

/**
 * @brief Declarative definition of a complete named workflow.
 *
 * Registered once at startup in the workflow_registry. The engine calls
 * build_steps once per instance at start time to determine the step sequence.
 */
struct workflow_definition {
    /**
     * @brief Unique type name matching workflow_instance.type.
     *
     * E.g. "provision_tenant_workflow"
     */
    std::string type_name;

    /**
     * @brief Human-readable description of what this workflow does.
     */
    std::string description;

    /**
     * @brief What the engine does when one of this definition's steps fails.
     */
    failure_policy on_failure = failure_policy::compensate;

    /**
     * @brief Whether the steps come from the request rather than the definition.
     *
     * A definition such as tenant provisioning builds one step per kind its
     * request orders, so it has no step list until a request arrives and
     * refuses an empty one. The definitions read lists it with no steps
     * instead of asking build_steps for a list it cannot give.
     */
    bool steps_depend_on_request = false;

    /**
     * @brief Builds the full step list for a specific workflow instance.
     *
     * Called once at instance start. The returned vector's size is persisted
     * as workflow_instance.step_count and the step metadata as
     * workflow_instance.materialised_steps_json. Never called again for
     * that instance (restart reads from DB instead).
     *
     * @param request_json   The workflow instance's originating request JSON.
     * @param tenant_id      UUID string of the tenant this instance runs for.
     * @param correlation_id Distributed tracing correlation ID.
     */
    std::function<std::vector<workflow_step_def>(const std::string& request_json,
                                                 const std::string& tenant_id,
                                                 const std::string& correlation_id)>
        build_steps;
};

}

#endif
