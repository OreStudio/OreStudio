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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_WORKFLOW_API_MESSAGING_WORKFLOW_PROTOCOL_HPP
#define ORES_WORKFLOW_API_MESSAGING_WORKFLOW_PROTOCOL_HPP

#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/messaging/workflow_vocabulary.hpp"
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::workflow::messaging {

/**
 * @brief A single structured log entry emitted by a step handler.
 *
 * Entries are stored as a JSON array in workflow_step.step_log_json and
 * surfaced in the workflow instance detail dialog.  The context field
 * carries item-level identifiers (trade ID, filename, etc.) so the user
 * can locate the source of each message.
 */
struct step_log_entry {
    ores::workflow::messaging::step_log_level level =
        ores::workflow::messaging::step_log_level::info;
    std::string message;
    std::string context;
};

/**
 * @brief Fire-and-forget event published by domain services on step completion.
 *
 * Published to workflow.v1.events.step-completed by any domain service that
 * participates in a workflow. The workflow engine subscribes to this subject
 * (queue-group) and advances or compensates the workflow accordingly.
 *
 * The step_id must echo the X-Workflow-Step-Id header from the command that
 * triggered this step. It is used as the idempotency key: the engine checks
 * that the referenced workflow_step is still in_progress before acting.
 */
struct step_completed_event {
    static constexpr std::string_view nats_subject = "workflow.v1.events.step-completed";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief UUID of the parent workflow instance.
     */
    std::string workflow_instance_id;
    /**
     * @brief UUID of the workflow step being completed.
     *
     * Echoed from the X-Workflow-Step-Id header of the originating command.
     */
    std::string step_id;
    /**
     * @brief Terminal outcome of this step.
     */
    ores::workflow::messaging::step_outcome outcome =
        ores::workflow::messaging::step_outcome::completed;
    /**
     * @brief Serialised JSON result payload from the domain service.
     *
     * Stored in workflow_step.response_json and passed as input to
     * subsequent step command builders.
     */
    std::string result_json;
    /**
     * @brief Human-readable error message on fatal failure.
     *
     * Stored in workflow_step.error and workflow_instance.error.
     * Non-empty only when outcome == failed.
     */
    std::string error_message;
    /**
     * @brief Ordered list of log entries emitted by this step.
     *
     * Serialised to step_log_json in the workflow step record.  Empty for
     * steps that produce no user-visible diagnostic output.
     */
    std::vector<step_log_entry> log;
};

/**
 * @brief Fire-and-forget message to start a new workflow instance.
 *
 * Published by client services (e.g. ores.reporting.service) to request
 * that the workflow engine create and drive a new workflow_instance.
 */
struct start_workflow_message {
    static constexpr std::string_view nats_subject = "workflow.v1.start";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief Workflow type name to look up in the registry.
     */
    std::string type;
    /**
     * @brief Tenant the workflow runs on behalf of.
     */
    std::string tenant_id;
    /**
     * @brief What the run acts on, named by the caller, or empty.
     *
     * The engine stores it on the instance and never interprets it, so a caller
     * states its own kind without the engine learning the caller's vocabulary.
     */
    std::string target_kind;
    /**
     * @brief Identity of the entity the run acts on, or empty.
     *
     * A start that names a target the engine cannot read is refused rather than
     * run without one, because a run that cannot be found by what it acts on is
     * a run the caller cannot follow.
     */
    std::string target_id;
    /**
     * @brief Serialised JSON payload for the initial step's command builder.
     */
    std::string request_json;
    /**
     * @brief Optional distributed tracing correlation ID.
     */
    std::string correlation_id;
    /**
     * @brief Optional pre-generated workflow instance UUID.
     *
     * When non-empty the engine uses this UUID for the new workflow_instance
     * record instead of generating one. Callers that need to return the
     * instance ID before the engine has processed the message (e.g. the
     * ore_import handler) pre-generate a UUID here so they can include it
     * in the synchronous response to the client.
     *
     * Empty string (default) means the engine generates a fresh UUID.
     */
    std::string instance_id;
};

/**
 * @brief Queries the workflow engine for a previously-completed step result.
 *
 * Domain service handlers send this before executing a workflow step command.
 * If the step already completed (e.g. the command was re-dispatched after a
 * restart), the handler can replay the cached result without re-executing.
 *
 * No authentication required: the step ID is an opaque idempotency key and the
 * reply is confined to the tenant the request names. The engine hands the
 * caller that tenant in the X-Tenant-Id header of the command it dispatched,
 * and the caller echoes it back, so the query stays inside the tenant the step
 * belongs to. A request that names no tenant is answered with found = false,
 * because there is no tenant whose data it could be entitled to.
 */
struct get_step_result_request {
    using response_type = struct get_step_result_response;
    static constexpr std::string_view nats_subject = "workflow.v1.steps.get-result";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief UUID of the workflow step to look up, echoed from X-Workflow-Step-Id.
     */
    std::string step_id;
    /**
     * @brief The tenant the step belongs to, echoed from X-Tenant-Id.
     *
     * The lookup is scoped to it, so a step in another tenant reads as not
     * found rather than being returned.
     */
    std::string tenant_id;
};

struct get_step_result_response {
    /**
     * @brief true if the step exists and has reached a terminal state.
     *
     * false if the step is not found, is pending, or is still in_progress.
     * Callers should proceed with normal execution when found == false.
     */
    bool found = false;
    /**
     * @brief Terminal outcome of the step.
     *
     * Valid only when found == true.
     */
    ores::workflow::messaging::step_outcome outcome =
        ores::workflow::messaging::step_outcome::completed;
    /**
     * @brief Serialised JSON result from the original execution.
     *
     * Non-empty when found == true && outcome != failed.
     */
    std::string result_json;
    /**
     * @brief Human-readable error from the original execution.
     *
     * Non-empty when found == true && outcome == failed.
     */
    std::string error_message;
    /**
     * @brief Log entries emitted during the original execution.
     *
     * Non-empty when the step produced user-visible diagnostics.
     */
    std::vector<step_log_entry> log;
    bool success = false;
    std::string message;
};

/**
 * @brief Summary of a single workflow instance returned by list_instances.
 *
 * All timestamps are UTC ISO-8601 strings (e.g. "2026-04-10T14:30:00Z").
 * status is a human-readable state name: in_progress, completed, failed,
 * compensating, or compensated.
 */
struct workflow_instance_summary {
    std::string id;
    std::string type;
    std::string status;
    int current_step_index = 0;
    int step_count = 0;
    std::string correlation_id;
    std::string created_by;
    std::string created_at;
    std::optional<std::string> completed_at;
    std::string error;
    /**
     * @brief What the run acts on, and its identity, as the start message named
     * them. Both are empty when the run acts on no entity.
     */
    std::string target_kind;
    std::string target_id;
};

/**
 * @brief Request to list workflow instances for the authenticated tenant.
 *
 * Results are ordered by created_at descending (most recent first).
 * Requires a valid Bearer JWT in the Authorization NATS header.
 */
struct list_workflow_instance_summaries_request {
    using response_type = struct list_workflow_instance_summaries_response;
    static constexpr std::string_view nats_subject = "workflow.v1.instances.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief Maximum number of instances to return (default 200, max 1000).
     */
    int limit = 200;
    /**
     * @brief Status filter. Empty means every status.
     *
     * Valid values: "in_progress", "completed", "failed",
     *               "compensating", "compensated"
     */
    std::string status_filter = {};
    /**
     * @brief Type filter. Empty means every type.
     */
    std::string type_filter = {};
    /**
     * @brief Kind of the entity the run acts on. Empty means any kind.
     *
     * Matched exactly and on its own, so a caller that names the kind alone gets
     * every run that acts on that kind of entity. A run with no target matches no
     * kind.
     */
    std::string target_kind_filter = {};
    /**
     * @brief Identity of the entity the run acts on. Empty means any entity.
     *
     * A run with no target matches no identity.
     */
    std::string target_id_filter = {};
};

struct list_workflow_instance_summaries_response {
    bool success = false;
    std::string message;
    std::vector<workflow_instance_summary> instances;
};

/**
 * @brief Summary of a single step within a workflow instance.
 */
struct workflow_step_summary {
    std::string id;
    std::string name;
    /**
     * @brief The step's name and description in a person's words, as its definition
     * declared them, and empty when it declared none.
     */
    std::string label;
    std::string description;
    std::string status;
    int step_index = 0;
    std::string created_at;
    std::optional<std::string> started_at;
    std::optional<std::string> completed_at;
    std::string error;
    std::vector<step_log_entry> log;
};

/**
 * @brief Request to retrieve all steps for a specific workflow instance.
 *
 * The instance must belong to the authenticated tenant; the handler returns
 * an error if the instance is owned by a different tenant.
 * Requires a valid Bearer JWT in the Authorization NATS header.
 */
struct get_workflow_steps_request {
    using response_type = struct get_workflow_steps_response;
    static constexpr std::string_view nats_subject = "workflow.v1.instances.steps";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string workflow_instance_id;
};

struct get_workflow_steps_response {
    bool success = false;
    std::string message;
    /**
     * @brief Instance-level status: in_progress, completed, failed,
     * compensating, or compensated.
     *
     * A failed instance may have zero steps (e.g. a start that died before
     * persisting its first step), so callers must consult status/error rather
     * than inferring failure from the step list alone.
     */
    std::string status;
    /**
     * @brief Instance-level error message, populated when status is failed
     * (or during compensation).
     */
    std::string error;
    /**
     * @brief Total step count of the instance (may exceed steps.size() when
     * not all steps have been created yet).
     */
    int step_count = 0;
    /**
     * @brief Zero-based index of the step currently being executed.
     */
    int current_step_index = 0;
    std::vector<workflow_step_summary> steps;
};

/**
 * @brief Summary of a single step within a workflow definition.
 */
struct workflow_step_definition_summary {
    int step_index = 0;
    std::string name;
    std::string description;
    std::string command_subject;
    bool has_compensation = false;
};

/**
 * @brief Summary of a workflow definition registered in the engine.
 */
struct workflow_definition_summary {
    std::string type_name;
    std::string description;
    int step_count = 0;
    std::vector<workflow_step_definition_summary> steps;
};

/**
 * @brief Request to list all registered workflow definitions.
 *
 * Returns the type name, description, and step metadata for every
 * workflow type known to the engine. No authentication required — the
 * definitions are public metadata.
 */
struct list_workflow_definitions_request {
    using response_type = struct list_workflow_definitions_response;
    static constexpr std::string_view nats_subject = "workflow.v1.definitions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct list_workflow_definitions_response {
    bool success = false;
    std::string message;
    std::vector<workflow_definition_summary> definitions;
};

/**
 * @brief Asks the engine to resume a stopped run from the step that failed.
 *
 * A retry is the second half of the failure policy the workflow definition
 * declares: a failure stops the run and keeps every completed step, and a
 * retry re-dispatches the step that failed so the run can finish.
 */
struct retry_workflow_instance_request {
    using response_type = struct retry_workflow_instance_response;
    static constexpr std::string_view nats_subject = "workflow.v1.instances.retry";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The run to resume.
     */
    std::string workflow_instance_id;
    /**
     * @brief The step to resume from, by the name the definition gave it.
     *
     * Empty means the step that failed, which is what a page that has just
     * rendered a stopped run asks for: it holds the whole step list from the
     * progress read and names one only when a person chooses to resume from
     * somewhere other than where the run stopped.
     */
    std::string step_name = {};
};

struct retry_workflow_instance_response {
    bool success = false;
    /**
     * @brief Why the run was not resumed, or empty when it was.
     *
     * A refusal names what it refused: a run that is not stopped, a step the
     * run does not hold, or a step whose predecessors have not all completed.
     */
    std::string message;
    /**
     * @brief The run the answer is about, echoed from the request.
     */
    std::string workflow_instance_id;
    /**
     * @brief The index of the step the engine re-dispatched, or -1 on refusal.
     */
    int step_index = -1;
    /**
     * @brief The name of the step the engine re-dispatched, or empty on refusal.
     */
    std::string step_name;
};

}

#endif
