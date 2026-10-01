/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * @brief A single structured log entry emitted by a step handler.
 *
 * Entries are stored as a JSON array in workflow_step.step_log_json and
 * surfaced in the workflow instance detail dialog.  The context field
 * carries item-level identifiers (trade ID, filename, etc.) so the user
 * can locate the source of each message.
 */
export interface StepLogEntry {
    level: string;
    message: string;
    context: string;
}

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
export interface StepCompletedEvent {
    /**
     * @brief UUID of the parent workflow instance.
     */
    workflow_instance_id: string;
    /**
     * @brief UUID of the workflow step being completed.
     *
     * Echoed from the X-Workflow-Step-Id header of the originating command.
     */
    step_id: string;
    /**
     * @brief Terminal outcome of this step.
     */
    outcome: string;
    /**
     * @brief Serialised JSON result payload from the domain service.
     *
     * Stored in workflow_step.response_json and passed as input to
     * subsequent step command builders.
     */
    result_json: string;
    /**
     * @brief Human-readable error message on fatal failure.
     *
     * Stored in workflow_step.error and workflow_instance.error.
     * Non-empty only when outcome == failed.
     */
    error_message: string;
    /**
     * @brief Ordered list of log entries emitted by this step.
     *
     * Serialised to step_log_json in the workflow step record.  Empty for
     * steps that produce no user-visible diagnostic output.
     */
    log: StepLogEntry[];
}

/**
 * @brief Fire-and-forget message to start a new workflow instance.
 *
 * Published by client services (e.g. ores.reporting.service) to request
 * that the workflow engine create and drive a new workflow_instance.
 */
export interface StartWorkflowMessage {
    /**
     * @brief Workflow type name to look up in the registry.
     */
    type: string;
    /**
     * @brief Tenant the workflow runs on behalf of.
     */
    tenant_id: string;
    /**
     * @brief What the run acts on, named by the caller, or empty.
     *
     * The engine stores it on the instance and never interprets it, so a caller
     * states its own kind without the engine learning the caller's vocabulary.
     */
    target_kind: string;
    /**
     * @brief Identity of the entity the run acts on, or empty.
     *
     * A start that names a target the engine cannot read is refused rather than
     * run without one, because a run that cannot be found by what it acts on is
     * a run the caller cannot follow.
     */
    target_id: string;
    /**
     * @brief Serialised JSON payload for the initial step's command builder.
     */
    request_json: string;
    /**
     * @brief Optional distributed tracing correlation ID.
     */
    correlation_id: string;
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
    instance_id: string;
}

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
export interface GetStepResultRequest {
    /**
     * @brief UUID of the workflow step to look up, echoed from X-Workflow-Step-Id.
     */
    step_id: string;
    /**
     * @brief The tenant the step belongs to, echoed from X-Tenant-Id.
     *
     * The lookup is scoped to it, so a step in another tenant reads as not
     * found rather than being returned.
     */
    tenant_id: string;
}

export interface GetStepResultResponse {
    /**
     * @brief true if the step exists and has reached a terminal state.
     *
     * false if the step is not found, is pending, or is still in_progress.
     * Callers should proceed with normal execution when found == false.
     */
    found: boolean;
    /**
     * @brief Terminal outcome of the step.
     *
     * Valid only when found == true.
     */
    outcome: string;
    /**
     * @brief Serialised JSON result from the original execution.
     *
     * Non-empty when found == true && outcome != failed.
     */
    result_json: string;
    /**
     * @brief Human-readable error from the original execution.
     *
     * Non-empty when found == true && outcome == failed.
     */
    error_message: string;
    /**
     * @brief Log entries emitted during the original execution.
     *
     * Non-empty when the step produced user-visible diagnostics.
     */
    log: StepLogEntry[];
    success: boolean;
    message: string;
}

/**
 * @brief Summary of a single workflow instance returned by list_instances.
 *
 * All timestamps are UTC ISO-8601 strings (e.g. "2026-04-10T14:30:00Z").
 * status is a human-readable state name: in_progress, completed, failed,
 * compensating, or compensated.
 */
export interface WorkflowInstanceSummary {
    id: string;
    type: string;
    status: string;
    current_step_index: number;
    step_count: number;
    correlation_id: string;
    created_by: string;
    created_at: string;
    completed_at: string | null;
    error: string;
    /**
     * @brief What the run acts on, and its identity, as the start message named
     * them. Both are empty when the run acts on no entity.
     */
    target_kind: string;
    target_id: string;
}

/**
 * @brief Request to list workflow instances for the authenticated tenant.
 *
 * Results are ordered by created_at descending (most recent first).
 * Requires a valid Bearer JWT in the Authorization NATS header.
 */
export interface ListWorkflowInstanceSummariesRequest {
    /**
     * @brief Maximum number of instances to return (default 200, max 1000).
     */
    limit: number;
    /**
     * @brief Status filter. Empty means every status.
     *
     * Valid values: "in_progress", "completed", "failed",
     *               "compensating", "compensated"
     */
    status_filter: string;
    /**
     * @brief Type filter. Empty means every type.
     */
    type_filter: string;
    /**
     * @brief Kind of the entity the run acts on. Empty means any kind.
     *
     * Matched exactly and on its own, so a caller that names the kind alone gets
     * every run that acts on that kind of entity. A run with no target matches no
     * kind.
     */
    target_kind_filter: string;
    /**
     * @brief Identity of the entity the run acts on. Empty means any entity.
     *
     * A run with no target matches no identity.
     */
    target_id_filter: string;
}

export interface ListWorkflowInstanceSummariesResponse {
    success: boolean;
    message: string;
    instances: WorkflowInstanceSummary[];
}

/**
 * @brief Summary of a single step within a workflow instance.
 */
export interface WorkflowStepSummary {
    id: string;
    name: string;
    /**
     * @brief The step's name and description in a person's words, as its definition
     * declared them, and empty when it declared none.
     */
    label: string;
    description: string;
    status: string;
    step_index: number;
    created_at: string;
    started_at: string | null;
    completed_at: string | null;
    error: string;
    log: StepLogEntry[];
}

/**
 * @brief Request to retrieve all steps for a specific workflow instance.
 *
 * The instance must belong to the authenticated tenant; the handler returns
 * an error if the instance is owned by a different tenant.
 * Requires a valid Bearer JWT in the Authorization NATS header.
 */
export interface GetWorkflowStepsRequest {
    workflow_instance_id: string;
}

export interface GetWorkflowStepsResponse {
    success: boolean;
    message: string;
    /**
     * @brief Instance-level status: in_progress, completed, failed,
     * compensating, or compensated.
     *
     * A failed instance may have zero steps (e.g. a start that died before
     * persisting its first step), so callers must consult status/error rather
     * than inferring failure from the step list alone.
     */
    status: string;
    /**
     * @brief Instance-level error message, populated when status is failed
     * (or during compensation).
     */
    error: string;
    /**
     * @brief Total step count of the instance (may exceed steps.size() when
     * not all steps have been created yet).
     */
    step_count: number;
    /**
     * @brief Zero-based index of the step currently being executed.
     */
    current_step_index: number;
    steps: WorkflowStepSummary[];
}

/**
 * @brief Summary of a single step within a workflow definition.
 */
export interface WorkflowStepDefinitionSummary {
    step_index: number;
    name: string;
    description: string;
    command_subject: string;
    has_compensation: boolean;
}

/**
 * @brief Summary of a workflow definition registered in the engine.
 */
export interface WorkflowDefinitionSummary {
    type_name: string;
    description: string;
    step_count: number;
    steps: WorkflowStepDefinitionSummary[];
}

/**
 * @brief Request to list all registered workflow definitions.
 *
 * Returns the type name, description, and step metadata for every
 * workflow type known to the engine. No authentication required — the
 * definitions are public metadata.
 */
export interface ListWorkflowDefinitionsRequest {}

export interface ListWorkflowDefinitionsResponse {
    success: boolean;
    message: string;
    definitions: WorkflowDefinitionSummary[];
}

/**
 * @brief Asks the engine to resume a stopped run from the step that failed.
 *
 * A retry is the second half of the failure policy the workflow definition
 * declares: a failure stops the run and keeps every completed step, and a
 * retry re-dispatches the step that failed so the run can finish.
 */
export interface RetryWorkflowInstanceRequest {
    /**
     * @brief The run to resume.
     */
    workflow_instance_id: string;
    /**
     * @brief The step to resume from, by the name the definition gave it.
     *
     * Empty means the step that failed, which is what a page that has just
     * rendered a stopped run asks for: it holds the whole step list from the
     * progress read and names one only when a person chooses to resume from
     * somewhere other than where the run stopped.
     */
    step_name: string;
}

export interface RetryWorkflowInstanceResponse {
    success: boolean;
    /**
     * @brief Why the run was not resumed, or empty when it was.
     *
     * A refusal names what it refused: a run that is not stopped, a step the
     * run does not hold, or a step whose predecessors have not all completed.
     */
    message: string;
    /**
     * @brief The run the answer is about, echoed from the request.
     */
    workflow_instance_id: string;
    /**
     * @brief The index of the step the engine re-dispatched, or -1 on refusal.
     */
    step_index: number;
    /**
     * @brief The name of the step the engine re-dispatched, or empty on refusal.
     */
    step_name: string;
}

export const subjects = {
    step_completed_event: 'workflow.v1.events.step-completed',
    start_workflow_message: 'workflow.v1.start',
    get_step_result_request: 'workflow.v1.steps.get-result',
    list_workflow_instance_summaries_request: 'workflow.v1.instances.list',
    get_workflow_steps_request: 'workflow.v1.instances.steps',
    list_workflow_definitions_request: 'workflow.v1.definitions.list',
    retry_workflow_instance_request: 'workflow.v1.instances.retry',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    step_completed_event: true,
    start_workflow_message: true,
    get_step_result_request: true,
    list_workflow_instance_summaries_request: true,
    get_workflow_steps_request: true,
    list_workflow_definitions_request: true,
    retry_workflow_instance_request: true,
} as const;
