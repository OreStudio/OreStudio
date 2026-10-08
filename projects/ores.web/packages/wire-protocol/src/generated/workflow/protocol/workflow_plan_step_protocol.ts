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
import type { WorkflowPlanStep } from '../domain/workflow_plan_step.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface WorkflowPlanStepKey {
    id: string;
}

export interface WorkflowPlanStepWrite {
    id: string;
    workflow_id: string;
    step_index: number;
    name: string;
    label: string;
    description: string;
    command_subject: string;
    compensation_subject: string;
    timeout_seconds: number;
}

export interface WorkflowPlanStepChange {
    write: WorkflowPlanStepWrite;
    precondition: Precondition;
}

export interface WorkflowPlanStepRemoval {
    key: WorkflowPlanStepKey;
    precondition: Precondition;
}

export interface WorkflowPlanStepLookup {
    key: WorkflowPlanStepKey;
    workflow_plan_step: WorkflowPlanStep | null;
}

export interface WorkflowPlanStepsFilter {
    workflow_id: string | null;
    id_one_of: string[] | null;
    workflow_id_one_of: string[] | null;
}

export interface WorkflowPlanStepEvent {
    event_id: string;
    key: WorkflowPlanStepKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface WorkflowPlanStepVersionKey {
    workflow_plan_step: WorkflowPlanStepKey;
    version: number;
}

export interface WorkflowPlanStepVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListWorkflowPlanStepsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: WorkflowPlanStepsFilter | null;
    as_of: string | null;
}

export interface ListWorkflowPlanStepsResponse {
    result: Result;
    plan_steps: WorkflowPlanStep[];
    total: number;
}

export interface GetWorkflowPlanStepRequest {
    key: WorkflowPlanStepKey;
}

export interface GetWorkflowPlanStepResponse {
    result: Result;
    workflow_plan_step: WorkflowPlanStep | null;
}

export interface GetManyWorkflowPlanStepsRequest {
    keys: WorkflowPlanStepKey[];
}

export interface GetManyWorkflowPlanStepsResponse {
    result: Result;
    entries: WorkflowPlanStepLookup[];
}

export interface PutWorkflowPlanStepRequest {
    change: WorkflowPlanStepChange;
    intent: ChangeIntent;
}

export interface PutWorkflowPlanStepResponse {
    result: Result;
    workflow_plan_step: WorkflowPlanStep | null;
}

export interface PutManyWorkflowPlanStepsRequest {
    changes: WorkflowPlanStepChange[];
    intent: ChangeIntent;
}

export interface PutManyWorkflowPlanStepsResponse {
    result: Result;
    plan_steps: WorkflowPlanStep[];
}

export interface DeleteWorkflowPlanStepRequest {
    removal: WorkflowPlanStepRemoval;
    intent: ChangeIntent;
}

export interface DeleteWorkflowPlanStepResponse {
    result: Result;
}

export interface DeleteManyWorkflowPlanStepsRequest {
    removals: WorkflowPlanStepRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyWorkflowPlanStepsResponse {
    result: Result;
}

export interface ListByWorkflowIdWorkflowPlanStepsRequest {
    workflow_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: WorkflowPlanStepsFilter | null;
}

export interface ListByWorkflowIdWorkflowPlanStepsResponse {
    result: Result;
    plan_steps: WorkflowPlanStep[];
    total: number;
}

export interface ListWorkflowPlanStepVersionsRequest {
    key: WorkflowPlanStepKey;
    offset: number;
    limit: number;
    order: Order;
    filter: WorkflowPlanStepVersionsFilter | null;
}

export interface ListWorkflowPlanStepVersionsResponse {
    result: Result;
    versions: WorkflowPlanStep[];
    total: number;
}

export interface GetWorkflowPlanStepVersionRequest {
    key: WorkflowPlanStepVersionKey;
}

export interface GetWorkflowPlanStepVersionResponse {
    result: Result;
    version: WorkflowPlanStep | null;
}

export const subjects = {
    list_workflow_plan_steps_request: 'workflow.v1.workflow_plan_steps.list',
    get_workflow_plan_step_request: 'workflow.v1.workflow_plan_steps.get',
    get_many_workflow_plan_steps_request: 'workflow.v1.workflow_plan_steps.get_many',
    put_workflow_plan_step_request: 'workflow.v1.workflow_plan_steps.put',
    put_many_workflow_plan_steps_request: 'workflow.v1.workflow_plan_steps.put_many',
    delete_workflow_plan_step_request: 'workflow.v1.workflow_plan_steps.delete',
    delete_many_workflow_plan_steps_request: 'workflow.v1.workflow_plan_steps.delete_many',
    list_by_workflow_id_workflow_plan_steps_request:
        'workflow.v1.workflow_plan_steps.list_by_workflow_id',
    list_workflow_plan_step_versions_request: 'workflow.v1.workflow_plan_steps_versions.list',
    get_workflow_plan_step_version_request: 'workflow.v1.workflow_plan_steps_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_workflow_plan_steps_request: true,
    get_workflow_plan_step_request: true,
    get_many_workflow_plan_steps_request: true,
    put_workflow_plan_step_request: true,
    put_many_workflow_plan_steps_request: true,
    delete_workflow_plan_step_request: true,
    delete_many_workflow_plan_steps_request: true,
    list_by_workflow_id_workflow_plan_steps_request: true,
    list_workflow_plan_step_versions_request: true,
    get_workflow_plan_step_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'workflow.v1.workflow_plan_steps_events.created',
    updated: 'workflow.v1.workflow_plan_steps_events.updated',
    deleted: 'workflow.v1.workflow_plan_steps_events.deleted',
} as const;
