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
import type { WorkflowStep } from '../domain/workflow_step.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface WorkflowStepKey {
    id: string;
}

export interface WorkflowStepWrite {
    id: string;
    workflow_id: string;
    step_index: number;
    name: string;
    state_id: string;
    request_json: string;
    response_json: string;
    error: string;
    step_log_json: string;
    command_subject: string;
    command_json: string;
    command_published_at: string | null;
    idempotency_key: string;
    compensation_subject: string;
    compensation_json: string;
    started_at: string | null;
    completed_at: string | null;
}

export interface WorkflowStepChange {
    write: WorkflowStepWrite;
    precondition: Precondition;
}

export interface WorkflowStepRemoval {
    key: WorkflowStepKey;
    precondition: Precondition;
}

export interface WorkflowStepLookup {
    key: WorkflowStepKey;
    workflow_step: WorkflowStep | null;
}

export interface WorkflowStepsFilter {
    workflow_id: string | null;
    id_one_of: string[] | null;
    workflow_id_one_of: string[] | null;
}

export interface WorkflowStepEvent {
    event_id: string;
    key: WorkflowStepKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface WorkflowStepVersionKey {
    workflow_step: WorkflowStepKey;
    version: number;
}

export interface WorkflowStepVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListWorkflowStepsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: WorkflowStepsFilter | null;
}

export interface ListWorkflowStepsResponse {
    result: Result;
    steps: WorkflowStep[];
    total: number;
}

export interface GetWorkflowStepRequest {
    key: WorkflowStepKey;
}

export interface GetWorkflowStepResponse {
    result: Result;
    workflow_step: WorkflowStep | null;
}

export interface GetManyWorkflowStepsRequest {
    keys: WorkflowStepKey[];
}

export interface GetManyWorkflowStepsResponse {
    result: Result;
    entries: WorkflowStepLookup[];
}

export interface PutWorkflowStepRequest {
    change: WorkflowStepChange;
    intent: ChangeIntent;
}

export interface PutWorkflowStepResponse {
    result: Result;
    workflow_step: WorkflowStep | null;
}

export interface PutManyWorkflowStepsRequest {
    changes: WorkflowStepChange[];
    intent: ChangeIntent;
}

export interface PutManyWorkflowStepsResponse {
    result: Result;
    steps: WorkflowStep[];
}

export interface DeleteWorkflowStepRequest {
    removal: WorkflowStepRemoval;
    intent: ChangeIntent;
}

export interface DeleteWorkflowStepResponse {
    result: Result;
}

export interface DeleteManyWorkflowStepsRequest {
    removals: WorkflowStepRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyWorkflowStepsResponse {
    result: Result;
}

export interface ListByWorkflowIdWorkflowStepsRequest {
    workflow_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: WorkflowStepsFilter | null;
}

export interface ListByWorkflowIdWorkflowStepsResponse {
    result: Result;
    steps: WorkflowStep[];
    total: number;
}

export interface ListWorkflowStepVersionsRequest {
    key: WorkflowStepKey;
    offset: number;
    limit: number;
    order: Order;
    filter: WorkflowStepVersionsFilter | null;
}

export interface ListWorkflowStepVersionsResponse {
    result: Result;
    versions: WorkflowStep[];
    total: number;
}

export interface GetWorkflowStepVersionRequest {
    key: WorkflowStepVersionKey;
}

export interface GetWorkflowStepVersionResponse {
    result: Result;
    version: WorkflowStep | null;
}

export const subjects = {
    list_workflow_steps_request: 'workflow.v1.workflow_steps.list',
    get_workflow_step_request: 'workflow.v1.workflow_steps.get',
    get_many_workflow_steps_request: 'workflow.v1.workflow_steps.get_many',
    put_workflow_step_request: 'workflow.v1.workflow_steps.put',
    put_many_workflow_steps_request: 'workflow.v1.workflow_steps.put_many',
    delete_workflow_step_request: 'workflow.v1.workflow_steps.delete',
    delete_many_workflow_steps_request: 'workflow.v1.workflow_steps.delete_many',
    list_by_workflow_id_workflow_steps_request: 'workflow.v1.workflow_steps.list_by_workflow_id',
    list_workflow_step_versions_request: 'workflow.v1.workflow_steps_versions.list',
    get_workflow_step_version_request: 'workflow.v1.workflow_steps_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_workflow_steps_request: true,
    get_workflow_step_request: true,
    get_many_workflow_steps_request: true,
    put_workflow_step_request: true,
    put_many_workflow_steps_request: true,
    delete_workflow_step_request: true,
    delete_many_workflow_steps_request: true,
    list_by_workflow_id_workflow_steps_request: true,
    list_workflow_step_versions_request: true,
    get_workflow_step_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'workflow.v1.workflow_steps_events.created',
    updated: 'workflow.v1.workflow_steps_events.updated',
    deleted: 'workflow.v1.workflow_steps_events.deleted',
} as const;
