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
import type { WorkflowInstance } from '../domain/workflow_instance.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface WorkflowInstanceKey {
    id: string;
}

export interface WorkflowInstanceWrite {
    id: string;
    type: string;
    target_kind: string;
    target_id: string;
    state_id: string;
    request_json: string;
    result_json: string;
    error: string;
    correlation_id: string;
    created_by: string;
    current_step_index: number;
    step_count: number;
    materialised_steps_json: string;
    completed_at: string | null;
    last_event_at: string | null;
}

export interface WorkflowInstanceChange {
    write: WorkflowInstanceWrite;
    precondition: Precondition;
}

export interface WorkflowInstanceRemoval {
    key: WorkflowInstanceKey;
    precondition: Precondition;
}

export interface WorkflowInstanceLookup {
    key: WorkflowInstanceKey;
    workflow_instance: WorkflowInstance | null;
}

export interface WorkflowInstancesFilter {
    id_one_of: string[] | null;
}

export interface WorkflowInstanceEvent {
    event_id: string;
    key: WorkflowInstanceKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface WorkflowInstanceVersionKey {
    workflow_instance: WorkflowInstanceKey;
    version: number;
}

export interface WorkflowInstanceVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListWorkflowInstancesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: WorkflowInstancesFilter | null;
}

export interface ListWorkflowInstancesResponse {
    result: Result;
    instances: WorkflowInstance[];
    total: number;
}

export interface GetWorkflowInstanceRequest {
    key: WorkflowInstanceKey;
}

export interface GetWorkflowInstanceResponse {
    result: Result;
    workflow_instance: WorkflowInstance | null;
}

export interface GetManyWorkflowInstancesRequest {
    keys: WorkflowInstanceKey[];
}

export interface GetManyWorkflowInstancesResponse {
    result: Result;
    entries: WorkflowInstanceLookup[];
}

export interface PutWorkflowInstanceRequest {
    change: WorkflowInstanceChange;
    intent: ChangeIntent;
}

export interface PutWorkflowInstanceResponse {
    result: Result;
    workflow_instance: WorkflowInstance | null;
}

export interface PutManyWorkflowInstancesRequest {
    changes: WorkflowInstanceChange[];
    intent: ChangeIntent;
}

export interface PutManyWorkflowInstancesResponse {
    result: Result;
    instances: WorkflowInstance[];
}

export interface DeleteWorkflowInstanceRequest {
    removal: WorkflowInstanceRemoval;
    intent: ChangeIntent;
}

export interface DeleteWorkflowInstanceResponse {
    result: Result;
}

export interface DeleteManyWorkflowInstancesRequest {
    removals: WorkflowInstanceRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyWorkflowInstancesResponse {
    result: Result;
}

export interface ListWorkflowInstanceVersionsRequest {
    key: WorkflowInstanceKey;
    offset: number;
    limit: number;
    order: Order;
    filter: WorkflowInstanceVersionsFilter | null;
}

export interface ListWorkflowInstanceVersionsResponse {
    result: Result;
    versions: WorkflowInstance[];
    total: number;
}

export interface GetWorkflowInstanceVersionRequest {
    key: WorkflowInstanceVersionKey;
}

export interface GetWorkflowInstanceVersionResponse {
    result: Result;
    version: WorkflowInstance | null;
}

export const subjects = {
    list_workflow_instances_request: 'workflow.v1.workflow_instances.list',
    get_workflow_instance_request: 'workflow.v1.workflow_instances.get',
    get_many_workflow_instances_request: 'workflow.v1.workflow_instances.get_many',
    put_workflow_instance_request: 'workflow.v1.workflow_instances.put',
    put_many_workflow_instances_request: 'workflow.v1.workflow_instances.put_many',
    delete_workflow_instance_request: 'workflow.v1.workflow_instances.delete',
    delete_many_workflow_instances_request: 'workflow.v1.workflow_instances.delete_many',
    list_workflow_instance_versions_request: 'workflow.v1.workflow_instances_versions.list',
    get_workflow_instance_version_request: 'workflow.v1.workflow_instances_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_workflow_instances_request: true,
    get_workflow_instance_request: true,
    get_many_workflow_instances_request: true,
    put_workflow_instance_request: true,
    put_many_workflow_instances_request: true,
    delete_workflow_instance_request: true,
    delete_many_workflow_instances_request: true,
    list_workflow_instance_versions_request: true,
    get_workflow_instance_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'workflow.v1.workflow_instances_events.created',
    updated: 'workflow.v1.workflow_instances_events.updated',
    deleted: 'workflow.v1.workflow_instances_events.deleted',
} as const;
