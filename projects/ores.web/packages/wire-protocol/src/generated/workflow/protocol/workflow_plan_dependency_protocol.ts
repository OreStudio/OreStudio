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
import type { WorkflowPlanDependency } from '../domain/workflow_plan_dependency.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface WorkflowPlanDependencyKey {
    id: string;
}

export interface WorkflowPlanDependencyWrite {
    id: string;
    workflow_id: string;
    consumer_step_index: number;
    producer_step_index: number;
}

export interface WorkflowPlanDependencyChange {
    write: WorkflowPlanDependencyWrite;
    precondition: Precondition;
}

export interface WorkflowPlanDependencyRemoval {
    key: WorkflowPlanDependencyKey;
    precondition: Precondition;
}

export interface WorkflowPlanDependencyLookup {
    key: WorkflowPlanDependencyKey;
    workflow_plan_dependency: WorkflowPlanDependency | null;
}

export interface WorkflowPlanDependenciesFilter {
    workflow_id: string | null;
    id_one_of: string[] | null;
    workflow_id_one_of: string[] | null;
}

export interface WorkflowPlanDependencyEvent {
    event_id: string;
    key: WorkflowPlanDependencyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface WorkflowPlanDependencyVersionKey {
    workflow_plan_dependency: WorkflowPlanDependencyKey;
    version: number;
}

export interface WorkflowPlanDependencyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListWorkflowPlanDependenciesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: WorkflowPlanDependenciesFilter | null;
    as_of: string | null;
}

export interface ListWorkflowPlanDependenciesResponse {
    result: Result;
    plan_dependencies: WorkflowPlanDependency[];
    total: number;
}

export interface GetWorkflowPlanDependencyRequest {
    key: WorkflowPlanDependencyKey;
}

export interface GetWorkflowPlanDependencyResponse {
    result: Result;
    workflow_plan_dependency: WorkflowPlanDependency | null;
}

export interface GetManyWorkflowPlanDependenciesRequest {
    keys: WorkflowPlanDependencyKey[];
}

export interface GetManyWorkflowPlanDependenciesResponse {
    result: Result;
    entries: WorkflowPlanDependencyLookup[];
}

export interface PutWorkflowPlanDependencyRequest {
    change: WorkflowPlanDependencyChange;
    intent: ChangeIntent;
}

export interface PutWorkflowPlanDependencyResponse {
    result: Result;
    workflow_plan_dependency: WorkflowPlanDependency | null;
}

export interface PutManyWorkflowPlanDependenciesRequest {
    changes: WorkflowPlanDependencyChange[];
    intent: ChangeIntent;
}

export interface PutManyWorkflowPlanDependenciesResponse {
    result: Result;
    plan_dependencies: WorkflowPlanDependency[];
}

export interface DeleteWorkflowPlanDependencyRequest {
    removal: WorkflowPlanDependencyRemoval;
    intent: ChangeIntent;
}

export interface DeleteWorkflowPlanDependencyResponse {
    result: Result;
}

export interface DeleteManyWorkflowPlanDependenciesRequest {
    removals: WorkflowPlanDependencyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyWorkflowPlanDependenciesResponse {
    result: Result;
}

export interface ListByWorkflowIdWorkflowPlanDependenciesRequest {
    workflow_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: WorkflowPlanDependenciesFilter | null;
}

export interface ListByWorkflowIdWorkflowPlanDependenciesResponse {
    result: Result;
    plan_dependencies: WorkflowPlanDependency[];
    total: number;
}

export interface ListWorkflowPlanDependencyVersionsRequest {
    key: WorkflowPlanDependencyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: WorkflowPlanDependencyVersionsFilter | null;
}

export interface ListWorkflowPlanDependencyVersionsResponse {
    result: Result;
    versions: WorkflowPlanDependency[];
    total: number;
}

export interface GetWorkflowPlanDependencyVersionRequest {
    key: WorkflowPlanDependencyVersionKey;
}

export interface GetWorkflowPlanDependencyVersionResponse {
    result: Result;
    version: WorkflowPlanDependency | null;
}

export const subjects = {
    list_workflow_plan_dependencies_request: 'workflow.v1.workflow_plan_dependencies.list',
    get_workflow_plan_dependency_request: 'workflow.v1.workflow_plan_dependencies.get',
    get_many_workflow_plan_dependencies_request: 'workflow.v1.workflow_plan_dependencies.get_many',
    put_workflow_plan_dependency_request: 'workflow.v1.workflow_plan_dependencies.put',
    put_many_workflow_plan_dependencies_request: 'workflow.v1.workflow_plan_dependencies.put_many',
    delete_workflow_plan_dependency_request: 'workflow.v1.workflow_plan_dependencies.delete',
    delete_many_workflow_plan_dependencies_request:
        'workflow.v1.workflow_plan_dependencies.delete_many',
    list_by_workflow_id_workflow_plan_dependencies_request:
        'workflow.v1.workflow_plan_dependencies.list_by_workflow_id',
    list_workflow_plan_dependency_versions_request:
        'workflow.v1.workflow_plan_dependencies_versions.list',
    get_workflow_plan_dependency_version_request:
        'workflow.v1.workflow_plan_dependencies_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_workflow_plan_dependencies_request: true,
    get_workflow_plan_dependency_request: true,
    get_many_workflow_plan_dependencies_request: true,
    put_workflow_plan_dependency_request: true,
    put_many_workflow_plan_dependencies_request: true,
    delete_workflow_plan_dependency_request: true,
    delete_many_workflow_plan_dependencies_request: true,
    list_by_workflow_id_workflow_plan_dependencies_request: true,
    list_workflow_plan_dependency_versions_request: true,
    get_workflow_plan_dependency_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'workflow.v1.workflow_plan_dependencies_events.created',
    updated: 'workflow.v1.workflow_plan_dependencies_events.updated',
    deleted: 'workflow.v1.workflow_plan_dependencies_events.deleted',
} as const;
