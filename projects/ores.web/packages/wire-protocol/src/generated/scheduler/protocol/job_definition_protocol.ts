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
import type { JobDefinition } from '../domain/job_definition.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface JobDefinitionKey {
    id: string;
}

export interface JobDefinitionWrite {
    id: string;
    job_name: string;
    description: string;
    command: string;
    schedule_expression: string;
    action_type: string;
    action_payload: string;
    is_active: boolean;
}

export interface JobDefinitionChange {
    write: JobDefinitionWrite;
    precondition: Precondition;
}

export interface JobDefinitionRemoval {
    key: JobDefinitionKey;
    precondition: Precondition;
}

export interface JobDefinitionLookup {
    key: JobDefinitionKey;
    job_definition: JobDefinition | null;
}

export interface JobDefinitionsFilter {
    id_one_of: string[] | null;
}

export interface JobDefinitionEvent {
    event_id: string;
    key: JobDefinitionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface JobDefinitionVersionKey {
    job_definition: JobDefinitionKey;
    version: number;
}

export interface JobDefinitionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListJobDefinitionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: JobDefinitionsFilter | null;
    as_of: string | null;
}

export interface ListJobDefinitionsResponse {
    result: Result;
    definitions: JobDefinition[];
    total: number;
}

export interface GetJobDefinitionRequest {
    key: JobDefinitionKey;
}

export interface GetJobDefinitionResponse {
    result: Result;
    job_definition: JobDefinition | null;
}

export interface GetManyJobDefinitionsRequest {
    keys: JobDefinitionKey[];
}

export interface GetManyJobDefinitionsResponse {
    result: Result;
    entries: JobDefinitionLookup[];
}

export interface PutJobDefinitionRequest {
    change: JobDefinitionChange;
    intent: ChangeIntent;
}

export interface PutJobDefinitionResponse {
    result: Result;
    job_definition: JobDefinition | null;
}

export interface PutManyJobDefinitionsRequest {
    changes: JobDefinitionChange[];
    intent: ChangeIntent;
}

export interface PutManyJobDefinitionsResponse {
    result: Result;
    definitions: JobDefinition[];
}

export interface DeleteJobDefinitionRequest {
    removal: JobDefinitionRemoval;
    intent: ChangeIntent;
}

export interface DeleteJobDefinitionResponse {
    result: Result;
}

export interface DeleteManyJobDefinitionsRequest {
    removals: JobDefinitionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyJobDefinitionsResponse {
    result: Result;
}

export interface ListJobDefinitionVersionsRequest {
    key: JobDefinitionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: JobDefinitionVersionsFilter | null;
}

export interface ListJobDefinitionVersionsResponse {
    result: Result;
    versions: JobDefinition[];
    total: number;
}

export interface GetJobDefinitionVersionRequest {
    key: JobDefinitionVersionKey;
}

export interface GetJobDefinitionVersionResponse {
    result: Result;
    version: JobDefinition | null;
}

export const subjects = {
    list_job_definitions_request: 'scheduler.v1.job_definitions.list',
    get_job_definition_request: 'scheduler.v1.job_definitions.get',
    get_many_job_definitions_request: 'scheduler.v1.job_definitions.get_many',
    put_job_definition_request: 'scheduler.v1.job_definitions.put',
    put_many_job_definitions_request: 'scheduler.v1.job_definitions.put_many',
    delete_job_definition_request: 'scheduler.v1.job_definitions.delete',
    delete_many_job_definitions_request: 'scheduler.v1.job_definitions.delete_many',
    list_job_definition_versions_request: 'scheduler.v1.job_definitions_versions.list',
    get_job_definition_version_request: 'scheduler.v1.job_definitions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_job_definitions_request: true,
    get_job_definition_request: true,
    get_many_job_definitions_request: true,
    put_job_definition_request: true,
    put_many_job_definitions_request: true,
    delete_job_definition_request: true,
    delete_many_job_definitions_request: true,
    list_job_definition_versions_request: true,
    get_job_definition_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'scheduler.v1.job_definitions_events.created',
    updated: 'scheduler.v1.job_definitions_events.updated',
    deleted: 'scheduler.v1.job_definitions_events.deleted',
} as const;
