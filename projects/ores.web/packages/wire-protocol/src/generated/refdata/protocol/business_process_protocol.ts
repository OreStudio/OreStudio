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
import type { BusinessProcess } from '../domain/business_process.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BusinessProcessKey {
    code: string;
    coding_scheme_code: string;
}

export interface BusinessProcessWrite {
    code: string;
    coding_scheme_code: string;
    source: string;
    description: string;
}

export interface BusinessProcessChange {
    write: BusinessProcessWrite;
    precondition: Precondition;
}

export interface BusinessProcessRemoval {
    key: BusinessProcessKey;
    precondition: Precondition;
}

export interface BusinessProcessLookup {
    key: BusinessProcessKey;
    business_process: BusinessProcess | null;
}

export interface BusinessProcessEvent {
    event_id: string;
    key: BusinessProcessKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BusinessProcessVersionKey {
    business_process: BusinessProcessKey;
    version: number;
}

export interface BusinessProcessVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBusinessProcessesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListBusinessProcessesResponse {
    result: Result;
    processes: BusinessProcess[];
    total: number;
}

export interface GetBusinessProcessRequest {
    key: BusinessProcessKey;
}

export interface GetBusinessProcessResponse {
    result: Result;
    business_process: BusinessProcess | null;
}

export interface GetManyBusinessProcessesRequest {
    keys: BusinessProcessKey[];
}

export interface GetManyBusinessProcessesResponse {
    result: Result;
    entries: BusinessProcessLookup[];
}

export interface PutBusinessProcessRequest {
    change: BusinessProcessChange;
    intent: ChangeIntent;
}

export interface PutBusinessProcessResponse {
    result: Result;
    business_process: BusinessProcess | null;
}

export interface PutManyBusinessProcessesRequest {
    changes: BusinessProcessChange[];
    intent: ChangeIntent;
}

export interface PutManyBusinessProcessesResponse {
    result: Result;
    processes: BusinessProcess[];
}

export interface DeleteBusinessProcessRequest {
    removal: BusinessProcessRemoval;
    intent: ChangeIntent;
}

export interface DeleteBusinessProcessResponse {
    result: Result;
}

export interface DeleteManyBusinessProcessesRequest {
    removals: BusinessProcessRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBusinessProcessesResponse {
    result: Result;
}

export interface ListBusinessProcessVersionsRequest {
    key: BusinessProcessKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BusinessProcessVersionsFilter | null;
}

export interface ListBusinessProcessVersionsResponse {
    result: Result;
    versions: BusinessProcess[];
    total: number;
}

export interface GetBusinessProcessVersionRequest {
    key: BusinessProcessVersionKey;
}

export interface GetBusinessProcessVersionResponse {
    result: Result;
    version: BusinessProcess | null;
}

export const subjects = {
    list_business_processes_request: 'refdata.v1.business_processes.list',
    get_business_process_request: 'refdata.v1.business_processes.get',
    get_many_business_processes_request: 'refdata.v1.business_processes.get_many',
    put_business_process_request: 'refdata.v1.business_processes.put',
    put_many_business_processes_request: 'refdata.v1.business_processes.put_many',
    delete_business_process_request: 'refdata.v1.business_processes.delete',
    delete_many_business_processes_request: 'refdata.v1.business_processes.delete_many',
    list_business_process_versions_request: 'refdata.v1.business_processes_versions.list',
    get_business_process_version_request: 'refdata.v1.business_processes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_business_processes_request: true,
    get_business_process_request: true,
    get_many_business_processes_request: true,
    put_business_process_request: true,
    put_many_business_processes_request: true,
    delete_business_process_request: true,
    delete_many_business_processes_request: true,
    list_business_process_versions_request: true,
    get_business_process_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.business_processes_events.created',
    updated: 'refdata.v1.business_processes_events.updated',
    deleted: 'refdata.v1.business_processes_events.deleted',
} as const;
