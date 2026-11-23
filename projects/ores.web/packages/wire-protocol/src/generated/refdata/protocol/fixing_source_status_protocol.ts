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
import type { FixingSourceStatus } from '../domain/fixing_source_status.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FixingSourceStatusKey {
    code: string;
}

export interface FixingSourceStatusWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface FixingSourceStatusChange {
    write: FixingSourceStatusWrite;
    precondition: Precondition;
}

export interface FixingSourceStatusRemoval {
    key: FixingSourceStatusKey;
    precondition: Precondition;
}

export interface FixingSourceStatusLookup {
    key: FixingSourceStatusKey;
    fixing_source_status: FixingSourceStatus | null;
}

export interface FixingSourceStatusesFilter {
    code_one_of: string[] | null;
}

export interface FixingSourceStatusEvent {
    event_id: string;
    key: FixingSourceStatusKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FixingSourceStatusVersionKey {
    fixing_source_status: FixingSourceStatusKey;
    version: number;
}

export interface FixingSourceStatusVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFixingSourceStatusesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FixingSourceStatusesFilter | null;
    as_of: string | null;
}

export interface ListFixingSourceStatusesResponse {
    result: Result;
    statuses: FixingSourceStatus[];
    total: number;
}

export interface GetFixingSourceStatusRequest {
    key: FixingSourceStatusKey;
}

export interface GetFixingSourceStatusResponse {
    result: Result;
    fixing_source_status: FixingSourceStatus | null;
}

export interface GetManyFixingSourceStatusesRequest {
    keys: FixingSourceStatusKey[];
}

export interface GetManyFixingSourceStatusesResponse {
    result: Result;
    entries: FixingSourceStatusLookup[];
}

export interface PutFixingSourceStatusRequest {
    change: FixingSourceStatusChange;
    intent: ChangeIntent;
}

export interface PutFixingSourceStatusResponse {
    result: Result;
    fixing_source_status: FixingSourceStatus | null;
}

export interface PutManyFixingSourceStatusesRequest {
    changes: FixingSourceStatusChange[];
    intent: ChangeIntent;
}

export interface PutManyFixingSourceStatusesResponse {
    result: Result;
    statuses: FixingSourceStatus[];
}

export interface DeleteFixingSourceStatusRequest {
    removal: FixingSourceStatusRemoval;
    intent: ChangeIntent;
}

export interface DeleteFixingSourceStatusResponse {
    result: Result;
}

export interface DeleteManyFixingSourceStatusesRequest {
    removals: FixingSourceStatusRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFixingSourceStatusesResponse {
    result: Result;
}

export interface ListFixingSourceStatusVersionsRequest {
    key: FixingSourceStatusKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FixingSourceStatusVersionsFilter | null;
}

export interface ListFixingSourceStatusVersionsResponse {
    result: Result;
    versions: FixingSourceStatus[];
    total: number;
}

export interface GetFixingSourceStatusVersionRequest {
    key: FixingSourceStatusVersionKey;
}

export interface GetFixingSourceStatusVersionResponse {
    result: Result;
    version: FixingSourceStatus | null;
}

export const subjects = {
    list_fixing_source_statuses_request: 'refdata.v1.fixing_source_statuses.list',
    get_fixing_source_status_request: 'refdata.v1.fixing_source_statuses.get',
    get_many_fixing_source_statuses_request: 'refdata.v1.fixing_source_statuses.get_many',
    put_fixing_source_status_request: 'refdata.v1.fixing_source_statuses.put',
    put_many_fixing_source_statuses_request: 'refdata.v1.fixing_source_statuses.put_many',
    delete_fixing_source_status_request: 'refdata.v1.fixing_source_statuses.delete',
    delete_many_fixing_source_statuses_request: 'refdata.v1.fixing_source_statuses.delete_many',
    list_fixing_source_status_versions_request: 'refdata.v1.fixing_source_statuses_versions.list',
    get_fixing_source_status_version_request: 'refdata.v1.fixing_source_statuses_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fixing_source_statuses_request: true,
    get_fixing_source_status_request: true,
    get_many_fixing_source_statuses_request: true,
    put_fixing_source_status_request: true,
    put_many_fixing_source_statuses_request: true,
    delete_fixing_source_status_request: true,
    delete_many_fixing_source_statuses_request: true,
    list_fixing_source_status_versions_request: true,
    get_fixing_source_status_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.fixing_source_statuses.created',
    updated: 'refdata.v1.events.fixing_source_statuses.updated',
    deleted: 'refdata.v1.events.fixing_source_statuses.deleted',
} as const;
