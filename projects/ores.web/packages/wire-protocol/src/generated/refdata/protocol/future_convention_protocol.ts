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
import type { FutureConvention } from '../domain/future_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FutureConventionKey {
    id: string;
}

export interface FutureConventionWrite {
    id: string;
    index: string;
    date_generation_rule: string | null;
    netting_type: string | null;
    calendar: string | null;
    overnight_index_tenor: string | null;
}

export interface FutureConventionChange {
    write: FutureConventionWrite;
    precondition: Precondition;
}

export interface FutureConventionRemoval {
    key: FutureConventionKey;
    precondition: Precondition;
}

export interface FutureConventionLookup {
    key: FutureConventionKey;
    future_convention: FutureConvention | null;
}

export interface FutureConventionsFilter {
    id_one_of: string[] | null;
}

export interface FutureConventionEvent {
    event_id: string;
    key: FutureConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FutureConventionVersionKey {
    future_convention: FutureConventionKey;
    version: number;
}

export interface FutureConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFutureConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FutureConventionsFilter | null;
    as_of: string | null;
}

export interface ListFutureConventionsResponse {
    result: Result;
    future_conventions: FutureConvention[];
    total: number;
}

export interface GetFutureConventionRequest {
    key: FutureConventionKey;
}

export interface GetFutureConventionResponse {
    result: Result;
    future_convention: FutureConvention | null;
}

export interface GetManyFutureConventionsRequest {
    keys: FutureConventionKey[];
}

export interface GetManyFutureConventionsResponse {
    result: Result;
    entries: FutureConventionLookup[];
}

export interface PutFutureConventionRequest {
    change: FutureConventionChange;
    intent: ChangeIntent;
}

export interface PutFutureConventionResponse {
    result: Result;
    future_convention: FutureConvention | null;
}

export interface PutManyFutureConventionsRequest {
    changes: FutureConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyFutureConventionsResponse {
    result: Result;
    future_conventions: FutureConvention[];
}

export interface DeleteFutureConventionRequest {
    removal: FutureConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteFutureConventionResponse {
    result: Result;
}

export interface DeleteManyFutureConventionsRequest {
    removals: FutureConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFutureConventionsResponse {
    result: Result;
}

export interface ListFutureConventionVersionsRequest {
    key: FutureConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FutureConventionVersionsFilter | null;
}

export interface ListFutureConventionVersionsResponse {
    result: Result;
    versions: FutureConvention[];
    total: number;
}

export interface GetFutureConventionVersionRequest {
    key: FutureConventionVersionKey;
}

export interface GetFutureConventionVersionResponse {
    result: Result;
    version: FutureConvention | null;
}

export const subjects = {
    list_future_conventions_request: 'refdata.v1.future_conventions.list',
    get_future_convention_request: 'refdata.v1.future_conventions.get',
    get_many_future_conventions_request: 'refdata.v1.future_conventions.get_many',
    put_future_convention_request: 'refdata.v1.future_conventions.put',
    put_many_future_conventions_request: 'refdata.v1.future_conventions.put_many',
    delete_future_convention_request: 'refdata.v1.future_conventions.delete',
    delete_many_future_conventions_request: 'refdata.v1.future_conventions.delete_many',
    list_future_convention_versions_request: 'refdata.v1.future_conventions_versions.list',
    get_future_convention_version_request: 'refdata.v1.future_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_future_conventions_request: true,
    get_future_convention_request: true,
    get_many_future_conventions_request: true,
    put_future_convention_request: true,
    put_many_future_conventions_request: true,
    delete_future_convention_request: true,
    delete_many_future_conventions_request: true,
    list_future_convention_versions_request: true,
    get_future_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.future_conventions_events.created',
    updated: 'refdata.v1.future_conventions_events.updated',
    deleted: 'refdata.v1.future_conventions_events.deleted',
} as const;
