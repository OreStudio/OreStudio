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
import type { SwapIndexConvention } from '../domain/swap_index_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SwapIndexConventionKey {
    id: string;
}

export interface SwapIndexConventionWrite {
    id: string;
    conventions: string;
    fixing_calendar: string | null;
}

export interface SwapIndexConventionChange {
    write: SwapIndexConventionWrite;
    precondition: Precondition;
}

export interface SwapIndexConventionRemoval {
    key: SwapIndexConventionKey;
    precondition: Precondition;
}

export interface SwapIndexConventionLookup {
    key: SwapIndexConventionKey;
    swap_index_convention: SwapIndexConvention | null;
}

export interface SwapIndexConventionEvent {
    event_id: string;
    key: SwapIndexConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SwapIndexConventionVersionKey {
    swap_index_convention: SwapIndexConventionKey;
    version: number;
}

export interface SwapIndexConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSwapIndexConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListSwapIndexConventionsResponse {
    result: Result;
    swap_index_conventions: SwapIndexConvention[];
    total: number;
}

export interface GetSwapIndexConventionRequest {
    key: SwapIndexConventionKey;
}

export interface GetSwapIndexConventionResponse {
    result: Result;
    swap_index_convention: SwapIndexConvention | null;
}

export interface GetManySwapIndexConventionsRequest {
    keys: SwapIndexConventionKey[];
}

export interface GetManySwapIndexConventionsResponse {
    result: Result;
    entries: SwapIndexConventionLookup[];
}

export interface PutSwapIndexConventionRequest {
    change: SwapIndexConventionChange;
    intent: ChangeIntent;
}

export interface PutSwapIndexConventionResponse {
    result: Result;
    swap_index_convention: SwapIndexConvention | null;
}

export interface PutManySwapIndexConventionsRequest {
    changes: SwapIndexConventionChange[];
    intent: ChangeIntent;
}

export interface PutManySwapIndexConventionsResponse {
    result: Result;
    swap_index_conventions: SwapIndexConvention[];
}

export interface DeleteSwapIndexConventionRequest {
    removal: SwapIndexConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteSwapIndexConventionResponse {
    result: Result;
}

export interface DeleteManySwapIndexConventionsRequest {
    removals: SwapIndexConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySwapIndexConventionsResponse {
    result: Result;
}

export interface ListSwapIndexConventionVersionsRequest {
    key: SwapIndexConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SwapIndexConventionVersionsFilter | null;
}

export interface ListSwapIndexConventionVersionsResponse {
    result: Result;
    versions: SwapIndexConvention[];
    total: number;
}

export interface GetSwapIndexConventionVersionRequest {
    key: SwapIndexConventionVersionKey;
}

export interface GetSwapIndexConventionVersionResponse {
    result: Result;
    version: SwapIndexConvention | null;
}

export const subjects = {
    list_swap_index_conventions_request: 'refdata.v1.swap_index_conventions.list',
    get_swap_index_convention_request: 'refdata.v1.swap_index_conventions.get',
    get_many_swap_index_conventions_request: 'refdata.v1.swap_index_conventions.get_many',
    put_swap_index_convention_request: 'refdata.v1.swap_index_conventions.put',
    put_many_swap_index_conventions_request: 'refdata.v1.swap_index_conventions.put_many',
    delete_swap_index_convention_request: 'refdata.v1.swap_index_conventions.delete',
    delete_many_swap_index_conventions_request: 'refdata.v1.swap_index_conventions.delete_many',
    list_swap_index_convention_versions_request: 'refdata.v1.swap_index_conventions_versions.list',
    get_swap_index_convention_version_request: 'refdata.v1.swap_index_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_swap_index_conventions_request: true,
    get_swap_index_convention_request: true,
    get_many_swap_index_conventions_request: true,
    put_swap_index_convention_request: true,
    put_many_swap_index_conventions_request: true,
    delete_swap_index_convention_request: true,
    delete_many_swap_index_conventions_request: true,
    list_swap_index_convention_versions_request: true,
    get_swap_index_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.swap_index_conventions_events.created',
    updated: 'refdata.v1.swap_index_conventions_events.updated',
    deleted: 'refdata.v1.swap_index_conventions_events.deleted',
} as const;
