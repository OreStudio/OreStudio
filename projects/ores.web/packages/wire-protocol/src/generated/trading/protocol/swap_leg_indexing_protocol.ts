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
import type { SwapLegIndexing } from '../domain/swap_leg_indexing.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface SwapLegIndexingKey {
    trade_id: string;
    leg_number: number;
    sequence_number: number;
}

export interface SwapLegIndexingWrite {
    trade_id: string;
    leg_number: number;
    sequence_number: number;
    trade_activity_id: string;
    index_name: string;
    quantity: string | null;
    initial_fixing: string | null;
    fixing_days: number | null;
    fixing_calendar: string | null;
    fixing_convention: string | null;
    is_in_arrears: boolean | null;
}

export interface SwapLegIndexingChange {
    write: SwapLegIndexingWrite;
    precondition: Precondition;
}

export interface SwapLegIndexingRemoval {
    key: SwapLegIndexingKey;
    precondition: Precondition;
}

export interface SwapLegIndexingLookup {
    key: SwapLegIndexingKey;
    swap_leg_indexing: SwapLegIndexing | null;
}

export interface SwapLegIndexingsFilter {
    trade_id: string | null;
    trade_id_one_of: string[] | null;
}

export interface SwapLegIndexingEvent {
    event_id: string;
    key: SwapLegIndexingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SwapLegIndexingVersionKey {
    swap_leg_indexing: SwapLegIndexingKey;
    version: number;
}

export interface SwapLegIndexingVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSwapLegIndexingsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SwapLegIndexingsFilter | null;
    as_of: string | null;
}

export interface ListSwapLegIndexingsResponse {
    result: Result;
    swap_leg_indexings: SwapLegIndexing[];
    total: number;
}

export interface GetSwapLegIndexingRequest {
    key: SwapLegIndexingKey;
}

export interface GetSwapLegIndexingResponse {
    result: Result;
    swap_leg_indexing: SwapLegIndexing | null;
}

export interface GetManySwapLegIndexingsRequest {
    keys: SwapLegIndexingKey[];
}

export interface GetManySwapLegIndexingsResponse {
    result: Result;
    entries: SwapLegIndexingLookup[];
}

export interface PutSwapLegIndexingRequest {
    change: SwapLegIndexingChange;
    intent: ChangeIntent;
}

export interface PutSwapLegIndexingResponse {
    result: Result;
    swap_leg_indexing: SwapLegIndexing | null;
}

export interface PutManySwapLegIndexingsRequest {
    changes: SwapLegIndexingChange[];
    intent: ChangeIntent;
}

export interface PutManySwapLegIndexingsResponse {
    result: Result;
    swap_leg_indexings: SwapLegIndexing[];
}

export interface DeleteSwapLegIndexingRequest {
    removal: SwapLegIndexingRemoval;
    intent: ChangeIntent;
}

export interface DeleteSwapLegIndexingResponse {
    result: Result;
}

export interface DeleteManySwapLegIndexingsRequest {
    removals: SwapLegIndexingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySwapLegIndexingsResponse {
    result: Result;
}

export interface ListByTradeIdSwapLegIndexingsRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: SwapLegIndexingsFilter | null;
}

export interface ListByTradeIdSwapLegIndexingsResponse {
    result: Result;
    swap_leg_indexings: SwapLegIndexing[];
    total: number;
}

export interface ListSwapLegIndexingVersionsRequest {
    key: SwapLegIndexingKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SwapLegIndexingVersionsFilter | null;
}

export interface ListSwapLegIndexingVersionsResponse {
    result: Result;
    versions: SwapLegIndexing[];
    total: number;
}

export interface GetSwapLegIndexingVersionRequest {
    key: SwapLegIndexingVersionKey;
}

export interface GetSwapLegIndexingVersionResponse {
    result: Result;
    version: SwapLegIndexing | null;
}

export const subjects = {
    list_swap_leg_indexings_request: 'trading.v1.swap_leg_indexings.list',
    get_swap_leg_indexing_request: 'trading.v1.swap_leg_indexings.get',
    get_many_swap_leg_indexings_request: 'trading.v1.swap_leg_indexings.get_many',
    put_swap_leg_indexing_request: 'trading.v1.swap_leg_indexings.put',
    put_many_swap_leg_indexings_request: 'trading.v1.swap_leg_indexings.put_many',
    delete_swap_leg_indexing_request: 'trading.v1.swap_leg_indexings.delete',
    delete_many_swap_leg_indexings_request: 'trading.v1.swap_leg_indexings.delete_many',
    list_by_trade_id_swap_leg_indexings_request: 'trading.v1.swap_leg_indexings.list_by_trade_id',
    list_swap_leg_indexing_versions_request: 'trading.v1.swap_leg_indexings_versions.list',
    get_swap_leg_indexing_version_request: 'trading.v1.swap_leg_indexings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_swap_leg_indexings_request: true,
    get_swap_leg_indexing_request: true,
    get_many_swap_leg_indexings_request: true,
    put_swap_leg_indexing_request: true,
    put_many_swap_leg_indexings_request: true,
    delete_swap_leg_indexing_request: true,
    delete_many_swap_leg_indexings_request: true,
    list_by_trade_id_swap_leg_indexings_request: true,
    list_swap_leg_indexing_versions_request: true,
    get_swap_leg_indexing_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.swap_leg_indexings_events.created',
    updated: 'trading.v1.swap_leg_indexings_events.updated',
    deleted: 'trading.v1.swap_leg_indexings_events.deleted',
} as const;
