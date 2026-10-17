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
import type { RangeBound } from '../domain/range_bound.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface RangeBoundKey {
    trade_id: string;
    sequence_number: number;
}

export interface RangeBoundWrite {
    trade_id: string;
    sequence_number: number;
    trade_activity_id: string;
    start_date: string | null;
    range_from: string | null;
    range_to: string | null;
    leverage: number | null;
    strike: string | null;
    strike_adjustment: string | null;
}

export interface RangeBoundChange {
    write: RangeBoundWrite;
    precondition: Precondition;
}

export interface RangeBoundRemoval {
    key: RangeBoundKey;
    precondition: Precondition;
}

export interface RangeBoundLookup {
    key: RangeBoundKey;
    range_bound: RangeBound | null;
}

export interface RangeBoundsFilter {
    trade_id: string | null;
    trade_id_one_of: string[] | null;
}

export interface RangeBoundEvent {
    event_id: string;
    key: RangeBoundKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface RangeBoundVersionKey {
    range_bound: RangeBoundKey;
    version: number;
}

export interface RangeBoundVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRangeBoundsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RangeBoundsFilter | null;
    as_of: string | null;
}

export interface ListRangeBoundsResponse {
    result: Result;
    range_bounds: RangeBound[];
    total: number;
}

export interface GetRangeBoundRequest {
    key: RangeBoundKey;
}

export interface GetRangeBoundResponse {
    result: Result;
    range_bound: RangeBound | null;
}

export interface GetManyRangeBoundsRequest {
    keys: RangeBoundKey[];
}

export interface GetManyRangeBoundsResponse {
    result: Result;
    entries: RangeBoundLookup[];
}

export interface PutRangeBoundRequest {
    change: RangeBoundChange;
    intent: ChangeIntent;
}

export interface PutRangeBoundResponse {
    result: Result;
    range_bound: RangeBound | null;
}

export interface PutManyRangeBoundsRequest {
    changes: RangeBoundChange[];
    intent: ChangeIntent;
}

export interface PutManyRangeBoundsResponse {
    result: Result;
    range_bounds: RangeBound[];
}

export interface DeleteRangeBoundRequest {
    removal: RangeBoundRemoval;
    intent: ChangeIntent;
}

export interface DeleteRangeBoundResponse {
    result: Result;
}

export interface DeleteManyRangeBoundsRequest {
    removals: RangeBoundRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRangeBoundsResponse {
    result: Result;
}

export interface ListByTradeIdRangeBoundsRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: RangeBoundsFilter | null;
}

export interface ListByTradeIdRangeBoundsResponse {
    result: Result;
    range_bounds: RangeBound[];
    total: number;
}

export interface ListRangeBoundVersionsRequest {
    key: RangeBoundKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RangeBoundVersionsFilter | null;
}

export interface ListRangeBoundVersionsResponse {
    result: Result;
    versions: RangeBound[];
    total: number;
}

export interface GetRangeBoundVersionRequest {
    key: RangeBoundVersionKey;
}

export interface GetRangeBoundVersionResponse {
    result: Result;
    version: RangeBound | null;
}

export const subjects = {
    list_range_bounds_request: 'trading.v1.range_bounds.list',
    get_range_bound_request: 'trading.v1.range_bounds.get',
    get_many_range_bounds_request: 'trading.v1.range_bounds.get_many',
    put_range_bound_request: 'trading.v1.range_bounds.put',
    put_many_range_bounds_request: 'trading.v1.range_bounds.put_many',
    delete_range_bound_request: 'trading.v1.range_bounds.delete',
    delete_many_range_bounds_request: 'trading.v1.range_bounds.delete_many',
    list_by_trade_id_range_bounds_request: 'trading.v1.range_bounds.list_by_trade_id',
    list_range_bound_versions_request: 'trading.v1.range_bounds_versions.list',
    get_range_bound_version_request: 'trading.v1.range_bounds_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_range_bounds_request: true,
    get_range_bound_request: true,
    get_many_range_bounds_request: true,
    put_range_bound_request: true,
    put_many_range_bounds_request: true,
    delete_range_bound_request: true,
    delete_many_range_bounds_request: true,
    list_by_trade_id_range_bounds_request: true,
    list_range_bound_versions_request: true,
    get_range_bound_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.range_bounds_events.created',
    updated: 'trading.v1.range_bounds_events.updated',
    deleted: 'trading.v1.range_bounds_events.deleted',
} as const;
