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
import type { EquityPositionOptionUnderlying } from '../domain/equity_position_option_underlying.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityPositionOptionUnderlyingKey {
    trade_id: string;
    sequence_number: number;
}

export interface EquityPositionOptionUnderlyingWrite {
    trade_id: string;
    sequence_number: number;
    trade_activity_id: string;
    underlying_name: string;
    strike: string;
    weight: number | null;
    long_short: string;
    option_type: string;
    exercise_type: string;
    settlement_type: string;
}

export interface EquityPositionOptionUnderlyingChange {
    write: EquityPositionOptionUnderlyingWrite;
    precondition: Precondition;
}

export interface EquityPositionOptionUnderlyingRemoval {
    key: EquityPositionOptionUnderlyingKey;
    precondition: Precondition;
}

export interface EquityPositionOptionUnderlyingLookup {
    key: EquityPositionOptionUnderlyingKey;
    equity_position_option_underlying: EquityPositionOptionUnderlying | null;
}

export interface EquityPositionOptionUnderlyingEvent {
    event_id: string;
    key: EquityPositionOptionUnderlyingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityPositionOptionUnderlyingVersionKey {
    equity_position_option_underlying: EquityPositionOptionUnderlyingKey;
    version: number;
}

export interface EquityPositionOptionUnderlyingVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityPositionOptionUnderlyingsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListEquityPositionOptionUnderlyingsResponse {
    result: Result;
    equity_position_option_underlyings: EquityPositionOptionUnderlying[];
    total: number;
}

export interface GetEquityPositionOptionUnderlyingRequest {
    key: EquityPositionOptionUnderlyingKey;
}

export interface GetEquityPositionOptionUnderlyingResponse {
    result: Result;
    equity_position_option_underlying: EquityPositionOptionUnderlying | null;
}

export interface GetManyEquityPositionOptionUnderlyingsRequest {
    keys: EquityPositionOptionUnderlyingKey[];
}

export interface GetManyEquityPositionOptionUnderlyingsResponse {
    result: Result;
    entries: EquityPositionOptionUnderlyingLookup[];
}

export interface PutEquityPositionOptionUnderlyingRequest {
    change: EquityPositionOptionUnderlyingChange;
    intent: ChangeIntent;
}

export interface PutEquityPositionOptionUnderlyingResponse {
    result: Result;
    equity_position_option_underlying: EquityPositionOptionUnderlying | null;
}

export interface PutManyEquityPositionOptionUnderlyingsRequest {
    changes: EquityPositionOptionUnderlyingChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityPositionOptionUnderlyingsResponse {
    result: Result;
    equity_position_option_underlyings: EquityPositionOptionUnderlying[];
}

export interface DeleteEquityPositionOptionUnderlyingRequest {
    removal: EquityPositionOptionUnderlyingRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityPositionOptionUnderlyingResponse {
    result: Result;
}

export interface DeleteManyEquityPositionOptionUnderlyingsRequest {
    removals: EquityPositionOptionUnderlyingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityPositionOptionUnderlyingsResponse {
    result: Result;
}

export interface ListEquityPositionOptionUnderlyingVersionsRequest {
    key: EquityPositionOptionUnderlyingKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityPositionOptionUnderlyingVersionsFilter | null;
}

export interface ListEquityPositionOptionUnderlyingVersionsResponse {
    result: Result;
    versions: EquityPositionOptionUnderlying[];
    total: number;
}

export interface GetEquityPositionOptionUnderlyingVersionRequest {
    key: EquityPositionOptionUnderlyingVersionKey;
}

export interface GetEquityPositionOptionUnderlyingVersionResponse {
    result: Result;
    version: EquityPositionOptionUnderlying | null;
}

export const subjects = {
    list_equity_position_option_underlyings_request:
        'trading.v1.equity_position_option_underlyings.list',
    get_equity_position_option_underlying_request:
        'trading.v1.equity_position_option_underlyings.get',
    get_many_equity_position_option_underlyings_request:
        'trading.v1.equity_position_option_underlyings.get_many',
    put_equity_position_option_underlying_request:
        'trading.v1.equity_position_option_underlyings.put',
    put_many_equity_position_option_underlyings_request:
        'trading.v1.equity_position_option_underlyings.put_many',
    delete_equity_position_option_underlying_request:
        'trading.v1.equity_position_option_underlyings.delete',
    delete_many_equity_position_option_underlyings_request:
        'trading.v1.equity_position_option_underlyings.delete_many',
    list_equity_position_option_underlying_versions_request:
        'trading.v1.equity_position_option_underlyings_versions.list',
    get_equity_position_option_underlying_version_request:
        'trading.v1.equity_position_option_underlyings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_position_option_underlyings_request: true,
    get_equity_position_option_underlying_request: true,
    get_many_equity_position_option_underlyings_request: true,
    put_equity_position_option_underlying_request: true,
    put_many_equity_position_option_underlyings_request: true,
    delete_equity_position_option_underlying_request: true,
    delete_many_equity_position_option_underlyings_request: true,
    list_equity_position_option_underlying_versions_request: true,
    get_equity_position_option_underlying_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_position_option_underlyings_events.created',
    updated: 'trading.v1.equity_position_option_underlyings_events.updated',
    deleted: 'trading.v1.equity_position_option_underlyings_events.deleted',
} as const;
