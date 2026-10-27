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
import type { EquityUnderlying } from '../domain/equity_underlying.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityUnderlyingKey {
    code: string;
}

export interface EquityUnderlyingWrite {
    code: string;
    name: string;
    description: string;
    instrument_type: string;
    resolves_to: string;
    currency_code: string;
    display_order: number;
}

export interface EquityUnderlyingChange {
    write: EquityUnderlyingWrite;
    precondition: Precondition;
}

export interface EquityUnderlyingRemoval {
    key: EquityUnderlyingKey;
    precondition: Precondition;
}

export interface EquityUnderlyingLookup {
    key: EquityUnderlyingKey;
    equity_underlying: EquityUnderlying | null;
}

export interface EquityUnderlyingsFilter {
    code_one_of: string[] | null;
}

export interface EquityUnderlyingEvent {
    event_id: string;
    key: EquityUnderlyingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityUnderlyingVersionKey {
    equity_underlying: EquityUnderlyingKey;
    version: number;
}

export interface EquityUnderlyingVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityUnderlyingsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityUnderlyingsFilter | null;
    as_of: string | null;
}

export interface ListEquityUnderlyingsResponse {
    result: Result;
    equity_underlyings: EquityUnderlying[];
    total: number;
}

export interface GetEquityUnderlyingRequest {
    key: EquityUnderlyingKey;
}

export interface GetEquityUnderlyingResponse {
    result: Result;
    equity_underlying: EquityUnderlying | null;
}

export interface GetManyEquityUnderlyingsRequest {
    keys: EquityUnderlyingKey[];
}

export interface GetManyEquityUnderlyingsResponse {
    result: Result;
    entries: EquityUnderlyingLookup[];
}

export interface PutEquityUnderlyingRequest {
    change: EquityUnderlyingChange;
    intent: ChangeIntent;
}

export interface PutEquityUnderlyingResponse {
    result: Result;
    equity_underlying: EquityUnderlying | null;
}

export interface PutManyEquityUnderlyingsRequest {
    changes: EquityUnderlyingChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityUnderlyingsResponse {
    result: Result;
    equity_underlyings: EquityUnderlying[];
}

export interface DeleteEquityUnderlyingRequest {
    removal: EquityUnderlyingRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityUnderlyingResponse {
    result: Result;
}

export interface DeleteManyEquityUnderlyingsRequest {
    removals: EquityUnderlyingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityUnderlyingsResponse {
    result: Result;
}

export interface ListEquityUnderlyingVersionsRequest {
    key: EquityUnderlyingKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityUnderlyingVersionsFilter | null;
}

export interface ListEquityUnderlyingVersionsResponse {
    result: Result;
    versions: EquityUnderlying[];
    total: number;
}

export interface GetEquityUnderlyingVersionRequest {
    key: EquityUnderlyingVersionKey;
}

export interface GetEquityUnderlyingVersionResponse {
    result: Result;
    version: EquityUnderlying | null;
}

export const subjects = {
    list_equity_underlyings_request: 'refdata.v1.equity_underlyings.list',
    get_equity_underlying_request: 'refdata.v1.equity_underlyings.get',
    get_many_equity_underlyings_request: 'refdata.v1.equity_underlyings.get_many',
    put_equity_underlying_request: 'refdata.v1.equity_underlyings.put',
    put_many_equity_underlyings_request: 'refdata.v1.equity_underlyings.put_many',
    delete_equity_underlying_request: 'refdata.v1.equity_underlyings.delete',
    delete_many_equity_underlyings_request: 'refdata.v1.equity_underlyings.delete_many',
    list_equity_underlying_versions_request: 'refdata.v1.equity_underlyings_versions.list',
    get_equity_underlying_version_request: 'refdata.v1.equity_underlyings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_underlyings_request: true,
    get_equity_underlying_request: true,
    get_many_equity_underlyings_request: true,
    put_equity_underlying_request: true,
    put_many_equity_underlyings_request: true,
    delete_equity_underlying_request: true,
    delete_many_equity_underlyings_request: true,
    list_equity_underlying_versions_request: true,
    get_equity_underlying_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.equity_underlyings_events.created',
    updated: 'refdata.v1.equity_underlyings_events.updated',
    deleted: 'refdata.v1.equity_underlyings_events.deleted',
} as const;
