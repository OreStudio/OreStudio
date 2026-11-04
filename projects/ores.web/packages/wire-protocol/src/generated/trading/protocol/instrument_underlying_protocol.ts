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
import type { InstrumentUnderlying } from '../domain/instrument_underlying.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface InstrumentUnderlyingKey {
    trade_id: string;
    underlying_role: string;
    sequence_number: number;
}

export interface InstrumentUnderlyingWrite {
    trade_id: string;
    underlying_role: string;
    sequence_number: number;
    trade_activity_id: string;
    underlying_type: string;
    name: string;
    identifier_type: string | null;
    currency: string | null;
    exchange: string | null;
    weight: string | null;
    price_type: string | null;
    future_month_offset: number | null;
    delivery_roll_days: number | null;
    delivery_roll_calendar: string | null;
    future_expiry_date: string | null;
    future_contract_month: string | null;
    interpolation: string | null;
    bid_ask_adjustment: string | null;
}

export interface InstrumentUnderlyingChange {
    write: InstrumentUnderlyingWrite;
    precondition: Precondition;
}

export interface InstrumentUnderlyingRemoval {
    key: InstrumentUnderlyingKey;
    precondition: Precondition;
}

export interface InstrumentUnderlyingLookup {
    key: InstrumentUnderlyingKey;
    instrument_underlying: InstrumentUnderlying | null;
}

export interface InstrumentUnderlyingsFilter {
    trade_id: string | null;
    trade_id_one_of: string[] | null;
}

export interface InstrumentUnderlyingEvent {
    event_id: string;
    key: InstrumentUnderlyingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InstrumentUnderlyingVersionKey {
    instrument_underlying: InstrumentUnderlyingKey;
    version: number;
}

export interface InstrumentUnderlyingVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInstrumentUnderlyingsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentUnderlyingsFilter | null;
    as_of: string | null;
}

export interface ListInstrumentUnderlyingsResponse {
    result: Result;
    instrument_underlyings: InstrumentUnderlying[];
    total: number;
}

export interface GetInstrumentUnderlyingRequest {
    key: InstrumentUnderlyingKey;
}

export interface GetInstrumentUnderlyingResponse {
    result: Result;
    instrument_underlying: InstrumentUnderlying | null;
}

export interface GetManyInstrumentUnderlyingsRequest {
    keys: InstrumentUnderlyingKey[];
}

export interface GetManyInstrumentUnderlyingsResponse {
    result: Result;
    entries: InstrumentUnderlyingLookup[];
}

export interface PutInstrumentUnderlyingRequest {
    change: InstrumentUnderlyingChange;
    intent: ChangeIntent;
}

export interface PutInstrumentUnderlyingResponse {
    result: Result;
    instrument_underlying: InstrumentUnderlying | null;
}

export interface PutManyInstrumentUnderlyingsRequest {
    changes: InstrumentUnderlyingChange[];
    intent: ChangeIntent;
}

export interface PutManyInstrumentUnderlyingsResponse {
    result: Result;
    instrument_underlyings: InstrumentUnderlying[];
}

export interface DeleteInstrumentUnderlyingRequest {
    removal: InstrumentUnderlyingRemoval;
    intent: ChangeIntent;
}

export interface DeleteInstrumentUnderlyingResponse {
    result: Result;
}

export interface DeleteManyInstrumentUnderlyingsRequest {
    removals: InstrumentUnderlyingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInstrumentUnderlyingsResponse {
    result: Result;
}

export interface ListByTradeIdInstrumentUnderlyingsRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentUnderlyingsFilter | null;
}

export interface ListByTradeIdInstrumentUnderlyingsResponse {
    result: Result;
    instrument_underlyings: InstrumentUnderlying[];
    total: number;
}

export interface ListInstrumentUnderlyingVersionsRequest {
    key: InstrumentUnderlyingKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentUnderlyingVersionsFilter | null;
}

export interface ListInstrumentUnderlyingVersionsResponse {
    result: Result;
    versions: InstrumentUnderlying[];
    total: number;
}

export interface GetInstrumentUnderlyingVersionRequest {
    key: InstrumentUnderlyingVersionKey;
}

export interface GetInstrumentUnderlyingVersionResponse {
    result: Result;
    version: InstrumentUnderlying | null;
}

export const subjects = {
    list_instrument_underlyings_request: 'trading.v1.instrument_underlyings.list',
    get_instrument_underlying_request: 'trading.v1.instrument_underlyings.get',
    get_many_instrument_underlyings_request: 'trading.v1.instrument_underlyings.get_many',
    put_instrument_underlying_request: 'trading.v1.instrument_underlyings.put',
    put_many_instrument_underlyings_request: 'trading.v1.instrument_underlyings.put_many',
    delete_instrument_underlying_request: 'trading.v1.instrument_underlyings.delete',
    delete_many_instrument_underlyings_request: 'trading.v1.instrument_underlyings.delete_many',
    list_by_trade_id_instrument_underlyings_request:
        'trading.v1.instrument_underlyings.list_by_trade_id',
    list_instrument_underlying_versions_request: 'trading.v1.instrument_underlyings_versions.list',
    get_instrument_underlying_version_request: 'trading.v1.instrument_underlyings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_instrument_underlyings_request: true,
    get_instrument_underlying_request: true,
    get_many_instrument_underlyings_request: true,
    put_instrument_underlying_request: true,
    put_many_instrument_underlyings_request: true,
    delete_instrument_underlying_request: true,
    delete_many_instrument_underlyings_request: true,
    list_by_trade_id_instrument_underlyings_request: true,
    list_instrument_underlying_versions_request: true,
    get_instrument_underlying_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.instrument_underlyings_events.created',
    updated: 'trading.v1.instrument_underlyings_events.updated',
    deleted: 'trading.v1.instrument_underlyings_events.deleted',
} as const;
