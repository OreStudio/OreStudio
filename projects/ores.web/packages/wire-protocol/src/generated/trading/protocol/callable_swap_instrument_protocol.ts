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
import type { CallableSwapInstrument } from '../domain/callable_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CallableSwapInstrumentKey {
    trade_id: string;
}

export interface CallableSwapInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    start_date: string;
    maturity_date: string;
    description: string;
}

export interface CallableSwapInstrumentChange {
    write: CallableSwapInstrumentWrite;
    precondition: Precondition;
}

export interface CallableSwapInstrumentRemoval {
    key: CallableSwapInstrumentKey;
    precondition: Precondition;
}

export interface CallableSwapInstrumentLookup {
    key: CallableSwapInstrumentKey;
    callable_swap_instrument: CallableSwapInstrument | null;
}

export interface CallableSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface CallableSwapInstrumentEvent {
    event_id: string;
    key: CallableSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CallableSwapInstrumentVersionKey {
    callable_swap_instrument: CallableSwapInstrumentKey;
    version: number;
}

export interface CallableSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCallableSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CallableSwapInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListCallableSwapInstrumentsResponse {
    result: Result;
    callable_swap_instruments: CallableSwapInstrument[];
    total: number;
}

export interface GetCallableSwapInstrumentRequest {
    key: CallableSwapInstrumentKey;
}

export interface GetCallableSwapInstrumentResponse {
    result: Result;
    callable_swap_instrument: CallableSwapInstrument | null;
}

export interface GetManyCallableSwapInstrumentsRequest {
    keys: CallableSwapInstrumentKey[];
}

export interface GetManyCallableSwapInstrumentsResponse {
    result: Result;
    entries: CallableSwapInstrumentLookup[];
}

export interface PutCallableSwapInstrumentRequest {
    change: CallableSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutCallableSwapInstrumentResponse {
    result: Result;
    callable_swap_instrument: CallableSwapInstrument | null;
}

export interface PutManyCallableSwapInstrumentsRequest {
    changes: CallableSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyCallableSwapInstrumentsResponse {
    result: Result;
    callable_swap_instruments: CallableSwapInstrument[];
}

export interface DeleteCallableSwapInstrumentRequest {
    removal: CallableSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteCallableSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyCallableSwapInstrumentsRequest {
    removals: CallableSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCallableSwapInstrumentsResponse {
    result: Result;
}

export interface ListCallableSwapInstrumentVersionsRequest {
    key: CallableSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CallableSwapInstrumentVersionsFilter | null;
}

export interface ListCallableSwapInstrumentVersionsResponse {
    result: Result;
    versions: CallableSwapInstrument[];
    total: number;
}

export interface GetCallableSwapInstrumentVersionRequest {
    key: CallableSwapInstrumentVersionKey;
}

export interface GetCallableSwapInstrumentVersionResponse {
    result: Result;
    version: CallableSwapInstrument | null;
}

export const subjects = {
    list_callable_swap_instruments_request: 'trading.v1.callable_swap_instruments.list',
    get_callable_swap_instrument_request: 'trading.v1.callable_swap_instruments.get',
    get_many_callable_swap_instruments_request: 'trading.v1.callable_swap_instruments.get_many',
    put_callable_swap_instrument_request: 'trading.v1.callable_swap_instruments.put',
    put_many_callable_swap_instruments_request: 'trading.v1.callable_swap_instruments.put_many',
    delete_callable_swap_instrument_request: 'trading.v1.callable_swap_instruments.delete',
    delete_many_callable_swap_instruments_request:
        'trading.v1.callable_swap_instruments.delete_many',
    list_callable_swap_instrument_versions_request:
        'trading.v1.callable_swap_instruments_versions.list',
    get_callable_swap_instrument_version_request:
        'trading.v1.callable_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_callable_swap_instruments_request: true,
    get_callable_swap_instrument_request: true,
    get_many_callable_swap_instruments_request: true,
    put_callable_swap_instrument_request: true,
    put_many_callable_swap_instruments_request: true,
    delete_callable_swap_instrument_request: true,
    delete_many_callable_swap_instruments_request: true,
    list_callable_swap_instrument_versions_request: true,
    get_callable_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.callable_swap_instruments_events.created',
    updated: 'trading.v1.callable_swap_instruments_events.updated',
    deleted: 'trading.v1.callable_swap_instruments_events.deleted',
} as const;
