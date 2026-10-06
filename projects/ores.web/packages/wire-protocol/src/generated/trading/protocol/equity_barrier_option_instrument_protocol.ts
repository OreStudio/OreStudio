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
import type { EquityBarrierOptionInstrument } from '../domain/equity_barrier_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityBarrierOptionInstrumentKey {
    trade_id: string;
}

export interface EquityBarrierOptionInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    underlying_name: string;
    currency: string;
    notional: string;
    option_type: string;
    strike: string;
    expiry_date: string;
    exercise_type: string;
    long_short: string;
    lower_barrier: string;
    lower_barrier_type: string;
    upper_barrier: string | null;
    upper_barrier_type: string;
    rebate: string | null;
    description: string;
}

export interface EquityBarrierOptionInstrumentChange {
    write: EquityBarrierOptionInstrumentWrite;
    precondition: Precondition;
}

export interface EquityBarrierOptionInstrumentRemoval {
    key: EquityBarrierOptionInstrumentKey;
    precondition: Precondition;
}

export interface EquityBarrierOptionInstrumentLookup {
    key: EquityBarrierOptionInstrumentKey;
    equity_barrier_option_instrument: EquityBarrierOptionInstrument | null;
}

export interface EquityBarrierOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityBarrierOptionInstrumentEvent {
    event_id: string;
    key: EquityBarrierOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityBarrierOptionInstrumentVersionKey {
    equity_barrier_option_instrument: EquityBarrierOptionInstrumentKey;
    version: number;
}

export interface EquityBarrierOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityBarrierOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityBarrierOptionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquityBarrierOptionInstrumentsResponse {
    result: Result;
    equity_barrier_option_instruments: EquityBarrierOptionInstrument[];
    total: number;
}

export interface GetEquityBarrierOptionInstrumentRequest {
    key: EquityBarrierOptionInstrumentKey;
}

export interface GetEquityBarrierOptionInstrumentResponse {
    result: Result;
    equity_barrier_option_instrument: EquityBarrierOptionInstrument | null;
}

export interface GetManyEquityBarrierOptionInstrumentsRequest {
    keys: EquityBarrierOptionInstrumentKey[];
}

export interface GetManyEquityBarrierOptionInstrumentsResponse {
    result: Result;
    entries: EquityBarrierOptionInstrumentLookup[];
}

export interface PutEquityBarrierOptionInstrumentRequest {
    change: EquityBarrierOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityBarrierOptionInstrumentResponse {
    result: Result;
    equity_barrier_option_instrument: EquityBarrierOptionInstrument | null;
}

export interface PutManyEquityBarrierOptionInstrumentsRequest {
    changes: EquityBarrierOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityBarrierOptionInstrumentsResponse {
    result: Result;
    equity_barrier_option_instruments: EquityBarrierOptionInstrument[];
}

export interface DeleteEquityBarrierOptionInstrumentRequest {
    removal: EquityBarrierOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityBarrierOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityBarrierOptionInstrumentsRequest {
    removals: EquityBarrierOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityBarrierOptionInstrumentsResponse {
    result: Result;
}

export interface ListEquityBarrierOptionInstrumentVersionsRequest {
    key: EquityBarrierOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityBarrierOptionInstrumentVersionsFilter | null;
}

export interface ListEquityBarrierOptionInstrumentVersionsResponse {
    result: Result;
    versions: EquityBarrierOptionInstrument[];
    total: number;
}

export interface GetEquityBarrierOptionInstrumentVersionRequest {
    key: EquityBarrierOptionInstrumentVersionKey;
}

export interface GetEquityBarrierOptionInstrumentVersionResponse {
    result: Result;
    version: EquityBarrierOptionInstrument | null;
}

export const subjects = {
    list_equity_barrier_option_instruments_request:
        'trading.v1.equity_barrier_option_instruments.list',
    get_equity_barrier_option_instrument_request:
        'trading.v1.equity_barrier_option_instruments.get',
    get_many_equity_barrier_option_instruments_request:
        'trading.v1.equity_barrier_option_instruments.get_many',
    put_equity_barrier_option_instrument_request:
        'trading.v1.equity_barrier_option_instruments.put',
    put_many_equity_barrier_option_instruments_request:
        'trading.v1.equity_barrier_option_instruments.put_many',
    delete_equity_barrier_option_instrument_request:
        'trading.v1.equity_barrier_option_instruments.delete',
    delete_many_equity_barrier_option_instruments_request:
        'trading.v1.equity_barrier_option_instruments.delete_many',
    list_equity_barrier_option_instrument_versions_request:
        'trading.v1.equity_barrier_option_instruments_versions.list',
    get_equity_barrier_option_instrument_version_request:
        'trading.v1.equity_barrier_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_barrier_option_instruments_request: true,
    get_equity_barrier_option_instrument_request: true,
    get_many_equity_barrier_option_instruments_request: true,
    put_equity_barrier_option_instrument_request: true,
    put_many_equity_barrier_option_instruments_request: true,
    delete_equity_barrier_option_instrument_request: true,
    delete_many_equity_barrier_option_instruments_request: true,
    list_equity_barrier_option_instrument_versions_request: true,
    get_equity_barrier_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_barrier_option_instruments_events.created',
    updated: 'trading.v1.equity_barrier_option_instruments_events.updated',
    deleted: 'trading.v1.equity_barrier_option_instruments_events.deleted',
} as const;
