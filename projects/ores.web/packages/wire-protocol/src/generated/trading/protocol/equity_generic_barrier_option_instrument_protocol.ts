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
import type { EquityGenericBarrierOptionInstrument } from '../domain/equity_generic_barrier_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityGenericBarrierOptionInstrumentKey {
    trade_id: string;
}

export interface EquityGenericBarrierOptionInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    pay_currency: string;
    quantity: number | null;
    strike: string | null;
    amount: string | null;
    settlement_date: string | null;
    settlement_lag: string | null;
    settlement_calendar: string | null;
    settlement_convention: string | null;
    barrier_start_date: string | null;
    barrier_end_date: string | null;
    kiko_type: string | null;
    description: string;
}

export interface EquityGenericBarrierOptionInstrumentChange {
    write: EquityGenericBarrierOptionInstrumentWrite;
    precondition: Precondition;
}

export interface EquityGenericBarrierOptionInstrumentRemoval {
    key: EquityGenericBarrierOptionInstrumentKey;
    precondition: Precondition;
}

export interface EquityGenericBarrierOptionInstrumentLookup {
    key: EquityGenericBarrierOptionInstrumentKey;
    equity_generic_barrier_option_instrument: EquityGenericBarrierOptionInstrument | null;
}

export interface EquityGenericBarrierOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityGenericBarrierOptionInstrumentEvent {
    event_id: string;
    key: EquityGenericBarrierOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityGenericBarrierOptionInstrumentVersionKey {
    equity_generic_barrier_option_instrument: EquityGenericBarrierOptionInstrumentKey;
    version: number;
}

export interface EquityGenericBarrierOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityGenericBarrierOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityGenericBarrierOptionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquityGenericBarrierOptionInstrumentsResponse {
    result: Result;
    equity_generic_barrier_option_instruments: EquityGenericBarrierOptionInstrument[];
    total: number;
}

export interface GetEquityGenericBarrierOptionInstrumentRequest {
    key: EquityGenericBarrierOptionInstrumentKey;
}

export interface GetEquityGenericBarrierOptionInstrumentResponse {
    result: Result;
    equity_generic_barrier_option_instrument: EquityGenericBarrierOptionInstrument | null;
}

export interface GetManyEquityGenericBarrierOptionInstrumentsRequest {
    keys: EquityGenericBarrierOptionInstrumentKey[];
}

export interface GetManyEquityGenericBarrierOptionInstrumentsResponse {
    result: Result;
    entries: EquityGenericBarrierOptionInstrumentLookup[];
}

export interface PutEquityGenericBarrierOptionInstrumentRequest {
    change: EquityGenericBarrierOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityGenericBarrierOptionInstrumentResponse {
    result: Result;
    equity_generic_barrier_option_instrument: EquityGenericBarrierOptionInstrument | null;
}

export interface PutManyEquityGenericBarrierOptionInstrumentsRequest {
    changes: EquityGenericBarrierOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityGenericBarrierOptionInstrumentsResponse {
    result: Result;
    equity_generic_barrier_option_instruments: EquityGenericBarrierOptionInstrument[];
}

export interface DeleteEquityGenericBarrierOptionInstrumentRequest {
    removal: EquityGenericBarrierOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityGenericBarrierOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityGenericBarrierOptionInstrumentsRequest {
    removals: EquityGenericBarrierOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityGenericBarrierOptionInstrumentsResponse {
    result: Result;
}

export interface ListEquityGenericBarrierOptionInstrumentVersionsRequest {
    key: EquityGenericBarrierOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityGenericBarrierOptionInstrumentVersionsFilter | null;
}

export interface ListEquityGenericBarrierOptionInstrumentVersionsResponse {
    result: Result;
    versions: EquityGenericBarrierOptionInstrument[];
    total: number;
}

export interface GetEquityGenericBarrierOptionInstrumentVersionRequest {
    key: EquityGenericBarrierOptionInstrumentVersionKey;
}

export interface GetEquityGenericBarrierOptionInstrumentVersionResponse {
    result: Result;
    version: EquityGenericBarrierOptionInstrument | null;
}

export const subjects = {
    list_equity_generic_barrier_option_instruments_request:
        'trading.v1.equity_generic_barrier_option_instruments.list',
    get_equity_generic_barrier_option_instrument_request:
        'trading.v1.equity_generic_barrier_option_instruments.get',
    get_many_equity_generic_barrier_option_instruments_request:
        'trading.v1.equity_generic_barrier_option_instruments.get_many',
    put_equity_generic_barrier_option_instrument_request:
        'trading.v1.equity_generic_barrier_option_instruments.put',
    put_many_equity_generic_barrier_option_instruments_request:
        'trading.v1.equity_generic_barrier_option_instruments.put_many',
    delete_equity_generic_barrier_option_instrument_request:
        'trading.v1.equity_generic_barrier_option_instruments.delete',
    delete_many_equity_generic_barrier_option_instruments_request:
        'trading.v1.equity_generic_barrier_option_instruments.delete_many',
    list_equity_generic_barrier_option_instrument_versions_request:
        'trading.v1.equity_generic_barrier_option_instruments_versions.list',
    get_equity_generic_barrier_option_instrument_version_request:
        'trading.v1.equity_generic_barrier_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_generic_barrier_option_instruments_request: true,
    get_equity_generic_barrier_option_instrument_request: true,
    get_many_equity_generic_barrier_option_instruments_request: true,
    put_equity_generic_barrier_option_instrument_request: true,
    put_many_equity_generic_barrier_option_instruments_request: true,
    delete_equity_generic_barrier_option_instrument_request: true,
    delete_many_equity_generic_barrier_option_instruments_request: true,
    list_equity_generic_barrier_option_instrument_versions_request: true,
    get_equity_generic_barrier_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.equity_generic_barrier_option_instruments.created',
    updated: 'trading.v1.events.equity_generic_barrier_option_instruments.updated',
    deleted: 'trading.v1.events.equity_generic_barrier_option_instruments.deleted',
} as const;
