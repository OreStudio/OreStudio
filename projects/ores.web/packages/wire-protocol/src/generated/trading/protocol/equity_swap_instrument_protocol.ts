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
import type { EquitySwapInstrument } from '../domain/equity_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquitySwapInstrumentKey {
    trade_id: string;
}

export interface EquitySwapInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    underlying_name: string;
    basket_json: string;
    currency: string;
    notional: string;
    return_type: string;
    start_date: string;
    maturity_date: string;
    long_short: string;
    payment_frequency_code: string;
    description: string;
}

export interface EquitySwapInstrumentChange {
    write: EquitySwapInstrumentWrite;
    precondition: Precondition;
}

export interface EquitySwapInstrumentRemoval {
    key: EquitySwapInstrumentKey;
    precondition: Precondition;
}

export interface EquitySwapInstrumentLookup {
    key: EquitySwapInstrumentKey;
    equity_swap_instrument: EquitySwapInstrument | null;
}

export interface EquitySwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquitySwapInstrumentEvent {
    event_id: string;
    key: EquitySwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquitySwapInstrumentVersionKey {
    equity_swap_instrument: EquitySwapInstrumentKey;
    version: number;
}

export interface EquitySwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquitySwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquitySwapInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquitySwapInstrumentsResponse {
    result: Result;
    equity_swap_instruments: EquitySwapInstrument[];
    total: number;
}

export interface GetEquitySwapInstrumentRequest {
    key: EquitySwapInstrumentKey;
}

export interface GetEquitySwapInstrumentResponse {
    result: Result;
    equity_swap_instrument: EquitySwapInstrument | null;
}

export interface GetManyEquitySwapInstrumentsRequest {
    keys: EquitySwapInstrumentKey[];
}

export interface GetManyEquitySwapInstrumentsResponse {
    result: Result;
    entries: EquitySwapInstrumentLookup[];
}

export interface PutEquitySwapInstrumentRequest {
    change: EquitySwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquitySwapInstrumentResponse {
    result: Result;
    equity_swap_instrument: EquitySwapInstrument | null;
}

export interface PutManyEquitySwapInstrumentsRequest {
    changes: EquitySwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquitySwapInstrumentsResponse {
    result: Result;
    equity_swap_instruments: EquitySwapInstrument[];
}

export interface DeleteEquitySwapInstrumentRequest {
    removal: EquitySwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquitySwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquitySwapInstrumentsRequest {
    removals: EquitySwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquitySwapInstrumentsResponse {
    result: Result;
}

export interface ListEquitySwapInstrumentVersionsRequest {
    key: EquitySwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquitySwapInstrumentVersionsFilter | null;
}

export interface ListEquitySwapInstrumentVersionsResponse {
    result: Result;
    versions: EquitySwapInstrument[];
    total: number;
}

export interface GetEquitySwapInstrumentVersionRequest {
    key: EquitySwapInstrumentVersionKey;
}

export interface GetEquitySwapInstrumentVersionResponse {
    result: Result;
    version: EquitySwapInstrument | null;
}

export const subjects = {
    list_equity_swap_instruments_request: 'trading.v1.equity_swap_instruments.list',
    get_equity_swap_instrument_request: 'trading.v1.equity_swap_instruments.get',
    get_many_equity_swap_instruments_request: 'trading.v1.equity_swap_instruments.get_many',
    put_equity_swap_instrument_request: 'trading.v1.equity_swap_instruments.put',
    put_many_equity_swap_instruments_request: 'trading.v1.equity_swap_instruments.put_many',
    delete_equity_swap_instrument_request: 'trading.v1.equity_swap_instruments.delete',
    delete_many_equity_swap_instruments_request: 'trading.v1.equity_swap_instruments.delete_many',
    list_equity_swap_instrument_versions_request:
        'trading.v1.equity_swap_instruments_versions.list',
    get_equity_swap_instrument_version_request: 'trading.v1.equity_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_swap_instruments_request: true,
    get_equity_swap_instrument_request: true,
    get_many_equity_swap_instruments_request: true,
    put_equity_swap_instrument_request: true,
    put_many_equity_swap_instruments_request: true,
    delete_equity_swap_instrument_request: true,
    delete_many_equity_swap_instruments_request: true,
    list_equity_swap_instrument_versions_request: true,
    get_equity_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_swap_instruments_events.created',
    updated: 'trading.v1.equity_swap_instruments_events.updated',
    deleted: 'trading.v1.equity_swap_instruments_events.deleted',
} as const;
