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
import type { EquityVarianceSwapInstrument } from '../domain/equity_variance_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityVarianceSwapInstrumentKey {
    trade_id: string;
}

export interface EquityVarianceSwapInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    underlying_name: string;
    currency: string;
    notional: string;
    variance_strike: number;
    start_date: string;
    maturity_date: string;
    long_short: string;
    description: string;
}

export interface EquityVarianceSwapInstrumentChange {
    write: EquityVarianceSwapInstrumentWrite;
    precondition: Precondition;
}

export interface EquityVarianceSwapInstrumentRemoval {
    key: EquityVarianceSwapInstrumentKey;
    precondition: Precondition;
}

export interface EquityVarianceSwapInstrumentLookup {
    key: EquityVarianceSwapInstrumentKey;
    equity_variance_swap_instrument: EquityVarianceSwapInstrument | null;
}

export interface EquityVarianceSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityVarianceSwapInstrumentEvent {
    event_id: string;
    key: EquityVarianceSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityVarianceSwapInstrumentVersionKey {
    equity_variance_swap_instrument: EquityVarianceSwapInstrumentKey;
    version: number;
}

export interface EquityVarianceSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityVarianceSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityVarianceSwapInstrumentsFilter | null;
}

export interface ListEquityVarianceSwapInstrumentsResponse {
    result: Result;
    equity_variance_swap_instruments: EquityVarianceSwapInstrument[];
    total: number;
}

export interface GetEquityVarianceSwapInstrumentRequest {
    key: EquityVarianceSwapInstrumentKey;
}

export interface GetEquityVarianceSwapInstrumentResponse {
    result: Result;
    equity_variance_swap_instrument: EquityVarianceSwapInstrument | null;
}

export interface GetManyEquityVarianceSwapInstrumentsRequest {
    keys: EquityVarianceSwapInstrumentKey[];
}

export interface GetManyEquityVarianceSwapInstrumentsResponse {
    result: Result;
    entries: EquityVarianceSwapInstrumentLookup[];
}

export interface PutEquityVarianceSwapInstrumentRequest {
    change: EquityVarianceSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityVarianceSwapInstrumentResponse {
    result: Result;
    equity_variance_swap_instrument: EquityVarianceSwapInstrument | null;
}

export interface PutManyEquityVarianceSwapInstrumentsRequest {
    changes: EquityVarianceSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityVarianceSwapInstrumentsResponse {
    result: Result;
    equity_variance_swap_instruments: EquityVarianceSwapInstrument[];
}

export interface DeleteEquityVarianceSwapInstrumentRequest {
    removal: EquityVarianceSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityVarianceSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityVarianceSwapInstrumentsRequest {
    removals: EquityVarianceSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityVarianceSwapInstrumentsResponse {
    result: Result;
}

export interface ListEquityVarianceSwapInstrumentVersionsRequest {
    key: EquityVarianceSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityVarianceSwapInstrumentVersionsFilter | null;
}

export interface ListEquityVarianceSwapInstrumentVersionsResponse {
    result: Result;
    versions: EquityVarianceSwapInstrument[];
    total: number;
}

export interface GetEquityVarianceSwapInstrumentVersionRequest {
    key: EquityVarianceSwapInstrumentVersionKey;
}

export interface GetEquityVarianceSwapInstrumentVersionResponse {
    result: Result;
    version: EquityVarianceSwapInstrument | null;
}

export const subjects = {
    list_equity_variance_swap_instruments_request:
        'trading.v1.equity_variance_swap_instruments.list',
    get_equity_variance_swap_instrument_request: 'trading.v1.equity_variance_swap_instruments.get',
    get_many_equity_variance_swap_instruments_request:
        'trading.v1.equity_variance_swap_instruments.get_many',
    put_equity_variance_swap_instrument_request: 'trading.v1.equity_variance_swap_instruments.put',
    put_many_equity_variance_swap_instruments_request:
        'trading.v1.equity_variance_swap_instruments.put_many',
    delete_equity_variance_swap_instrument_request:
        'trading.v1.equity_variance_swap_instruments.delete',
    delete_many_equity_variance_swap_instruments_request:
        'trading.v1.equity_variance_swap_instruments.delete_many',
    list_equity_variance_swap_instrument_versions_request:
        'trading.v1.equity_variance_swap_instruments_versions.list',
    get_equity_variance_swap_instrument_version_request:
        'trading.v1.equity_variance_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_variance_swap_instruments_request: true,
    get_equity_variance_swap_instrument_request: true,
    get_many_equity_variance_swap_instruments_request: true,
    put_equity_variance_swap_instrument_request: true,
    put_many_equity_variance_swap_instruments_request: true,
    delete_equity_variance_swap_instrument_request: true,
    delete_many_equity_variance_swap_instruments_request: true,
    list_equity_variance_swap_instrument_versions_request: true,
    get_equity_variance_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_variance_swap_instruments_events.created',
    updated: 'trading.v1.equity_variance_swap_instruments_events.updated',
    deleted: 'trading.v1.equity_variance_swap_instruments_events.deleted',
} as const;
