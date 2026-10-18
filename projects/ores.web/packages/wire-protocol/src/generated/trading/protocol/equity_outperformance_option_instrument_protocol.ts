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
import type { EquityOutperformanceOptionInstrument } from '../domain/equity_outperformance_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityOutperformanceOptionInstrumentKey {
    trade_id: string;
}

export interface EquityOutperformanceOptionInstrumentWrite {
    trade_id: string;
    trade_activity_id: string;
    underlying1_name: string;
    underlying2_name: string;
    currency: string;
    notional: string;
    long_short: string;
    initial_price1: string;
    initial_price2: string;
    strike_return: string;
    knock_in_price: string | null;
    knock_out_price: string | null;
    initial_price_currency1: string | null;
    initial_price_currency2: string | null;
}

export interface EquityOutperformanceOptionInstrumentChange {
    write: EquityOutperformanceOptionInstrumentWrite;
    precondition: Precondition;
}

export interface EquityOutperformanceOptionInstrumentRemoval {
    key: EquityOutperformanceOptionInstrumentKey;
    precondition: Precondition;
}

export interface EquityOutperformanceOptionInstrumentLookup {
    key: EquityOutperformanceOptionInstrumentKey;
    equity_outperformance_option_instrument: EquityOutperformanceOptionInstrument | null;
}

export interface EquityOutperformanceOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityOutperformanceOptionInstrumentEvent {
    event_id: string;
    key: EquityOutperformanceOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityOutperformanceOptionInstrumentVersionKey {
    equity_outperformance_option_instrument: EquityOutperformanceOptionInstrumentKey;
    version: number;
}

export interface EquityOutperformanceOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityOutperformanceOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityOutperformanceOptionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquityOutperformanceOptionInstrumentsResponse {
    result: Result;
    equity_outperformance_option_instruments: EquityOutperformanceOptionInstrument[];
    total: number;
}

export interface GetEquityOutperformanceOptionInstrumentRequest {
    key: EquityOutperformanceOptionInstrumentKey;
}

export interface GetEquityOutperformanceOptionInstrumentResponse {
    result: Result;
    equity_outperformance_option_instrument: EquityOutperformanceOptionInstrument | null;
}

export interface GetManyEquityOutperformanceOptionInstrumentsRequest {
    keys: EquityOutperformanceOptionInstrumentKey[];
}

export interface GetManyEquityOutperformanceOptionInstrumentsResponse {
    result: Result;
    entries: EquityOutperformanceOptionInstrumentLookup[];
}

export interface PutEquityOutperformanceOptionInstrumentRequest {
    change: EquityOutperformanceOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityOutperformanceOptionInstrumentResponse {
    result: Result;
    equity_outperformance_option_instrument: EquityOutperformanceOptionInstrument | null;
}

export interface PutManyEquityOutperformanceOptionInstrumentsRequest {
    changes: EquityOutperformanceOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityOutperformanceOptionInstrumentsResponse {
    result: Result;
    equity_outperformance_option_instruments: EquityOutperformanceOptionInstrument[];
}

export interface DeleteEquityOutperformanceOptionInstrumentRequest {
    removal: EquityOutperformanceOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityOutperformanceOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityOutperformanceOptionInstrumentsRequest {
    removals: EquityOutperformanceOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityOutperformanceOptionInstrumentsResponse {
    result: Result;
}

export interface ListEquityOutperformanceOptionInstrumentVersionsRequest {
    key: EquityOutperformanceOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityOutperformanceOptionInstrumentVersionsFilter | null;
}

export interface ListEquityOutperformanceOptionInstrumentVersionsResponse {
    result: Result;
    versions: EquityOutperformanceOptionInstrument[];
    total: number;
}

export interface GetEquityOutperformanceOptionInstrumentVersionRequest {
    key: EquityOutperformanceOptionInstrumentVersionKey;
}

export interface GetEquityOutperformanceOptionInstrumentVersionResponse {
    result: Result;
    version: EquityOutperformanceOptionInstrument | null;
}

export const subjects = {
    list_equity_outperformance_option_instruments_request:
        'trading.v1.equity_outperformance_option_instruments.list',
    get_equity_outperformance_option_instrument_request:
        'trading.v1.equity_outperformance_option_instruments.get',
    get_many_equity_outperformance_option_instruments_request:
        'trading.v1.equity_outperformance_option_instruments.get_many',
    put_equity_outperformance_option_instrument_request:
        'trading.v1.equity_outperformance_option_instruments.put',
    put_many_equity_outperformance_option_instruments_request:
        'trading.v1.equity_outperformance_option_instruments.put_many',
    delete_equity_outperformance_option_instrument_request:
        'trading.v1.equity_outperformance_option_instruments.delete',
    delete_many_equity_outperformance_option_instruments_request:
        'trading.v1.equity_outperformance_option_instruments.delete_many',
    list_equity_outperformance_option_instrument_versions_request:
        'trading.v1.equity_outperformance_option_instruments_versions.list',
    get_equity_outperformance_option_instrument_version_request:
        'trading.v1.equity_outperformance_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_outperformance_option_instruments_request: true,
    get_equity_outperformance_option_instrument_request: true,
    get_many_equity_outperformance_option_instruments_request: true,
    put_equity_outperformance_option_instrument_request: true,
    put_many_equity_outperformance_option_instruments_request: true,
    delete_equity_outperformance_option_instrument_request: true,
    delete_many_equity_outperformance_option_instruments_request: true,
    list_equity_outperformance_option_instrument_versions_request: true,
    get_equity_outperformance_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_outperformance_option_instruments_events.created',
    updated: 'trading.v1.equity_outperformance_option_instruments_events.updated',
    deleted: 'trading.v1.equity_outperformance_option_instruments_events.deleted',
} as const;
