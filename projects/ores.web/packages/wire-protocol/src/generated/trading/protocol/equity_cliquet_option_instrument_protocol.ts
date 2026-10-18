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
import type { EquityCliquetOptionInstrument } from '../domain/equity_cliquet_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityCliquetOptionInstrumentKey {
    trade_id: string;
}

export interface EquityCliquetOptionInstrumentWrite {
    trade_id: string;
    trade_activity_id: string;
    underlying_name: string;
    currency: string;
    notional: string;
    long_short: string;
    option_type: string;
    moneyness: number | null;
    local_cap: string | null;
    local_floor: string | null;
    global_cap: string | null;
    global_floor: string | null;
    settlement_days: number | null;
    premium: string | null;
    premium_currency: string | null;
    premium_payment_date: string | null;
}

export interface EquityCliquetOptionInstrumentChange {
    write: EquityCliquetOptionInstrumentWrite;
    precondition: Precondition;
}

export interface EquityCliquetOptionInstrumentRemoval {
    key: EquityCliquetOptionInstrumentKey;
    precondition: Precondition;
}

export interface EquityCliquetOptionInstrumentLookup {
    key: EquityCliquetOptionInstrumentKey;
    equity_cliquet_option_instrument: EquityCliquetOptionInstrument | null;
}

export interface EquityCliquetOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityCliquetOptionInstrumentEvent {
    event_id: string;
    key: EquityCliquetOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityCliquetOptionInstrumentVersionKey {
    equity_cliquet_option_instrument: EquityCliquetOptionInstrumentKey;
    version: number;
}

export interface EquityCliquetOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityCliquetOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityCliquetOptionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquityCliquetOptionInstrumentsResponse {
    result: Result;
    equity_cliquet_option_instruments: EquityCliquetOptionInstrument[];
    total: number;
}

export interface GetEquityCliquetOptionInstrumentRequest {
    key: EquityCliquetOptionInstrumentKey;
}

export interface GetEquityCliquetOptionInstrumentResponse {
    result: Result;
    equity_cliquet_option_instrument: EquityCliquetOptionInstrument | null;
}

export interface GetManyEquityCliquetOptionInstrumentsRequest {
    keys: EquityCliquetOptionInstrumentKey[];
}

export interface GetManyEquityCliquetOptionInstrumentsResponse {
    result: Result;
    entries: EquityCliquetOptionInstrumentLookup[];
}

export interface PutEquityCliquetOptionInstrumentRequest {
    change: EquityCliquetOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityCliquetOptionInstrumentResponse {
    result: Result;
    equity_cliquet_option_instrument: EquityCliquetOptionInstrument | null;
}

export interface PutManyEquityCliquetOptionInstrumentsRequest {
    changes: EquityCliquetOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityCliquetOptionInstrumentsResponse {
    result: Result;
    equity_cliquet_option_instruments: EquityCliquetOptionInstrument[];
}

export interface DeleteEquityCliquetOptionInstrumentRequest {
    removal: EquityCliquetOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityCliquetOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityCliquetOptionInstrumentsRequest {
    removals: EquityCliquetOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityCliquetOptionInstrumentsResponse {
    result: Result;
}

export interface ListEquityCliquetOptionInstrumentVersionsRequest {
    key: EquityCliquetOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityCliquetOptionInstrumentVersionsFilter | null;
}

export interface ListEquityCliquetOptionInstrumentVersionsResponse {
    result: Result;
    versions: EquityCliquetOptionInstrument[];
    total: number;
}

export interface GetEquityCliquetOptionInstrumentVersionRequest {
    key: EquityCliquetOptionInstrumentVersionKey;
}

export interface GetEquityCliquetOptionInstrumentVersionResponse {
    result: Result;
    version: EquityCliquetOptionInstrument | null;
}

export const subjects = {
    list_equity_cliquet_option_instruments_request:
        'trading.v1.equity_cliquet_option_instruments.list',
    get_equity_cliquet_option_instrument_request:
        'trading.v1.equity_cliquet_option_instruments.get',
    get_many_equity_cliquet_option_instruments_request:
        'trading.v1.equity_cliquet_option_instruments.get_many',
    put_equity_cliquet_option_instrument_request:
        'trading.v1.equity_cliquet_option_instruments.put',
    put_many_equity_cliquet_option_instruments_request:
        'trading.v1.equity_cliquet_option_instruments.put_many',
    delete_equity_cliquet_option_instrument_request:
        'trading.v1.equity_cliquet_option_instruments.delete',
    delete_many_equity_cliquet_option_instruments_request:
        'trading.v1.equity_cliquet_option_instruments.delete_many',
    list_equity_cliquet_option_instrument_versions_request:
        'trading.v1.equity_cliquet_option_instruments_versions.list',
    get_equity_cliquet_option_instrument_version_request:
        'trading.v1.equity_cliquet_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_cliquet_option_instruments_request: true,
    get_equity_cliquet_option_instrument_request: true,
    get_many_equity_cliquet_option_instruments_request: true,
    put_equity_cliquet_option_instrument_request: true,
    put_many_equity_cliquet_option_instruments_request: true,
    delete_equity_cliquet_option_instrument_request: true,
    delete_many_equity_cliquet_option_instruments_request: true,
    list_equity_cliquet_option_instrument_versions_request: true,
    get_equity_cliquet_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_cliquet_option_instruments_events.created',
    updated: 'trading.v1.equity_cliquet_option_instruments_events.updated',
    deleted: 'trading.v1.equity_cliquet_option_instruments_events.deleted',
} as const;
