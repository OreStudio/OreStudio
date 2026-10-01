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
import type { EquityAsianOptionInstrument } from '../domain/equity_asian_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityAsianOptionInstrumentKey {
    trade_id: string;
}

export interface EquityAsianOptionInstrumentWrite {
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
    average_type: string;
    averaging_start_date: string;
    averaging_end_date: string;
    description: string;
}

export interface EquityAsianOptionInstrumentChange {
    write: EquityAsianOptionInstrumentWrite;
    precondition: Precondition;
}

export interface EquityAsianOptionInstrumentRemoval {
    key: EquityAsianOptionInstrumentKey;
    precondition: Precondition;
}

export interface EquityAsianOptionInstrumentLookup {
    key: EquityAsianOptionInstrumentKey;
    equity_asian_option_instrument: EquityAsianOptionInstrument | null;
}

export interface EquityAsianOptionInstrumentEvent {
    event_id: string;
    key: EquityAsianOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityAsianOptionInstrumentVersionKey {
    equity_asian_option_instrument: EquityAsianOptionInstrumentKey;
    version: number;
}

export interface EquityAsianOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityAsianOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListEquityAsianOptionInstrumentsResponse {
    result: Result;
    equity_asian_option_instruments: EquityAsianOptionInstrument[];
    total: number;
}

export interface GetEquityAsianOptionInstrumentRequest {
    key: EquityAsianOptionInstrumentKey;
}

export interface GetEquityAsianOptionInstrumentResponse {
    result: Result;
    equity_asian_option_instrument: EquityAsianOptionInstrument | null;
}

export interface GetManyEquityAsianOptionInstrumentsRequest {
    keys: EquityAsianOptionInstrumentKey[];
}

export interface GetManyEquityAsianOptionInstrumentsResponse {
    result: Result;
    entries: EquityAsianOptionInstrumentLookup[];
}

export interface PutEquityAsianOptionInstrumentRequest {
    change: EquityAsianOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityAsianOptionInstrumentResponse {
    result: Result;
    equity_asian_option_instrument: EquityAsianOptionInstrument | null;
}

export interface PutManyEquityAsianOptionInstrumentsRequest {
    changes: EquityAsianOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityAsianOptionInstrumentsResponse {
    result: Result;
    equity_asian_option_instruments: EquityAsianOptionInstrument[];
}

export interface DeleteEquityAsianOptionInstrumentRequest {
    removal: EquityAsianOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityAsianOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityAsianOptionInstrumentsRequest {
    removals: EquityAsianOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityAsianOptionInstrumentsResponse {
    result: Result;
}

export interface ListEquityAsianOptionInstrumentVersionsRequest {
    key: EquityAsianOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityAsianOptionInstrumentVersionsFilter | null;
}

export interface ListEquityAsianOptionInstrumentVersionsResponse {
    result: Result;
    versions: EquityAsianOptionInstrument[];
    total: number;
}

export interface GetEquityAsianOptionInstrumentVersionRequest {
    key: EquityAsianOptionInstrumentVersionKey;
}

export interface GetEquityAsianOptionInstrumentVersionResponse {
    result: Result;
    version: EquityAsianOptionInstrument | null;
}

export const subjects = {
    list_equity_asian_option_instruments_request: 'trading.v1.equity_asian_option_instruments.list',
    get_equity_asian_option_instrument_request: 'trading.v1.equity_asian_option_instruments.get',
    get_many_equity_asian_option_instruments_request:
        'trading.v1.equity_asian_option_instruments.get_many',
    put_equity_asian_option_instrument_request: 'trading.v1.equity_asian_option_instruments.put',
    put_many_equity_asian_option_instruments_request:
        'trading.v1.equity_asian_option_instruments.put_many',
    delete_equity_asian_option_instrument_request:
        'trading.v1.equity_asian_option_instruments.delete',
    delete_many_equity_asian_option_instruments_request:
        'trading.v1.equity_asian_option_instruments.delete_many',
    list_equity_asian_option_instrument_versions_request:
        'trading.v1.equity_asian_option_instruments_versions.list',
    get_equity_asian_option_instrument_version_request:
        'trading.v1.equity_asian_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_asian_option_instruments_request: true,
    get_equity_asian_option_instrument_request: true,
    get_many_equity_asian_option_instruments_request: true,
    put_equity_asian_option_instrument_request: true,
    put_many_equity_asian_option_instruments_request: true,
    delete_equity_asian_option_instrument_request: true,
    delete_many_equity_asian_option_instruments_request: true,
    list_equity_asian_option_instrument_versions_request: true,
    get_equity_asian_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_asian_option_instruments_events.created',
    updated: 'trading.v1.equity_asian_option_instruments_events.updated',
    deleted: 'trading.v1.equity_asian_option_instruments_events.deleted',
} as const;
