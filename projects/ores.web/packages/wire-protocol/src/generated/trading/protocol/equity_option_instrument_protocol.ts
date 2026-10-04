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
import type { EquityOptionInstrument } from '../domain/equity_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityOptionInstrumentKey {
    trade_id: string;
}

export interface EquityOptionInstrumentWrite {
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
    settlement_type: string;
    cliquet_frequency: string;
    description: string;
}

export interface EquityOptionInstrumentChange {
    write: EquityOptionInstrumentWrite;
    precondition: Precondition;
}

export interface EquityOptionInstrumentRemoval {
    key: EquityOptionInstrumentKey;
    precondition: Precondition;
}

export interface EquityOptionInstrumentLookup {
    key: EquityOptionInstrumentKey;
    equity_option_instrument: EquityOptionInstrument | null;
}

export interface EquityOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityOptionInstrumentEvent {
    event_id: string;
    key: EquityOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityOptionInstrumentVersionKey {
    equity_option_instrument: EquityOptionInstrumentKey;
    version: number;
}

export interface EquityOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityOptionInstrumentsFilter | null;
}

export interface ListEquityOptionInstrumentsResponse {
    result: Result;
    equity_option_instruments: EquityOptionInstrument[];
    total: number;
}

export interface GetEquityOptionInstrumentRequest {
    key: EquityOptionInstrumentKey;
}

export interface GetEquityOptionInstrumentResponse {
    result: Result;
    equity_option_instrument: EquityOptionInstrument | null;
}

export interface GetManyEquityOptionInstrumentsRequest {
    keys: EquityOptionInstrumentKey[];
}

export interface GetManyEquityOptionInstrumentsResponse {
    result: Result;
    entries: EquityOptionInstrumentLookup[];
}

export interface PutEquityOptionInstrumentRequest {
    change: EquityOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityOptionInstrumentResponse {
    result: Result;
    equity_option_instrument: EquityOptionInstrument | null;
}

export interface PutManyEquityOptionInstrumentsRequest {
    changes: EquityOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityOptionInstrumentsResponse {
    result: Result;
    equity_option_instruments: EquityOptionInstrument[];
}

export interface DeleteEquityOptionInstrumentRequest {
    removal: EquityOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityOptionInstrumentsRequest {
    removals: EquityOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityOptionInstrumentsResponse {
    result: Result;
}

export interface ListEquityOptionInstrumentVersionsRequest {
    key: EquityOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityOptionInstrumentVersionsFilter | null;
}

export interface ListEquityOptionInstrumentVersionsResponse {
    result: Result;
    versions: EquityOptionInstrument[];
    total: number;
}

export interface GetEquityOptionInstrumentVersionRequest {
    key: EquityOptionInstrumentVersionKey;
}

export interface GetEquityOptionInstrumentVersionResponse {
    result: Result;
    version: EquityOptionInstrument | null;
}

export const subjects = {
    list_equity_option_instruments_request: 'trading.v1.equity_option_instruments.list',
    get_equity_option_instrument_request: 'trading.v1.equity_option_instruments.get',
    get_many_equity_option_instruments_request: 'trading.v1.equity_option_instruments.get_many',
    put_equity_option_instrument_request: 'trading.v1.equity_option_instruments.put',
    put_many_equity_option_instruments_request: 'trading.v1.equity_option_instruments.put_many',
    delete_equity_option_instrument_request: 'trading.v1.equity_option_instruments.delete',
    delete_many_equity_option_instruments_request:
        'trading.v1.equity_option_instruments.delete_many',
    list_equity_option_instrument_versions_request:
        'trading.v1.equity_option_instruments_versions.list',
    get_equity_option_instrument_version_request:
        'trading.v1.equity_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_option_instruments_request: true,
    get_equity_option_instrument_request: true,
    get_many_equity_option_instruments_request: true,
    put_equity_option_instrument_request: true,
    put_many_equity_option_instruments_request: true,
    delete_equity_option_instrument_request: true,
    delete_many_equity_option_instruments_request: true,
    list_equity_option_instrument_versions_request: true,
    get_equity_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_option_instruments_events.created',
    updated: 'trading.v1.equity_option_instruments_events.updated',
    deleted: 'trading.v1.equity_option_instruments_events.deleted',
} as const;
