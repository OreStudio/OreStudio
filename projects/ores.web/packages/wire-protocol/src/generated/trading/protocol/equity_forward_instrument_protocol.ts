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
import type { EquityForwardInstrument } from '../domain/equity_forward_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityForwardInstrumentKey {
    trade_id: string;
}

export interface EquityForwardInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    underlying_name: string;
    currency: string;
    quantity: number;
    forward_price: string | null;
    expiry_date: string;
    long_short: string;
    settlement_type: string;
    description: string;
}

export interface EquityForwardInstrumentChange {
    write: EquityForwardInstrumentWrite;
    precondition: Precondition;
}

export interface EquityForwardInstrumentRemoval {
    key: EquityForwardInstrumentKey;
    precondition: Precondition;
}

export interface EquityForwardInstrumentLookup {
    key: EquityForwardInstrumentKey;
    equity_forward_instrument: EquityForwardInstrument | null;
}

export interface EquityForwardInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityForwardInstrumentEvent {
    event_id: string;
    key: EquityForwardInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityForwardInstrumentVersionKey {
    equity_forward_instrument: EquityForwardInstrumentKey;
    version: number;
}

export interface EquityForwardInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityForwardInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityForwardInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquityForwardInstrumentsResponse {
    result: Result;
    equity_forward_instruments: EquityForwardInstrument[];
    total: number;
}

export interface GetEquityForwardInstrumentRequest {
    key: EquityForwardInstrumentKey;
}

export interface GetEquityForwardInstrumentResponse {
    result: Result;
    equity_forward_instrument: EquityForwardInstrument | null;
}

export interface GetManyEquityForwardInstrumentsRequest {
    keys: EquityForwardInstrumentKey[];
}

export interface GetManyEquityForwardInstrumentsResponse {
    result: Result;
    entries: EquityForwardInstrumentLookup[];
}

export interface PutEquityForwardInstrumentRequest {
    change: EquityForwardInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityForwardInstrumentResponse {
    result: Result;
    equity_forward_instrument: EquityForwardInstrument | null;
}

export interface PutManyEquityForwardInstrumentsRequest {
    changes: EquityForwardInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityForwardInstrumentsResponse {
    result: Result;
    equity_forward_instruments: EquityForwardInstrument[];
}

export interface DeleteEquityForwardInstrumentRequest {
    removal: EquityForwardInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityForwardInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityForwardInstrumentsRequest {
    removals: EquityForwardInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityForwardInstrumentsResponse {
    result: Result;
}

export interface ListEquityForwardInstrumentVersionsRequest {
    key: EquityForwardInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityForwardInstrumentVersionsFilter | null;
}

export interface ListEquityForwardInstrumentVersionsResponse {
    result: Result;
    versions: EquityForwardInstrument[];
    total: number;
}

export interface GetEquityForwardInstrumentVersionRequest {
    key: EquityForwardInstrumentVersionKey;
}

export interface GetEquityForwardInstrumentVersionResponse {
    result: Result;
    version: EquityForwardInstrument | null;
}

export const subjects = {
    list_equity_forward_instruments_request: 'trading.v1.equity_forward_instruments.list',
    get_equity_forward_instrument_request: 'trading.v1.equity_forward_instruments.get',
    get_many_equity_forward_instruments_request: 'trading.v1.equity_forward_instruments.get_many',
    put_equity_forward_instrument_request: 'trading.v1.equity_forward_instruments.put',
    put_many_equity_forward_instruments_request: 'trading.v1.equity_forward_instruments.put_many',
    delete_equity_forward_instrument_request: 'trading.v1.equity_forward_instruments.delete',
    delete_many_equity_forward_instruments_request:
        'trading.v1.equity_forward_instruments.delete_many',
    list_equity_forward_instrument_versions_request:
        'trading.v1.equity_forward_instruments_versions.list',
    get_equity_forward_instrument_version_request:
        'trading.v1.equity_forward_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_forward_instruments_request: true,
    get_equity_forward_instrument_request: true,
    get_many_equity_forward_instruments_request: true,
    put_equity_forward_instrument_request: true,
    put_many_equity_forward_instruments_request: true,
    delete_equity_forward_instrument_request: true,
    delete_many_equity_forward_instruments_request: true,
    list_equity_forward_instrument_versions_request: true,
    get_equity_forward_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_forward_instruments_events.created',
    updated: 'trading.v1.equity_forward_instruments_events.updated',
    deleted: 'trading.v1.equity_forward_instruments_events.deleted',
} as const;
