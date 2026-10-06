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
import type { EquityPositionInstrument } from '../domain/equity_position_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityPositionInstrumentKey {
    trade_id: string;
}

export interface EquityPositionInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    underlying_name: string;
    currency: string;
    quantity: number;
    price: string | null;
    description: string;
}

export interface EquityPositionInstrumentChange {
    write: EquityPositionInstrumentWrite;
    precondition: Precondition;
}

export interface EquityPositionInstrumentRemoval {
    key: EquityPositionInstrumentKey;
    precondition: Precondition;
}

export interface EquityPositionInstrumentLookup {
    key: EquityPositionInstrumentKey;
    equity_position_instrument: EquityPositionInstrument | null;
}

export interface EquityPositionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityPositionInstrumentEvent {
    event_id: string;
    key: EquityPositionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityPositionInstrumentVersionKey {
    equity_position_instrument: EquityPositionInstrumentKey;
    version: number;
}

export interface EquityPositionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityPositionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityPositionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquityPositionInstrumentsResponse {
    result: Result;
    equity_position_instruments: EquityPositionInstrument[];
    total: number;
}

export interface GetEquityPositionInstrumentRequest {
    key: EquityPositionInstrumentKey;
}

export interface GetEquityPositionInstrumentResponse {
    result: Result;
    equity_position_instrument: EquityPositionInstrument | null;
}

export interface GetManyEquityPositionInstrumentsRequest {
    keys: EquityPositionInstrumentKey[];
}

export interface GetManyEquityPositionInstrumentsResponse {
    result: Result;
    entries: EquityPositionInstrumentLookup[];
}

export interface PutEquityPositionInstrumentRequest {
    change: EquityPositionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityPositionInstrumentResponse {
    result: Result;
    equity_position_instrument: EquityPositionInstrument | null;
}

export interface PutManyEquityPositionInstrumentsRequest {
    changes: EquityPositionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityPositionInstrumentsResponse {
    result: Result;
    equity_position_instruments: EquityPositionInstrument[];
}

export interface DeleteEquityPositionInstrumentRequest {
    removal: EquityPositionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityPositionInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityPositionInstrumentsRequest {
    removals: EquityPositionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityPositionInstrumentsResponse {
    result: Result;
}

export interface ListEquityPositionInstrumentVersionsRequest {
    key: EquityPositionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityPositionInstrumentVersionsFilter | null;
}

export interface ListEquityPositionInstrumentVersionsResponse {
    result: Result;
    versions: EquityPositionInstrument[];
    total: number;
}

export interface GetEquityPositionInstrumentVersionRequest {
    key: EquityPositionInstrumentVersionKey;
}

export interface GetEquityPositionInstrumentVersionResponse {
    result: Result;
    version: EquityPositionInstrument | null;
}

export const subjects = {
    list_equity_position_instruments_request: 'trading.v1.equity_position_instruments.list',
    get_equity_position_instrument_request: 'trading.v1.equity_position_instruments.get',
    get_many_equity_position_instruments_request: 'trading.v1.equity_position_instruments.get_many',
    put_equity_position_instrument_request: 'trading.v1.equity_position_instruments.put',
    put_many_equity_position_instruments_request: 'trading.v1.equity_position_instruments.put_many',
    delete_equity_position_instrument_request: 'trading.v1.equity_position_instruments.delete',
    delete_many_equity_position_instruments_request:
        'trading.v1.equity_position_instruments.delete_many',
    list_equity_position_instrument_versions_request:
        'trading.v1.equity_position_instruments_versions.list',
    get_equity_position_instrument_version_request:
        'trading.v1.equity_position_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_position_instruments_request: true,
    get_equity_position_instrument_request: true,
    get_many_equity_position_instruments_request: true,
    put_equity_position_instrument_request: true,
    put_many_equity_position_instruments_request: true,
    delete_equity_position_instrument_request: true,
    delete_many_equity_position_instruments_request: true,
    list_equity_position_instrument_versions_request: true,
    get_equity_position_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_position_instruments_events.created',
    updated: 'trading.v1.equity_position_instruments_events.updated',
    deleted: 'trading.v1.equity_position_instruments_events.deleted',
} as const;
