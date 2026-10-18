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
import type { EquityInstrument } from '../domain/equity_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityInstrumentKey {
    trade_id: string;
}

export interface EquityInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    start_date: string | null;
    maturity_date: string | null;
    description: string;
}

export interface EquityInstrumentChange {
    write: EquityInstrumentWrite;
    precondition: Precondition;
}

export interface EquityInstrumentRemoval {
    key: EquityInstrumentKey;
    precondition: Precondition;
}

export interface EquityInstrumentLookup {
    key: EquityInstrumentKey;
    equity_instrument: EquityInstrument | null;
}

export interface EquityInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityInstrumentEvent {
    event_id: string;
    key: EquityInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityInstrumentVersionKey {
    equity_instrument: EquityInstrumentKey;
    version: number;
}

export interface EquityInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquityInstrumentsResponse {
    result: Result;
    equity_instruments: EquityInstrument[];
    total: number;
}

export interface GetEquityInstrumentRequest {
    key: EquityInstrumentKey;
}

export interface GetEquityInstrumentResponse {
    result: Result;
    equity_instrument: EquityInstrument | null;
}

export interface GetManyEquityInstrumentsRequest {
    keys: EquityInstrumentKey[];
}

export interface GetManyEquityInstrumentsResponse {
    result: Result;
    entries: EquityInstrumentLookup[];
}

export interface PutEquityInstrumentRequest {
    change: EquityInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityInstrumentResponse {
    result: Result;
    equity_instrument: EquityInstrument | null;
}

export interface PutManyEquityInstrumentsRequest {
    changes: EquityInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityInstrumentsResponse {
    result: Result;
    equity_instruments: EquityInstrument[];
}

export interface DeleteEquityInstrumentRequest {
    removal: EquityInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityInstrumentsRequest {
    removals: EquityInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityInstrumentsResponse {
    result: Result;
}

export interface ListEquityInstrumentVersionsRequest {
    key: EquityInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityInstrumentVersionsFilter | null;
}

export interface ListEquityInstrumentVersionsResponse {
    result: Result;
    versions: EquityInstrument[];
    total: number;
}

export interface GetEquityInstrumentVersionRequest {
    key: EquityInstrumentVersionKey;
}

export interface GetEquityInstrumentVersionResponse {
    result: Result;
    version: EquityInstrument | null;
}

export const subjects = {
    list_equity_instruments_request: 'trading.v1.equity_instruments.list',
    get_equity_instrument_request: 'trading.v1.equity_instruments.get',
    get_many_equity_instruments_request: 'trading.v1.equity_instruments.get_many',
    put_equity_instrument_request: 'trading.v1.equity_instruments.put',
    put_many_equity_instruments_request: 'trading.v1.equity_instruments.put_many',
    delete_equity_instrument_request: 'trading.v1.equity_instruments.delete',
    delete_many_equity_instruments_request: 'trading.v1.equity_instruments.delete_many',
    list_equity_instrument_versions_request: 'trading.v1.equity_instruments_versions.list',
    get_equity_instrument_version_request: 'trading.v1.equity_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_instruments_request: true,
    get_equity_instrument_request: true,
    get_many_equity_instruments_request: true,
    put_equity_instrument_request: true,
    put_many_equity_instruments_request: true,
    delete_equity_instrument_request: true,
    delete_many_equity_instruments_request: true,
    list_equity_instrument_versions_request: true,
    get_equity_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_instruments_events.created',
    updated: 'trading.v1.equity_instruments_events.updated',
    deleted: 'trading.v1.equity_instruments_events.deleted',
} as const;
