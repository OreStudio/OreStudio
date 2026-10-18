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
import type { EquityTarfInstrument } from '../domain/equity_tarf_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityTarfInstrumentKey {
    trade_id: string;
}

export interface EquityTarfInstrumentWrite {
    trade_id: string;
    trade_activity_id: string;
    underlying_name: string;
    currency: string;
    fixing_amount: string;
    target_amount: string | null;
    target_points: number | null;
    strike: string | null;
}

export interface EquityTarfInstrumentChange {
    write: EquityTarfInstrumentWrite;
    precondition: Precondition;
}

export interface EquityTarfInstrumentRemoval {
    key: EquityTarfInstrumentKey;
    precondition: Precondition;
}

export interface EquityTarfInstrumentLookup {
    key: EquityTarfInstrumentKey;
    equity_tarf_instrument: EquityTarfInstrument | null;
}

export interface EquityTarfInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityTarfInstrumentEvent {
    event_id: string;
    key: EquityTarfInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityTarfInstrumentVersionKey {
    equity_tarf_instrument: EquityTarfInstrumentKey;
    version: number;
}

export interface EquityTarfInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityTarfInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityTarfInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquityTarfInstrumentsResponse {
    result: Result;
    equity_tarf_instruments: EquityTarfInstrument[];
    total: number;
}

export interface GetEquityTarfInstrumentRequest {
    key: EquityTarfInstrumentKey;
}

export interface GetEquityTarfInstrumentResponse {
    result: Result;
    equity_tarf_instrument: EquityTarfInstrument | null;
}

export interface GetManyEquityTarfInstrumentsRequest {
    keys: EquityTarfInstrumentKey[];
}

export interface GetManyEquityTarfInstrumentsResponse {
    result: Result;
    entries: EquityTarfInstrumentLookup[];
}

export interface PutEquityTarfInstrumentRequest {
    change: EquityTarfInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityTarfInstrumentResponse {
    result: Result;
    equity_tarf_instrument: EquityTarfInstrument | null;
}

export interface PutManyEquityTarfInstrumentsRequest {
    changes: EquityTarfInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityTarfInstrumentsResponse {
    result: Result;
    equity_tarf_instruments: EquityTarfInstrument[];
}

export interface DeleteEquityTarfInstrumentRequest {
    removal: EquityTarfInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityTarfInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityTarfInstrumentsRequest {
    removals: EquityTarfInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityTarfInstrumentsResponse {
    result: Result;
}

export interface ListEquityTarfInstrumentVersionsRequest {
    key: EquityTarfInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityTarfInstrumentVersionsFilter | null;
}

export interface ListEquityTarfInstrumentVersionsResponse {
    result: Result;
    versions: EquityTarfInstrument[];
    total: number;
}

export interface GetEquityTarfInstrumentVersionRequest {
    key: EquityTarfInstrumentVersionKey;
}

export interface GetEquityTarfInstrumentVersionResponse {
    result: Result;
    version: EquityTarfInstrument | null;
}

export const subjects = {
    list_equity_tarf_instruments_request: 'trading.v1.equity_tarf_instruments.list',
    get_equity_tarf_instrument_request: 'trading.v1.equity_tarf_instruments.get',
    get_many_equity_tarf_instruments_request: 'trading.v1.equity_tarf_instruments.get_many',
    put_equity_tarf_instrument_request: 'trading.v1.equity_tarf_instruments.put',
    put_many_equity_tarf_instruments_request: 'trading.v1.equity_tarf_instruments.put_many',
    delete_equity_tarf_instrument_request: 'trading.v1.equity_tarf_instruments.delete',
    delete_many_equity_tarf_instruments_request: 'trading.v1.equity_tarf_instruments.delete_many',
    list_equity_tarf_instrument_versions_request:
        'trading.v1.equity_tarf_instruments_versions.list',
    get_equity_tarf_instrument_version_request: 'trading.v1.equity_tarf_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_tarf_instruments_request: true,
    get_equity_tarf_instrument_request: true,
    get_many_equity_tarf_instruments_request: true,
    put_equity_tarf_instrument_request: true,
    put_many_equity_tarf_instruments_request: true,
    delete_equity_tarf_instrument_request: true,
    delete_many_equity_tarf_instruments_request: true,
    list_equity_tarf_instrument_versions_request: true,
    get_equity_tarf_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_tarf_instruments_events.created',
    updated: 'trading.v1.equity_tarf_instruments_events.updated',
    deleted: 'trading.v1.equity_tarf_instruments_events.deleted',
} as const;
