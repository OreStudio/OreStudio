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
import type { InflationSwapInstrument } from '../domain/inflation_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InflationSwapInstrumentKey {
    trade_id: string;
}

export interface InflationSwapInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    start_date: string;
    maturity_date: string;
    inflation_index_code: string;
    base_cpi: number | null;
    lag_convention: string;
    description: string;
}

export interface InflationSwapInstrumentChange {
    write: InflationSwapInstrumentWrite;
    precondition: Precondition;
}

export interface InflationSwapInstrumentRemoval {
    key: InflationSwapInstrumentKey;
    precondition: Precondition;
}

export interface InflationSwapInstrumentLookup {
    key: InflationSwapInstrumentKey;
    inflation_swap_instrument: InflationSwapInstrument | null;
}

export interface InflationSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface InflationSwapInstrumentEvent {
    event_id: string;
    key: InflationSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InflationSwapInstrumentVersionKey {
    inflation_swap_instrument: InflationSwapInstrumentKey;
    version: number;
}

export interface InflationSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInflationSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: InflationSwapInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListInflationSwapInstrumentsResponse {
    result: Result;
    inflation_swap_instruments: InflationSwapInstrument[];
    total: number;
}

export interface GetInflationSwapInstrumentRequest {
    key: InflationSwapInstrumentKey;
}

export interface GetInflationSwapInstrumentResponse {
    result: Result;
    inflation_swap_instrument: InflationSwapInstrument | null;
}

export interface GetManyInflationSwapInstrumentsRequest {
    keys: InflationSwapInstrumentKey[];
}

export interface GetManyInflationSwapInstrumentsResponse {
    result: Result;
    entries: InflationSwapInstrumentLookup[];
}

export interface PutInflationSwapInstrumentRequest {
    change: InflationSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutInflationSwapInstrumentResponse {
    result: Result;
    inflation_swap_instrument: InflationSwapInstrument | null;
}

export interface PutManyInflationSwapInstrumentsRequest {
    changes: InflationSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyInflationSwapInstrumentsResponse {
    result: Result;
    inflation_swap_instruments: InflationSwapInstrument[];
}

export interface DeleteInflationSwapInstrumentRequest {
    removal: InflationSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteInflationSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyInflationSwapInstrumentsRequest {
    removals: InflationSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInflationSwapInstrumentsResponse {
    result: Result;
}

export interface ListInflationSwapInstrumentVersionsRequest {
    key: InflationSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InflationSwapInstrumentVersionsFilter | null;
}

export interface ListInflationSwapInstrumentVersionsResponse {
    result: Result;
    versions: InflationSwapInstrument[];
    total: number;
}

export interface GetInflationSwapInstrumentVersionRequest {
    key: InflationSwapInstrumentVersionKey;
}

export interface GetInflationSwapInstrumentVersionResponse {
    result: Result;
    version: InflationSwapInstrument | null;
}

export const subjects = {
    list_inflation_swap_instruments_request: 'trading.v1.inflation_swap_instruments.list',
    get_inflation_swap_instrument_request: 'trading.v1.inflation_swap_instruments.get',
    get_many_inflation_swap_instruments_request: 'trading.v1.inflation_swap_instruments.get_many',
    put_inflation_swap_instrument_request: 'trading.v1.inflation_swap_instruments.put',
    put_many_inflation_swap_instruments_request: 'trading.v1.inflation_swap_instruments.put_many',
    delete_inflation_swap_instrument_request: 'trading.v1.inflation_swap_instruments.delete',
    delete_many_inflation_swap_instruments_request:
        'trading.v1.inflation_swap_instruments.delete_many',
    list_inflation_swap_instrument_versions_request:
        'trading.v1.inflation_swap_instruments_versions.list',
    get_inflation_swap_instrument_version_request:
        'trading.v1.inflation_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_inflation_swap_instruments_request: true,
    get_inflation_swap_instrument_request: true,
    get_many_inflation_swap_instruments_request: true,
    put_inflation_swap_instrument_request: true,
    put_many_inflation_swap_instruments_request: true,
    delete_inflation_swap_instrument_request: true,
    delete_many_inflation_swap_instruments_request: true,
    list_inflation_swap_instrument_versions_request: true,
    get_inflation_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.inflation_swap_instruments_events.created',
    updated: 'trading.v1.inflation_swap_instruments_events.updated',
    deleted: 'trading.v1.inflation_swap_instruments_events.deleted',
} as const;
