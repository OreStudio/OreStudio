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
import type { FxAccumulatorInstrument } from '../domain/fx_accumulator_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxAccumulatorInstrumentKey {
    trade_id: string;
}

export interface FxAccumulatorInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    currency: string;
    fixing_amount: string;
    strike: string;
    underlying_code: string;
    long_short: string;
    start_date: string;
    knock_out_barrier: string | null;
    description: string;
}

export interface FxAccumulatorInstrumentChange {
    write: FxAccumulatorInstrumentWrite;
    precondition: Precondition;
}

export interface FxAccumulatorInstrumentRemoval {
    key: FxAccumulatorInstrumentKey;
    precondition: Precondition;
}

export interface FxAccumulatorInstrumentLookup {
    key: FxAccumulatorInstrumentKey;
    fx_accumulator_instrument: FxAccumulatorInstrument | null;
}

export interface FxAccumulatorInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface FxAccumulatorInstrumentEvent {
    event_id: string;
    key: FxAccumulatorInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxAccumulatorInstrumentVersionKey {
    fx_accumulator_instrument: FxAccumulatorInstrumentKey;
    version: number;
}

export interface FxAccumulatorInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxAccumulatorInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxAccumulatorInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListFxAccumulatorInstrumentsResponse {
    result: Result;
    fx_accumulator_instruments: FxAccumulatorInstrument[];
    total: number;
}

export interface GetFxAccumulatorInstrumentRequest {
    key: FxAccumulatorInstrumentKey;
}

export interface GetFxAccumulatorInstrumentResponse {
    result: Result;
    fx_accumulator_instrument: FxAccumulatorInstrument | null;
}

export interface GetManyFxAccumulatorInstrumentsRequest {
    keys: FxAccumulatorInstrumentKey[];
}

export interface GetManyFxAccumulatorInstrumentsResponse {
    result: Result;
    entries: FxAccumulatorInstrumentLookup[];
}

export interface PutFxAccumulatorInstrumentRequest {
    change: FxAccumulatorInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFxAccumulatorInstrumentResponse {
    result: Result;
    fx_accumulator_instrument: FxAccumulatorInstrument | null;
}

export interface PutManyFxAccumulatorInstrumentsRequest {
    changes: FxAccumulatorInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFxAccumulatorInstrumentsResponse {
    result: Result;
    fx_accumulator_instruments: FxAccumulatorInstrument[];
}

export interface DeleteFxAccumulatorInstrumentRequest {
    removal: FxAccumulatorInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxAccumulatorInstrumentResponse {
    result: Result;
}

export interface DeleteManyFxAccumulatorInstrumentsRequest {
    removals: FxAccumulatorInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxAccumulatorInstrumentsResponse {
    result: Result;
}

export interface ListFxAccumulatorInstrumentVersionsRequest {
    key: FxAccumulatorInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxAccumulatorInstrumentVersionsFilter | null;
}

export interface ListFxAccumulatorInstrumentVersionsResponse {
    result: Result;
    versions: FxAccumulatorInstrument[];
    total: number;
}

export interface GetFxAccumulatorInstrumentVersionRequest {
    key: FxAccumulatorInstrumentVersionKey;
}

export interface GetFxAccumulatorInstrumentVersionResponse {
    result: Result;
    version: FxAccumulatorInstrument | null;
}

export const subjects = {
    list_fx_accumulator_instruments_request: 'trading.v1.fx_accumulator_instruments.list',
    get_fx_accumulator_instrument_request: 'trading.v1.fx_accumulator_instruments.get',
    get_many_fx_accumulator_instruments_request: 'trading.v1.fx_accumulator_instruments.get_many',
    put_fx_accumulator_instrument_request: 'trading.v1.fx_accumulator_instruments.put',
    put_many_fx_accumulator_instruments_request: 'trading.v1.fx_accumulator_instruments.put_many',
    delete_fx_accumulator_instrument_request: 'trading.v1.fx_accumulator_instruments.delete',
    delete_many_fx_accumulator_instruments_request:
        'trading.v1.fx_accumulator_instruments.delete_many',
    list_fx_accumulator_instrument_versions_request:
        'trading.v1.fx_accumulator_instruments_versions.list',
    get_fx_accumulator_instrument_version_request:
        'trading.v1.fx_accumulator_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_accumulator_instruments_request: true,
    get_fx_accumulator_instrument_request: true,
    get_many_fx_accumulator_instruments_request: true,
    put_fx_accumulator_instrument_request: true,
    put_many_fx_accumulator_instruments_request: true,
    delete_fx_accumulator_instrument_request: true,
    delete_many_fx_accumulator_instruments_request: true,
    list_fx_accumulator_instrument_versions_request: true,
    get_fx_accumulator_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.fx_accumulator_instruments_events.created',
    updated: 'trading.v1.fx_accumulator_instruments_events.updated',
    deleted: 'trading.v1.fx_accumulator_instruments_events.deleted',
} as const;
