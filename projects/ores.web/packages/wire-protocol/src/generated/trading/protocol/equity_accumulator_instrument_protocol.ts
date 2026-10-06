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
import type { EquityAccumulatorInstrument } from '../domain/equity_accumulator_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityAccumulatorInstrumentKey {
    trade_id: string;
}

export interface EquityAccumulatorInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    underlying_name: string;
    currency: string;
    strike: string;
    fixing_amount: string;
    start_date: string;
    expiry_date: string;
    fixing_frequency: string;
    long_short: string;
    knock_out_level: string | null;
    target_amount: string | null;
    target_type: string;
    payoff_type: string;
    description: string;
}

export interface EquityAccumulatorInstrumentChange {
    write: EquityAccumulatorInstrumentWrite;
    precondition: Precondition;
}

export interface EquityAccumulatorInstrumentRemoval {
    key: EquityAccumulatorInstrumentKey;
    precondition: Precondition;
}

export interface EquityAccumulatorInstrumentLookup {
    key: EquityAccumulatorInstrumentKey;
    equity_accumulator_instrument: EquityAccumulatorInstrument | null;
}

export interface EquityAccumulatorInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityAccumulatorInstrumentEvent {
    event_id: string;
    key: EquityAccumulatorInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityAccumulatorInstrumentVersionKey {
    equity_accumulator_instrument: EquityAccumulatorInstrumentKey;
    version: number;
}

export interface EquityAccumulatorInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityAccumulatorInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityAccumulatorInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquityAccumulatorInstrumentsResponse {
    result: Result;
    equity_accumulator_instruments: EquityAccumulatorInstrument[];
    total: number;
}

export interface GetEquityAccumulatorInstrumentRequest {
    key: EquityAccumulatorInstrumentKey;
}

export interface GetEquityAccumulatorInstrumentResponse {
    result: Result;
    equity_accumulator_instrument: EquityAccumulatorInstrument | null;
}

export interface GetManyEquityAccumulatorInstrumentsRequest {
    keys: EquityAccumulatorInstrumentKey[];
}

export interface GetManyEquityAccumulatorInstrumentsResponse {
    result: Result;
    entries: EquityAccumulatorInstrumentLookup[];
}

export interface PutEquityAccumulatorInstrumentRequest {
    change: EquityAccumulatorInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityAccumulatorInstrumentResponse {
    result: Result;
    equity_accumulator_instrument: EquityAccumulatorInstrument | null;
}

export interface PutManyEquityAccumulatorInstrumentsRequest {
    changes: EquityAccumulatorInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityAccumulatorInstrumentsResponse {
    result: Result;
    equity_accumulator_instruments: EquityAccumulatorInstrument[];
}

export interface DeleteEquityAccumulatorInstrumentRequest {
    removal: EquityAccumulatorInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityAccumulatorInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityAccumulatorInstrumentsRequest {
    removals: EquityAccumulatorInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityAccumulatorInstrumentsResponse {
    result: Result;
}

export interface ListEquityAccumulatorInstrumentVersionsRequest {
    key: EquityAccumulatorInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityAccumulatorInstrumentVersionsFilter | null;
}

export interface ListEquityAccumulatorInstrumentVersionsResponse {
    result: Result;
    versions: EquityAccumulatorInstrument[];
    total: number;
}

export interface GetEquityAccumulatorInstrumentVersionRequest {
    key: EquityAccumulatorInstrumentVersionKey;
}

export interface GetEquityAccumulatorInstrumentVersionResponse {
    result: Result;
    version: EquityAccumulatorInstrument | null;
}

export const subjects = {
    list_equity_accumulator_instruments_request: 'trading.v1.equity_accumulator_instruments.list',
    get_equity_accumulator_instrument_request: 'trading.v1.equity_accumulator_instruments.get',
    get_many_equity_accumulator_instruments_request:
        'trading.v1.equity_accumulator_instruments.get_many',
    put_equity_accumulator_instrument_request: 'trading.v1.equity_accumulator_instruments.put',
    put_many_equity_accumulator_instruments_request:
        'trading.v1.equity_accumulator_instruments.put_many',
    delete_equity_accumulator_instrument_request:
        'trading.v1.equity_accumulator_instruments.delete',
    delete_many_equity_accumulator_instruments_request:
        'trading.v1.equity_accumulator_instruments.delete_many',
    list_equity_accumulator_instrument_versions_request:
        'trading.v1.equity_accumulator_instruments_versions.list',
    get_equity_accumulator_instrument_version_request:
        'trading.v1.equity_accumulator_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_accumulator_instruments_request: true,
    get_equity_accumulator_instrument_request: true,
    get_many_equity_accumulator_instruments_request: true,
    put_equity_accumulator_instrument_request: true,
    put_many_equity_accumulator_instruments_request: true,
    delete_equity_accumulator_instrument_request: true,
    delete_many_equity_accumulator_instruments_request: true,
    list_equity_accumulator_instrument_versions_request: true,
    get_equity_accumulator_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_accumulator_instruments_events.created',
    updated: 'trading.v1.equity_accumulator_instruments_events.updated',
    deleted: 'trading.v1.equity_accumulator_instruments_events.deleted',
} as const;
