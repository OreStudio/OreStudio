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
import type { EuropeanOptionBarrierInstrument } from '../domain/european_option_barrier_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EuropeanOptionBarrierInstrumentKey {
    trade_id: string;
}

export interface EuropeanOptionBarrierInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    quantity: number;
    option_type: string;
    long_short: string;
    strike: string;
    premium_amount: string;
    premium_currency: string;
    premium_date: string;
    option_expiry: string;
    barrier_level: string;
    barrier_type: string;
    barrier_style: string;
    settlement_date: string;
    pay_currency: string;
    description: string;
}

export interface EuropeanOptionBarrierInstrumentChange {
    write: EuropeanOptionBarrierInstrumentWrite;
    precondition: Precondition;
}

export interface EuropeanOptionBarrierInstrumentRemoval {
    key: EuropeanOptionBarrierInstrumentKey;
    precondition: Precondition;
}

export interface EuropeanOptionBarrierInstrumentLookup {
    key: EuropeanOptionBarrierInstrumentKey;
    european_option_barrier_instrument: EuropeanOptionBarrierInstrument | null;
}

export interface EuropeanOptionBarrierInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EuropeanOptionBarrierInstrumentEvent {
    event_id: string;
    key: EuropeanOptionBarrierInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EuropeanOptionBarrierInstrumentVersionKey {
    european_option_barrier_instrument: EuropeanOptionBarrierInstrumentKey;
    version: number;
}

export interface EuropeanOptionBarrierInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEuropeanOptionBarrierInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EuropeanOptionBarrierInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEuropeanOptionBarrierInstrumentsResponse {
    result: Result;
    european_option_barrier_instruments: EuropeanOptionBarrierInstrument[];
    total: number;
}

export interface GetEuropeanOptionBarrierInstrumentRequest {
    key: EuropeanOptionBarrierInstrumentKey;
}

export interface GetEuropeanOptionBarrierInstrumentResponse {
    result: Result;
    european_option_barrier_instrument: EuropeanOptionBarrierInstrument | null;
}

export interface GetManyEuropeanOptionBarrierInstrumentsRequest {
    keys: EuropeanOptionBarrierInstrumentKey[];
}

export interface GetManyEuropeanOptionBarrierInstrumentsResponse {
    result: Result;
    entries: EuropeanOptionBarrierInstrumentLookup[];
}

export interface PutEuropeanOptionBarrierInstrumentRequest {
    change: EuropeanOptionBarrierInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEuropeanOptionBarrierInstrumentResponse {
    result: Result;
    european_option_barrier_instrument: EuropeanOptionBarrierInstrument | null;
}

export interface PutManyEuropeanOptionBarrierInstrumentsRequest {
    changes: EuropeanOptionBarrierInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEuropeanOptionBarrierInstrumentsResponse {
    result: Result;
    european_option_barrier_instruments: EuropeanOptionBarrierInstrument[];
}

export interface DeleteEuropeanOptionBarrierInstrumentRequest {
    removal: EuropeanOptionBarrierInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEuropeanOptionBarrierInstrumentResponse {
    result: Result;
}

export interface DeleteManyEuropeanOptionBarrierInstrumentsRequest {
    removals: EuropeanOptionBarrierInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEuropeanOptionBarrierInstrumentsResponse {
    result: Result;
}

export interface ListEuropeanOptionBarrierInstrumentVersionsRequest {
    key: EuropeanOptionBarrierInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EuropeanOptionBarrierInstrumentVersionsFilter | null;
}

export interface ListEuropeanOptionBarrierInstrumentVersionsResponse {
    result: Result;
    versions: EuropeanOptionBarrierInstrument[];
    total: number;
}

export interface GetEuropeanOptionBarrierInstrumentVersionRequest {
    key: EuropeanOptionBarrierInstrumentVersionKey;
}

export interface GetEuropeanOptionBarrierInstrumentVersionResponse {
    result: Result;
    version: EuropeanOptionBarrierInstrument | null;
}

export const subjects = {
    list_european_option_barrier_instruments_request:
        'trading.v1.european_option_barrier_instruments.list',
    get_european_option_barrier_instrument_request:
        'trading.v1.european_option_barrier_instruments.get',
    get_many_european_option_barrier_instruments_request:
        'trading.v1.european_option_barrier_instruments.get_many',
    put_european_option_barrier_instrument_request:
        'trading.v1.european_option_barrier_instruments.put',
    put_many_european_option_barrier_instruments_request:
        'trading.v1.european_option_barrier_instruments.put_many',
    delete_european_option_barrier_instrument_request:
        'trading.v1.european_option_barrier_instruments.delete',
    delete_many_european_option_barrier_instruments_request:
        'trading.v1.european_option_barrier_instruments.delete_many',
    list_european_option_barrier_instrument_versions_request:
        'trading.v1.european_option_barrier_instruments_versions.list',
    get_european_option_barrier_instrument_version_request:
        'trading.v1.european_option_barrier_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_european_option_barrier_instruments_request: true,
    get_european_option_barrier_instrument_request: true,
    get_many_european_option_barrier_instruments_request: true,
    put_european_option_barrier_instrument_request: true,
    put_many_european_option_barrier_instruments_request: true,
    delete_european_option_barrier_instrument_request: true,
    delete_many_european_option_barrier_instruments_request: true,
    list_european_option_barrier_instrument_versions_request: true,
    get_european_option_barrier_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.european_option_barrier_instruments.created',
    updated: 'trading.v1.events.european_option_barrier_instruments.updated',
    deleted: 'trading.v1.events.european_option_barrier_instruments.deleted',
} as const;
