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
import type { FraInstrument } from '../domain/fra_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FraInstrumentKey {
    trade_id: string;
}

export interface FraInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    start_date: string;
    end_date: string;
    currency: string;
    rate_index: string;
    long_short: string;
    strike: number;
    notional: string;
    description: string;
}

export interface FraInstrumentChange {
    write: FraInstrumentWrite;
    precondition: Precondition;
}

export interface FraInstrumentRemoval {
    key: FraInstrumentKey;
    precondition: Precondition;
}

export interface FraInstrumentLookup {
    key: FraInstrumentKey;
    fra_instrument: FraInstrument | null;
}

export interface FraInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface FraInstrumentEvent {
    event_id: string;
    key: FraInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FraInstrumentVersionKey {
    fra_instrument: FraInstrumentKey;
    version: number;
}

export interface FraInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFraInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FraInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListFraInstrumentsResponse {
    result: Result;
    fra_instruments: FraInstrument[];
    total: number;
}

export interface GetFraInstrumentRequest {
    key: FraInstrumentKey;
}

export interface GetFraInstrumentResponse {
    result: Result;
    fra_instrument: FraInstrument | null;
}

export interface GetManyFraInstrumentsRequest {
    keys: FraInstrumentKey[];
}

export interface GetManyFraInstrumentsResponse {
    result: Result;
    entries: FraInstrumentLookup[];
}

export interface PutFraInstrumentRequest {
    change: FraInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFraInstrumentResponse {
    result: Result;
    fra_instrument: FraInstrument | null;
}

export interface PutManyFraInstrumentsRequest {
    changes: FraInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFraInstrumentsResponse {
    result: Result;
    fra_instruments: FraInstrument[];
}

export interface DeleteFraInstrumentRequest {
    removal: FraInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFraInstrumentResponse {
    result: Result;
}

export interface DeleteManyFraInstrumentsRequest {
    removals: FraInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFraInstrumentsResponse {
    result: Result;
}

export interface ListFraInstrumentVersionsRequest {
    key: FraInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FraInstrumentVersionsFilter | null;
}

export interface ListFraInstrumentVersionsResponse {
    result: Result;
    versions: FraInstrument[];
    total: number;
}

export interface GetFraInstrumentVersionRequest {
    key: FraInstrumentVersionKey;
}

export interface GetFraInstrumentVersionResponse {
    result: Result;
    version: FraInstrument | null;
}

export const subjects = {
    list_fra_instruments_request: 'trading.v1.fra_instruments.list',
    get_fra_instrument_request: 'trading.v1.fra_instruments.get',
    get_many_fra_instruments_request: 'trading.v1.fra_instruments.get_many',
    put_fra_instrument_request: 'trading.v1.fra_instruments.put',
    put_many_fra_instruments_request: 'trading.v1.fra_instruments.put_many',
    delete_fra_instrument_request: 'trading.v1.fra_instruments.delete',
    delete_many_fra_instruments_request: 'trading.v1.fra_instruments.delete_many',
    list_fra_instrument_versions_request: 'trading.v1.fra_instruments_versions.list',
    get_fra_instrument_version_request: 'trading.v1.fra_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fra_instruments_request: true,
    get_fra_instrument_request: true,
    get_many_fra_instruments_request: true,
    put_fra_instrument_request: true,
    put_many_fra_instruments_request: true,
    delete_fra_instrument_request: true,
    delete_many_fra_instruments_request: true,
    list_fra_instrument_versions_request: true,
    get_fra_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.fra_instruments_events.created',
    updated: 'trading.v1.fra_instruments_events.updated',
    deleted: 'trading.v1.fra_instruments_events.deleted',
} as const;
