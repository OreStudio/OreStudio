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
import type { RateInstrument } from '../domain/rate_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RateInstrumentKey {
    trade_id: string;
}

export interface RateInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    start_date: string | null;
    maturity_date: string | null;
    description: string;
}

export interface RateInstrumentChange {
    write: RateInstrumentWrite;
    precondition: Precondition;
}

export interface RateInstrumentRemoval {
    key: RateInstrumentKey;
    precondition: Precondition;
}

export interface RateInstrumentLookup {
    key: RateInstrumentKey;
    rate_instrument: RateInstrument | null;
}

export interface RateInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface RateInstrumentEvent {
    event_id: string;
    key: RateInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface RateInstrumentVersionKey {
    rate_instrument: RateInstrumentKey;
    version: number;
}

export interface RateInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRateInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RateInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListRateInstrumentsResponse {
    result: Result;
    rate_instruments: RateInstrument[];
    total: number;
}

export interface GetRateInstrumentRequest {
    key: RateInstrumentKey;
}

export interface GetRateInstrumentResponse {
    result: Result;
    rate_instrument: RateInstrument | null;
}

export interface GetManyRateInstrumentsRequest {
    keys: RateInstrumentKey[];
}

export interface GetManyRateInstrumentsResponse {
    result: Result;
    entries: RateInstrumentLookup[];
}

export interface PutRateInstrumentRequest {
    change: RateInstrumentChange;
    intent: ChangeIntent;
}

export interface PutRateInstrumentResponse {
    result: Result;
    rate_instrument: RateInstrument | null;
}

export interface PutManyRateInstrumentsRequest {
    changes: RateInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyRateInstrumentsResponse {
    result: Result;
    rate_instruments: RateInstrument[];
}

export interface DeleteRateInstrumentRequest {
    removal: RateInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteRateInstrumentResponse {
    result: Result;
}

export interface DeleteManyRateInstrumentsRequest {
    removals: RateInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRateInstrumentsResponse {
    result: Result;
}

export interface ListRateInstrumentVersionsRequest {
    key: RateInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RateInstrumentVersionsFilter | null;
}

export interface ListRateInstrumentVersionsResponse {
    result: Result;
    versions: RateInstrument[];
    total: number;
}

export interface GetRateInstrumentVersionRequest {
    key: RateInstrumentVersionKey;
}

export interface GetRateInstrumentVersionResponse {
    result: Result;
    version: RateInstrument | null;
}

export const subjects = {
    list_rate_instruments_request: 'trading.v1.rate_instruments.list',
    get_rate_instrument_request: 'trading.v1.rate_instruments.get',
    get_many_rate_instruments_request: 'trading.v1.rate_instruments.get_many',
    put_rate_instrument_request: 'trading.v1.rate_instruments.put',
    put_many_rate_instruments_request: 'trading.v1.rate_instruments.put_many',
    delete_rate_instrument_request: 'trading.v1.rate_instruments.delete',
    delete_many_rate_instruments_request: 'trading.v1.rate_instruments.delete_many',
    list_rate_instrument_versions_request: 'trading.v1.rate_instruments_versions.list',
    get_rate_instrument_version_request: 'trading.v1.rate_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_rate_instruments_request: true,
    get_rate_instrument_request: true,
    get_many_rate_instruments_request: true,
    put_rate_instrument_request: true,
    put_many_rate_instruments_request: true,
    delete_rate_instrument_request: true,
    delete_many_rate_instruments_request: true,
    list_rate_instrument_versions_request: true,
    get_rate_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.rate_instruments_events.created',
    updated: 'trading.v1.rate_instruments_events.updated',
    deleted: 'trading.v1.rate_instruments_events.deleted',
} as const;
