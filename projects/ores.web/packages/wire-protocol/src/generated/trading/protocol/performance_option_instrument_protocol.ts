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
import type { PerformanceOptionInstrument } from '../domain/performance_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PerformanceOptionInstrumentKey {
    trade_id: string;
}

export interface PerformanceOptionInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    notional_amount: string;
    participation_rate: number;
    valuation_date: string;
    settlement_date: string;
    strike: string;
    strike_included: boolean | null;
    long_short: string;
    pay_currency: string;
    description: string;
}

export interface PerformanceOptionInstrumentChange {
    write: PerformanceOptionInstrumentWrite;
    precondition: Precondition;
}

export interface PerformanceOptionInstrumentRemoval {
    key: PerformanceOptionInstrumentKey;
    precondition: Precondition;
}

export interface PerformanceOptionInstrumentLookup {
    key: PerformanceOptionInstrumentKey;
    performance_option_instrument: PerformanceOptionInstrument | null;
}

export interface PerformanceOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface PerformanceOptionInstrumentEvent {
    event_id: string;
    key: PerformanceOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PerformanceOptionInstrumentVersionKey {
    performance_option_instrument: PerformanceOptionInstrumentKey;
    version: number;
}

export interface PerformanceOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPerformanceOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: PerformanceOptionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListPerformanceOptionInstrumentsResponse {
    result: Result;
    performance_option_instruments: PerformanceOptionInstrument[];
    total: number;
}

export interface GetPerformanceOptionInstrumentRequest {
    key: PerformanceOptionInstrumentKey;
}

export interface GetPerformanceOptionInstrumentResponse {
    result: Result;
    performance_option_instrument: PerformanceOptionInstrument | null;
}

export interface GetManyPerformanceOptionInstrumentsRequest {
    keys: PerformanceOptionInstrumentKey[];
}

export interface GetManyPerformanceOptionInstrumentsResponse {
    result: Result;
    entries: PerformanceOptionInstrumentLookup[];
}

export interface PutPerformanceOptionInstrumentRequest {
    change: PerformanceOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutPerformanceOptionInstrumentResponse {
    result: Result;
    performance_option_instrument: PerformanceOptionInstrument | null;
}

export interface PutManyPerformanceOptionInstrumentsRequest {
    changes: PerformanceOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyPerformanceOptionInstrumentsResponse {
    result: Result;
    performance_option_instruments: PerformanceOptionInstrument[];
}

export interface DeletePerformanceOptionInstrumentRequest {
    removal: PerformanceOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeletePerformanceOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyPerformanceOptionInstrumentsRequest {
    removals: PerformanceOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPerformanceOptionInstrumentsResponse {
    result: Result;
}

export interface ListPerformanceOptionInstrumentVersionsRequest {
    key: PerformanceOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PerformanceOptionInstrumentVersionsFilter | null;
}

export interface ListPerformanceOptionInstrumentVersionsResponse {
    result: Result;
    versions: PerformanceOptionInstrument[];
    total: number;
}

export interface GetPerformanceOptionInstrumentVersionRequest {
    key: PerformanceOptionInstrumentVersionKey;
}

export interface GetPerformanceOptionInstrumentVersionResponse {
    result: Result;
    version: PerformanceOptionInstrument | null;
}

export const subjects = {
    list_performance_option_instruments_request: 'trading.v1.performance_option_instruments.list',
    get_performance_option_instrument_request: 'trading.v1.performance_option_instruments.get',
    get_many_performance_option_instruments_request:
        'trading.v1.performance_option_instruments.get_many',
    put_performance_option_instrument_request: 'trading.v1.performance_option_instruments.put',
    put_many_performance_option_instruments_request:
        'trading.v1.performance_option_instruments.put_many',
    delete_performance_option_instrument_request:
        'trading.v1.performance_option_instruments.delete',
    delete_many_performance_option_instruments_request:
        'trading.v1.performance_option_instruments.delete_many',
    list_performance_option_instrument_versions_request:
        'trading.v1.performance_option_instruments_versions.list',
    get_performance_option_instrument_version_request:
        'trading.v1.performance_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_performance_option_instruments_request: true,
    get_performance_option_instrument_request: true,
    get_many_performance_option_instruments_request: true,
    put_performance_option_instrument_request: true,
    put_many_performance_option_instruments_request: true,
    delete_performance_option_instrument_request: true,
    delete_many_performance_option_instruments_request: true,
    list_performance_option_instrument_versions_request: true,
    get_performance_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.performance_option_instruments.created',
    updated: 'trading.v1.events.performance_option_instruments.updated',
    deleted: 'trading.v1.events.performance_option_instruments.deleted',
} as const;
