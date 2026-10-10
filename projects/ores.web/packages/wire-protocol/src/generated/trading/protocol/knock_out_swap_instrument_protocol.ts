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
import type { KnockOutSwapInstrument } from '../domain/knock_out_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface KnockOutSwapInstrumentKey {
    trade_id: string;
}

export interface KnockOutSwapInstrumentWrite {
    trade_id: string;
    trade_activity_id: string;
    barrier_start_date: string;
    barrier_level: string;
    barrier_type: string;
}

export interface KnockOutSwapInstrumentChange {
    write: KnockOutSwapInstrumentWrite;
    precondition: Precondition;
}

export interface KnockOutSwapInstrumentRemoval {
    key: KnockOutSwapInstrumentKey;
    precondition: Precondition;
}

export interface KnockOutSwapInstrumentLookup {
    key: KnockOutSwapInstrumentKey;
    knock_out_swap_instrument: KnockOutSwapInstrument | null;
}

export interface KnockOutSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface KnockOutSwapInstrumentEvent {
    event_id: string;
    key: KnockOutSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface KnockOutSwapInstrumentVersionKey {
    knock_out_swap_instrument: KnockOutSwapInstrumentKey;
    version: number;
}

export interface KnockOutSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListKnockOutSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: KnockOutSwapInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListKnockOutSwapInstrumentsResponse {
    result: Result;
    knock_out_swap_instruments: KnockOutSwapInstrument[];
    total: number;
}

export interface GetKnockOutSwapInstrumentRequest {
    key: KnockOutSwapInstrumentKey;
}

export interface GetKnockOutSwapInstrumentResponse {
    result: Result;
    knock_out_swap_instrument: KnockOutSwapInstrument | null;
}

export interface GetManyKnockOutSwapInstrumentsRequest {
    keys: KnockOutSwapInstrumentKey[];
}

export interface GetManyKnockOutSwapInstrumentsResponse {
    result: Result;
    entries: KnockOutSwapInstrumentLookup[];
}

export interface PutKnockOutSwapInstrumentRequest {
    change: KnockOutSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutKnockOutSwapInstrumentResponse {
    result: Result;
    knock_out_swap_instrument: KnockOutSwapInstrument | null;
}

export interface PutManyKnockOutSwapInstrumentsRequest {
    changes: KnockOutSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyKnockOutSwapInstrumentsResponse {
    result: Result;
    knock_out_swap_instruments: KnockOutSwapInstrument[];
}

export interface DeleteKnockOutSwapInstrumentRequest {
    removal: KnockOutSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteKnockOutSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyKnockOutSwapInstrumentsRequest {
    removals: KnockOutSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyKnockOutSwapInstrumentsResponse {
    result: Result;
}

export interface ListKnockOutSwapInstrumentVersionsRequest {
    key: KnockOutSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: KnockOutSwapInstrumentVersionsFilter | null;
}

export interface ListKnockOutSwapInstrumentVersionsResponse {
    result: Result;
    versions: KnockOutSwapInstrument[];
    total: number;
}

export interface GetKnockOutSwapInstrumentVersionRequest {
    key: KnockOutSwapInstrumentVersionKey;
}

export interface GetKnockOutSwapInstrumentVersionResponse {
    result: Result;
    version: KnockOutSwapInstrument | null;
}

export const subjects = {
    list_knock_out_swap_instruments_request: 'trading.v1.knock_out_swap_instruments.list',
    get_knock_out_swap_instrument_request: 'trading.v1.knock_out_swap_instruments.get',
    get_many_knock_out_swap_instruments_request: 'trading.v1.knock_out_swap_instruments.get_many',
    put_knock_out_swap_instrument_request: 'trading.v1.knock_out_swap_instruments.put',
    put_many_knock_out_swap_instruments_request: 'trading.v1.knock_out_swap_instruments.put_many',
    delete_knock_out_swap_instrument_request: 'trading.v1.knock_out_swap_instruments.delete',
    delete_many_knock_out_swap_instruments_request:
        'trading.v1.knock_out_swap_instruments.delete_many',
    list_knock_out_swap_instrument_versions_request:
        'trading.v1.knock_out_swap_instruments_versions.list',
    get_knock_out_swap_instrument_version_request:
        'trading.v1.knock_out_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_knock_out_swap_instruments_request: true,
    get_knock_out_swap_instrument_request: true,
    get_many_knock_out_swap_instruments_request: true,
    put_knock_out_swap_instrument_request: true,
    put_many_knock_out_swap_instruments_request: true,
    delete_knock_out_swap_instrument_request: true,
    delete_many_knock_out_swap_instruments_request: true,
    list_knock_out_swap_instrument_versions_request: true,
    get_knock_out_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.knock_out_swap_instruments_events.created',
    updated: 'trading.v1.knock_out_swap_instruments_events.updated',
    deleted: 'trading.v1.knock_out_swap_instruments_events.deleted',
} as const;
