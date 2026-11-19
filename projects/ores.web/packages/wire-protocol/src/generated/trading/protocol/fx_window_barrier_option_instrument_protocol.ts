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
import type { FxWindowBarrierOptionInstrument } from '../domain/fx_window_barrier_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxWindowBarrierOptionInstrumentKey {
    trade_id: string;
}

export interface FxWindowBarrierOptionInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    currency: string;
    fixing_amount: string;
    strike: string;
    strike_currency: string | null;
    start_date: string;
    end_date: string;
    description: string;
}

export interface FxWindowBarrierOptionInstrumentChange {
    write: FxWindowBarrierOptionInstrumentWrite;
    precondition: Precondition;
}

export interface FxWindowBarrierOptionInstrumentRemoval {
    key: FxWindowBarrierOptionInstrumentKey;
    precondition: Precondition;
}

export interface FxWindowBarrierOptionInstrumentLookup {
    key: FxWindowBarrierOptionInstrumentKey;
    fx_window_barrier_option_instrument: FxWindowBarrierOptionInstrument | null;
}

export interface FxWindowBarrierOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface FxWindowBarrierOptionInstrumentEvent {
    event_id: string;
    key: FxWindowBarrierOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxWindowBarrierOptionInstrumentVersionKey {
    fx_window_barrier_option_instrument: FxWindowBarrierOptionInstrumentKey;
    version: number;
}

export interface FxWindowBarrierOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxWindowBarrierOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxWindowBarrierOptionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListFxWindowBarrierOptionInstrumentsResponse {
    result: Result;
    fx_window_barrier_option_instruments: FxWindowBarrierOptionInstrument[];
    total: number;
}

export interface GetFxWindowBarrierOptionInstrumentRequest {
    key: FxWindowBarrierOptionInstrumentKey;
}

export interface GetFxWindowBarrierOptionInstrumentResponse {
    result: Result;
    fx_window_barrier_option_instrument: FxWindowBarrierOptionInstrument | null;
}

export interface GetManyFxWindowBarrierOptionInstrumentsRequest {
    keys: FxWindowBarrierOptionInstrumentKey[];
}

export interface GetManyFxWindowBarrierOptionInstrumentsResponse {
    result: Result;
    entries: FxWindowBarrierOptionInstrumentLookup[];
}

export interface PutFxWindowBarrierOptionInstrumentRequest {
    change: FxWindowBarrierOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFxWindowBarrierOptionInstrumentResponse {
    result: Result;
    fx_window_barrier_option_instrument: FxWindowBarrierOptionInstrument | null;
}

export interface PutManyFxWindowBarrierOptionInstrumentsRequest {
    changes: FxWindowBarrierOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFxWindowBarrierOptionInstrumentsResponse {
    result: Result;
    fx_window_barrier_option_instruments: FxWindowBarrierOptionInstrument[];
}

export interface DeleteFxWindowBarrierOptionInstrumentRequest {
    removal: FxWindowBarrierOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxWindowBarrierOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyFxWindowBarrierOptionInstrumentsRequest {
    removals: FxWindowBarrierOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxWindowBarrierOptionInstrumentsResponse {
    result: Result;
}

export interface ListFxWindowBarrierOptionInstrumentVersionsRequest {
    key: FxWindowBarrierOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxWindowBarrierOptionInstrumentVersionsFilter | null;
}

export interface ListFxWindowBarrierOptionInstrumentVersionsResponse {
    result: Result;
    versions: FxWindowBarrierOptionInstrument[];
    total: number;
}

export interface GetFxWindowBarrierOptionInstrumentVersionRequest {
    key: FxWindowBarrierOptionInstrumentVersionKey;
}

export interface GetFxWindowBarrierOptionInstrumentVersionResponse {
    result: Result;
    version: FxWindowBarrierOptionInstrument | null;
}

export const subjects = {
    list_fx_window_barrier_option_instruments_request:
        'trading.v1.fx_window_barrier_option_instruments.list',
    get_fx_window_barrier_option_instrument_request:
        'trading.v1.fx_window_barrier_option_instruments.get',
    get_many_fx_window_barrier_option_instruments_request:
        'trading.v1.fx_window_barrier_option_instruments.get_many',
    put_fx_window_barrier_option_instrument_request:
        'trading.v1.fx_window_barrier_option_instruments.put',
    put_many_fx_window_barrier_option_instruments_request:
        'trading.v1.fx_window_barrier_option_instruments.put_many',
    delete_fx_window_barrier_option_instrument_request:
        'trading.v1.fx_window_barrier_option_instruments.delete',
    delete_many_fx_window_barrier_option_instruments_request:
        'trading.v1.fx_window_barrier_option_instruments.delete_many',
    list_fx_window_barrier_option_instrument_versions_request:
        'trading.v1.fx_window_barrier_option_instruments_versions.list',
    get_fx_window_barrier_option_instrument_version_request:
        'trading.v1.fx_window_barrier_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_window_barrier_option_instruments_request: true,
    get_fx_window_barrier_option_instrument_request: true,
    get_many_fx_window_barrier_option_instruments_request: true,
    put_fx_window_barrier_option_instrument_request: true,
    put_many_fx_window_barrier_option_instruments_request: true,
    delete_fx_window_barrier_option_instrument_request: true,
    delete_many_fx_window_barrier_option_instruments_request: true,
    list_fx_window_barrier_option_instrument_versions_request: true,
    get_fx_window_barrier_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.fx_window_barrier_option_instruments.created',
    updated: 'trading.v1.events.fx_window_barrier_option_instruments.updated',
    deleted: 'trading.v1.events.fx_window_barrier_option_instruments.deleted',
} as const;
