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
import type { FxBarrierOptionInstrument } from '../domain/fx_barrier_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxBarrierOptionInstrumentKey {
    trade_id: string;
}

export interface FxBarrierOptionInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    bought_currency: string;
    bought_amount: string;
    sold_currency: string;
    sold_amount: string;
    option_type: string;
    expiry_date: string;
    settlement: string;
    barrier_type: string;
    lower_barrier: number;
    upper_barrier: number | null;
    underlying_code: string;
    description: string;
}

export interface FxBarrierOptionInstrumentChange {
    write: FxBarrierOptionInstrumentWrite;
    precondition: Precondition;
}

export interface FxBarrierOptionInstrumentRemoval {
    key: FxBarrierOptionInstrumentKey;
    precondition: Precondition;
}

export interface FxBarrierOptionInstrumentLookup {
    key: FxBarrierOptionInstrumentKey;
    fx_barrier_option_instrument: FxBarrierOptionInstrument | null;
}

export interface FxBarrierOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface FxBarrierOptionInstrumentEvent {
    event_id: string;
    key: FxBarrierOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxBarrierOptionInstrumentVersionKey {
    fx_barrier_option_instrument: FxBarrierOptionInstrumentKey;
    version: number;
}

export interface FxBarrierOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxBarrierOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxBarrierOptionInstrumentsFilter | null;
}

export interface ListFxBarrierOptionInstrumentsResponse {
    result: Result;
    fx_barrier_option_instruments: FxBarrierOptionInstrument[];
    total: number;
}

export interface GetFxBarrierOptionInstrumentRequest {
    key: FxBarrierOptionInstrumentKey;
}

export interface GetFxBarrierOptionInstrumentResponse {
    result: Result;
    fx_barrier_option_instrument: FxBarrierOptionInstrument | null;
}

export interface GetManyFxBarrierOptionInstrumentsRequest {
    keys: FxBarrierOptionInstrumentKey[];
}

export interface GetManyFxBarrierOptionInstrumentsResponse {
    result: Result;
    entries: FxBarrierOptionInstrumentLookup[];
}

export interface PutFxBarrierOptionInstrumentRequest {
    change: FxBarrierOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFxBarrierOptionInstrumentResponse {
    result: Result;
    fx_barrier_option_instrument: FxBarrierOptionInstrument | null;
}

export interface PutManyFxBarrierOptionInstrumentsRequest {
    changes: FxBarrierOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFxBarrierOptionInstrumentsResponse {
    result: Result;
    fx_barrier_option_instruments: FxBarrierOptionInstrument[];
}

export interface DeleteFxBarrierOptionInstrumentRequest {
    removal: FxBarrierOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxBarrierOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyFxBarrierOptionInstrumentsRequest {
    removals: FxBarrierOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxBarrierOptionInstrumentsResponse {
    result: Result;
}

export interface ListFxBarrierOptionInstrumentVersionsRequest {
    key: FxBarrierOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxBarrierOptionInstrumentVersionsFilter | null;
}

export interface ListFxBarrierOptionInstrumentVersionsResponse {
    result: Result;
    versions: FxBarrierOptionInstrument[];
    total: number;
}

export interface GetFxBarrierOptionInstrumentVersionRequest {
    key: FxBarrierOptionInstrumentVersionKey;
}

export interface GetFxBarrierOptionInstrumentVersionResponse {
    result: Result;
    version: FxBarrierOptionInstrument | null;
}

export const subjects = {
    list_fx_barrier_option_instruments_request: 'trading.v1.fx_barrier_option_instruments.list',
    get_fx_barrier_option_instrument_request: 'trading.v1.fx_barrier_option_instruments.get',
    get_many_fx_barrier_option_instruments_request:
        'trading.v1.fx_barrier_option_instruments.get_many',
    put_fx_barrier_option_instrument_request: 'trading.v1.fx_barrier_option_instruments.put',
    put_many_fx_barrier_option_instruments_request:
        'trading.v1.fx_barrier_option_instruments.put_many',
    delete_fx_barrier_option_instrument_request: 'trading.v1.fx_barrier_option_instruments.delete',
    delete_many_fx_barrier_option_instruments_request:
        'trading.v1.fx_barrier_option_instruments.delete_many',
    list_fx_barrier_option_instrument_versions_request:
        'trading.v1.fx_barrier_option_instruments_versions.list',
    get_fx_barrier_option_instrument_version_request:
        'trading.v1.fx_barrier_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_barrier_option_instruments_request: true,
    get_fx_barrier_option_instrument_request: true,
    get_many_fx_barrier_option_instruments_request: true,
    put_fx_barrier_option_instrument_request: true,
    put_many_fx_barrier_option_instruments_request: true,
    delete_fx_barrier_option_instrument_request: true,
    delete_many_fx_barrier_option_instruments_request: true,
    list_fx_barrier_option_instrument_versions_request: true,
    get_fx_barrier_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.fx_barrier_option_instruments_events.created',
    updated: 'trading.v1.fx_barrier_option_instruments_events.updated',
    deleted: 'trading.v1.fx_barrier_option_instruments_events.deleted',
} as const;
