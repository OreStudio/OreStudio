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
import type { FxVarianceSwapInstrument } from '../domain/fx_variance_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxVarianceSwapInstrumentKey {
    trade_id: string;
}

export interface FxVarianceSwapInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    start_date: string;
    end_date: string;
    currency: string;
    underlying_code: string;
    long_short: string;
    strike: number;
    notional: string;
    moment_type: string;
    description: string;
}

export interface FxVarianceSwapInstrumentChange {
    write: FxVarianceSwapInstrumentWrite;
    precondition: Precondition;
}

export interface FxVarianceSwapInstrumentRemoval {
    key: FxVarianceSwapInstrumentKey;
    precondition: Precondition;
}

export interface FxVarianceSwapInstrumentLookup {
    key: FxVarianceSwapInstrumentKey;
    fx_variance_swap_instrument: FxVarianceSwapInstrument | null;
}

export interface FxVarianceSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface FxVarianceSwapInstrumentEvent {
    event_id: string;
    key: FxVarianceSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxVarianceSwapInstrumentVersionKey {
    fx_variance_swap_instrument: FxVarianceSwapInstrumentKey;
    version: number;
}

export interface FxVarianceSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxVarianceSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxVarianceSwapInstrumentsFilter | null;
}

export interface ListFxVarianceSwapInstrumentsResponse {
    result: Result;
    fx_variance_swap_instruments: FxVarianceSwapInstrument[];
    total: number;
}

export interface GetFxVarianceSwapInstrumentRequest {
    key: FxVarianceSwapInstrumentKey;
}

export interface GetFxVarianceSwapInstrumentResponse {
    result: Result;
    fx_variance_swap_instrument: FxVarianceSwapInstrument | null;
}

export interface GetManyFxVarianceSwapInstrumentsRequest {
    keys: FxVarianceSwapInstrumentKey[];
}

export interface GetManyFxVarianceSwapInstrumentsResponse {
    result: Result;
    entries: FxVarianceSwapInstrumentLookup[];
}

export interface PutFxVarianceSwapInstrumentRequest {
    change: FxVarianceSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFxVarianceSwapInstrumentResponse {
    result: Result;
    fx_variance_swap_instrument: FxVarianceSwapInstrument | null;
}

export interface PutManyFxVarianceSwapInstrumentsRequest {
    changes: FxVarianceSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFxVarianceSwapInstrumentsResponse {
    result: Result;
    fx_variance_swap_instruments: FxVarianceSwapInstrument[];
}

export interface DeleteFxVarianceSwapInstrumentRequest {
    removal: FxVarianceSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxVarianceSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyFxVarianceSwapInstrumentsRequest {
    removals: FxVarianceSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxVarianceSwapInstrumentsResponse {
    result: Result;
}

export interface ListFxVarianceSwapInstrumentVersionsRequest {
    key: FxVarianceSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVarianceSwapInstrumentVersionsFilter | null;
}

export interface ListFxVarianceSwapInstrumentVersionsResponse {
    result: Result;
    versions: FxVarianceSwapInstrument[];
    total: number;
}

export interface GetFxVarianceSwapInstrumentVersionRequest {
    key: FxVarianceSwapInstrumentVersionKey;
}

export interface GetFxVarianceSwapInstrumentVersionResponse {
    result: Result;
    version: FxVarianceSwapInstrument | null;
}

export const subjects = {
    list_fx_variance_swap_instruments_request: 'trading.v1.fx_variance_swap_instruments.list',
    get_fx_variance_swap_instrument_request: 'trading.v1.fx_variance_swap_instruments.get',
    get_many_fx_variance_swap_instruments_request:
        'trading.v1.fx_variance_swap_instruments.get_many',
    put_fx_variance_swap_instrument_request: 'trading.v1.fx_variance_swap_instruments.put',
    put_many_fx_variance_swap_instruments_request:
        'trading.v1.fx_variance_swap_instruments.put_many',
    delete_fx_variance_swap_instrument_request: 'trading.v1.fx_variance_swap_instruments.delete',
    delete_many_fx_variance_swap_instruments_request:
        'trading.v1.fx_variance_swap_instruments.delete_many',
    list_fx_variance_swap_instrument_versions_request:
        'trading.v1.fx_variance_swap_instruments_versions.list',
    get_fx_variance_swap_instrument_version_request:
        'trading.v1.fx_variance_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_variance_swap_instruments_request: true,
    get_fx_variance_swap_instrument_request: true,
    get_many_fx_variance_swap_instruments_request: true,
    put_fx_variance_swap_instrument_request: true,
    put_many_fx_variance_swap_instruments_request: true,
    delete_fx_variance_swap_instrument_request: true,
    delete_many_fx_variance_swap_instruments_request: true,
    list_fx_variance_swap_instrument_versions_request: true,
    get_fx_variance_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.fx_variance_swap_instruments_events.created',
    updated: 'trading.v1.fx_variance_swap_instruments_events.updated',
    deleted: 'trading.v1.fx_variance_swap_instruments_events.deleted',
} as const;
