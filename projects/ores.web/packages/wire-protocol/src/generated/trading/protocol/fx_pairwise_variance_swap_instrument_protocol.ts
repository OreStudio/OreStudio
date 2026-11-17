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
import type { FxPairwiseVarianceSwapInstrument } from '../domain/fx_pairwise_variance_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxPairwiseVarianceSwapInstrumentKey {
    trade_id: string;
}

export interface FxPairwiseVarianceSwapInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    first_underlying: string;
    second_underlying: string;
    first_strike: string;
    second_strike: string;
    first_notional: string;
    second_notional: string;
    basket_notional: string;
    basket_strike: string;
    currency: string;
    settlement_date: string;
    long_short: string;
    accrual_lag: number | null;
    payoff_limit: string | null;
    cap: string | null;
    floor: string | null;
    description: string;
}

export interface FxPairwiseVarianceSwapInstrumentChange {
    write: FxPairwiseVarianceSwapInstrumentWrite;
    precondition: Precondition;
}

export interface FxPairwiseVarianceSwapInstrumentRemoval {
    key: FxPairwiseVarianceSwapInstrumentKey;
    precondition: Precondition;
}

export interface FxPairwiseVarianceSwapInstrumentLookup {
    key: FxPairwiseVarianceSwapInstrumentKey;
    fx_pairwise_variance_swap_instrument: FxPairwiseVarianceSwapInstrument | null;
}

export interface FxPairwiseVarianceSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface FxPairwiseVarianceSwapInstrumentEvent {
    event_id: string;
    key: FxPairwiseVarianceSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxPairwiseVarianceSwapInstrumentVersionKey {
    fx_pairwise_variance_swap_instrument: FxPairwiseVarianceSwapInstrumentKey;
    version: number;
}

export interface FxPairwiseVarianceSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxPairwiseVarianceSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxPairwiseVarianceSwapInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListFxPairwiseVarianceSwapInstrumentsResponse {
    result: Result;
    fx_pairwise_variance_swap_instruments: FxPairwiseVarianceSwapInstrument[];
    total: number;
}

export interface GetFxPairwiseVarianceSwapInstrumentRequest {
    key: FxPairwiseVarianceSwapInstrumentKey;
}

export interface GetFxPairwiseVarianceSwapInstrumentResponse {
    result: Result;
    fx_pairwise_variance_swap_instrument: FxPairwiseVarianceSwapInstrument | null;
}

export interface GetManyFxPairwiseVarianceSwapInstrumentsRequest {
    keys: FxPairwiseVarianceSwapInstrumentKey[];
}

export interface GetManyFxPairwiseVarianceSwapInstrumentsResponse {
    result: Result;
    entries: FxPairwiseVarianceSwapInstrumentLookup[];
}

export interface PutFxPairwiseVarianceSwapInstrumentRequest {
    change: FxPairwiseVarianceSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFxPairwiseVarianceSwapInstrumentResponse {
    result: Result;
    fx_pairwise_variance_swap_instrument: FxPairwiseVarianceSwapInstrument | null;
}

export interface PutManyFxPairwiseVarianceSwapInstrumentsRequest {
    changes: FxPairwiseVarianceSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFxPairwiseVarianceSwapInstrumentsResponse {
    result: Result;
    fx_pairwise_variance_swap_instruments: FxPairwiseVarianceSwapInstrument[];
}

export interface DeleteFxPairwiseVarianceSwapInstrumentRequest {
    removal: FxPairwiseVarianceSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxPairwiseVarianceSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyFxPairwiseVarianceSwapInstrumentsRequest {
    removals: FxPairwiseVarianceSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxPairwiseVarianceSwapInstrumentsResponse {
    result: Result;
}

export interface ListFxPairwiseVarianceSwapInstrumentVersionsRequest {
    key: FxPairwiseVarianceSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxPairwiseVarianceSwapInstrumentVersionsFilter | null;
}

export interface ListFxPairwiseVarianceSwapInstrumentVersionsResponse {
    result: Result;
    versions: FxPairwiseVarianceSwapInstrument[];
    total: number;
}

export interface GetFxPairwiseVarianceSwapInstrumentVersionRequest {
    key: FxPairwiseVarianceSwapInstrumentVersionKey;
}

export interface GetFxPairwiseVarianceSwapInstrumentVersionResponse {
    result: Result;
    version: FxPairwiseVarianceSwapInstrument | null;
}

export const subjects = {
    list_fx_pairwise_variance_swap_instruments_request:
        'trading.v1.fx_pairwise_variance_swap_instruments.list',
    get_fx_pairwise_variance_swap_instrument_request:
        'trading.v1.fx_pairwise_variance_swap_instruments.get',
    get_many_fx_pairwise_variance_swap_instruments_request:
        'trading.v1.fx_pairwise_variance_swap_instruments.get_many',
    put_fx_pairwise_variance_swap_instrument_request:
        'trading.v1.fx_pairwise_variance_swap_instruments.put',
    put_many_fx_pairwise_variance_swap_instruments_request:
        'trading.v1.fx_pairwise_variance_swap_instruments.put_many',
    delete_fx_pairwise_variance_swap_instrument_request:
        'trading.v1.fx_pairwise_variance_swap_instruments.delete',
    delete_many_fx_pairwise_variance_swap_instruments_request:
        'trading.v1.fx_pairwise_variance_swap_instruments.delete_many',
    list_fx_pairwise_variance_swap_instrument_versions_request:
        'trading.v1.fx_pairwise_variance_swap_instruments_versions.list',
    get_fx_pairwise_variance_swap_instrument_version_request:
        'trading.v1.fx_pairwise_variance_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_pairwise_variance_swap_instruments_request: true,
    get_fx_pairwise_variance_swap_instrument_request: true,
    get_many_fx_pairwise_variance_swap_instruments_request: true,
    put_fx_pairwise_variance_swap_instrument_request: true,
    put_many_fx_pairwise_variance_swap_instruments_request: true,
    delete_fx_pairwise_variance_swap_instrument_request: true,
    delete_many_fx_pairwise_variance_swap_instruments_request: true,
    list_fx_pairwise_variance_swap_instrument_versions_request: true,
    get_fx_pairwise_variance_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.fx_pairwise_variance_swap_instruments.created',
    updated: 'trading.v1.events.fx_pairwise_variance_swap_instruments.updated',
    deleted: 'trading.v1.events.fx_pairwise_variance_swap_instruments.deleted',
} as const;
