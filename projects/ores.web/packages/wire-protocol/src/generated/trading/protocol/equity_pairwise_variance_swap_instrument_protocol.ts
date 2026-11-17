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
import type { EquityPairwiseVarianceSwapInstrument } from '../domain/equity_pairwise_variance_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityPairwiseVarianceSwapInstrumentKey {
    trade_id: string;
}

export interface EquityPairwiseVarianceSwapInstrumentWrite {
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

export interface EquityPairwiseVarianceSwapInstrumentChange {
    write: EquityPairwiseVarianceSwapInstrumentWrite;
    precondition: Precondition;
}

export interface EquityPairwiseVarianceSwapInstrumentRemoval {
    key: EquityPairwiseVarianceSwapInstrumentKey;
    precondition: Precondition;
}

export interface EquityPairwiseVarianceSwapInstrumentLookup {
    key: EquityPairwiseVarianceSwapInstrumentKey;
    equity_pairwise_variance_swap_instrument: EquityPairwiseVarianceSwapInstrument | null;
}

export interface EquityPairwiseVarianceSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityPairwiseVarianceSwapInstrumentEvent {
    event_id: string;
    key: EquityPairwiseVarianceSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityPairwiseVarianceSwapInstrumentVersionKey {
    equity_pairwise_variance_swap_instrument: EquityPairwiseVarianceSwapInstrumentKey;
    version: number;
}

export interface EquityPairwiseVarianceSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityPairwiseVarianceSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityPairwiseVarianceSwapInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquityPairwiseVarianceSwapInstrumentsResponse {
    result: Result;
    equity_pairwise_variance_swap_instruments: EquityPairwiseVarianceSwapInstrument[];
    total: number;
}

export interface GetEquityPairwiseVarianceSwapInstrumentRequest {
    key: EquityPairwiseVarianceSwapInstrumentKey;
}

export interface GetEquityPairwiseVarianceSwapInstrumentResponse {
    result: Result;
    equity_pairwise_variance_swap_instrument: EquityPairwiseVarianceSwapInstrument | null;
}

export interface GetManyEquityPairwiseVarianceSwapInstrumentsRequest {
    keys: EquityPairwiseVarianceSwapInstrumentKey[];
}

export interface GetManyEquityPairwiseVarianceSwapInstrumentsResponse {
    result: Result;
    entries: EquityPairwiseVarianceSwapInstrumentLookup[];
}

export interface PutEquityPairwiseVarianceSwapInstrumentRequest {
    change: EquityPairwiseVarianceSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityPairwiseVarianceSwapInstrumentResponse {
    result: Result;
    equity_pairwise_variance_swap_instrument: EquityPairwiseVarianceSwapInstrument | null;
}

export interface PutManyEquityPairwiseVarianceSwapInstrumentsRequest {
    changes: EquityPairwiseVarianceSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityPairwiseVarianceSwapInstrumentsResponse {
    result: Result;
    equity_pairwise_variance_swap_instruments: EquityPairwiseVarianceSwapInstrument[];
}

export interface DeleteEquityPairwiseVarianceSwapInstrumentRequest {
    removal: EquityPairwiseVarianceSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityPairwiseVarianceSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityPairwiseVarianceSwapInstrumentsRequest {
    removals: EquityPairwiseVarianceSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityPairwiseVarianceSwapInstrumentsResponse {
    result: Result;
}

export interface ListEquityPairwiseVarianceSwapInstrumentVersionsRequest {
    key: EquityPairwiseVarianceSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityPairwiseVarianceSwapInstrumentVersionsFilter | null;
}

export interface ListEquityPairwiseVarianceSwapInstrumentVersionsResponse {
    result: Result;
    versions: EquityPairwiseVarianceSwapInstrument[];
    total: number;
}

export interface GetEquityPairwiseVarianceSwapInstrumentVersionRequest {
    key: EquityPairwiseVarianceSwapInstrumentVersionKey;
}

export interface GetEquityPairwiseVarianceSwapInstrumentVersionResponse {
    result: Result;
    version: EquityPairwiseVarianceSwapInstrument | null;
}

export const subjects = {
    list_equity_pairwise_variance_swap_instruments_request:
        'trading.v1.equity_pairwise_variance_swap_instruments.list',
    get_equity_pairwise_variance_swap_instrument_request:
        'trading.v1.equity_pairwise_variance_swap_instruments.get',
    get_many_equity_pairwise_variance_swap_instruments_request:
        'trading.v1.equity_pairwise_variance_swap_instruments.get_many',
    put_equity_pairwise_variance_swap_instrument_request:
        'trading.v1.equity_pairwise_variance_swap_instruments.put',
    put_many_equity_pairwise_variance_swap_instruments_request:
        'trading.v1.equity_pairwise_variance_swap_instruments.put_many',
    delete_equity_pairwise_variance_swap_instrument_request:
        'trading.v1.equity_pairwise_variance_swap_instruments.delete',
    delete_many_equity_pairwise_variance_swap_instruments_request:
        'trading.v1.equity_pairwise_variance_swap_instruments.delete_many',
    list_equity_pairwise_variance_swap_instrument_versions_request:
        'trading.v1.equity_pairwise_variance_swap_instruments_versions.list',
    get_equity_pairwise_variance_swap_instrument_version_request:
        'trading.v1.equity_pairwise_variance_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_pairwise_variance_swap_instruments_request: true,
    get_equity_pairwise_variance_swap_instrument_request: true,
    get_many_equity_pairwise_variance_swap_instruments_request: true,
    put_equity_pairwise_variance_swap_instrument_request: true,
    put_many_equity_pairwise_variance_swap_instruments_request: true,
    delete_equity_pairwise_variance_swap_instrument_request: true,
    delete_many_equity_pairwise_variance_swap_instruments_request: true,
    list_equity_pairwise_variance_swap_instrument_versions_request: true,
    get_equity_pairwise_variance_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.equity_pairwise_variance_swap_instruments.created',
    updated: 'trading.v1.events.equity_pairwise_variance_swap_instruments.updated',
    deleted: 'trading.v1.events.equity_pairwise_variance_swap_instruments.deleted',
} as const;
