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
import type { FxBasketVarianceSwapInstrument } from '../domain/fx_basket_variance_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxBasketVarianceSwapInstrumentKey {
    trade_id: string;
}

export interface FxBasketVarianceSwapInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    long_short: string;
    strike: string;
    notional: string;
    squared_payoff: boolean;
    cap: string | null;
    floor: string | null;
    settlement_date: string;
    currency: string;
    description: string;
}

export interface FxBasketVarianceSwapInstrumentChange {
    write: FxBasketVarianceSwapInstrumentWrite;
    precondition: Precondition;
}

export interface FxBasketVarianceSwapInstrumentRemoval {
    key: FxBasketVarianceSwapInstrumentKey;
    precondition: Precondition;
}

export interface FxBasketVarianceSwapInstrumentLookup {
    key: FxBasketVarianceSwapInstrumentKey;
    fx_basket_variance_swap_instrument: FxBasketVarianceSwapInstrument | null;
}

export interface FxBasketVarianceSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface FxBasketVarianceSwapInstrumentEvent {
    event_id: string;
    key: FxBasketVarianceSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxBasketVarianceSwapInstrumentVersionKey {
    fx_basket_variance_swap_instrument: FxBasketVarianceSwapInstrumentKey;
    version: number;
}

export interface FxBasketVarianceSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxBasketVarianceSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxBasketVarianceSwapInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListFxBasketVarianceSwapInstrumentsResponse {
    result: Result;
    fx_basket_variance_swap_instruments: FxBasketVarianceSwapInstrument[];
    total: number;
}

export interface GetFxBasketVarianceSwapInstrumentRequest {
    key: FxBasketVarianceSwapInstrumentKey;
}

export interface GetFxBasketVarianceSwapInstrumentResponse {
    result: Result;
    fx_basket_variance_swap_instrument: FxBasketVarianceSwapInstrument | null;
}

export interface GetManyFxBasketVarianceSwapInstrumentsRequest {
    keys: FxBasketVarianceSwapInstrumentKey[];
}

export interface GetManyFxBasketVarianceSwapInstrumentsResponse {
    result: Result;
    entries: FxBasketVarianceSwapInstrumentLookup[];
}

export interface PutFxBasketVarianceSwapInstrumentRequest {
    change: FxBasketVarianceSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFxBasketVarianceSwapInstrumentResponse {
    result: Result;
    fx_basket_variance_swap_instrument: FxBasketVarianceSwapInstrument | null;
}

export interface PutManyFxBasketVarianceSwapInstrumentsRequest {
    changes: FxBasketVarianceSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFxBasketVarianceSwapInstrumentsResponse {
    result: Result;
    fx_basket_variance_swap_instruments: FxBasketVarianceSwapInstrument[];
}

export interface DeleteFxBasketVarianceSwapInstrumentRequest {
    removal: FxBasketVarianceSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxBasketVarianceSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyFxBasketVarianceSwapInstrumentsRequest {
    removals: FxBasketVarianceSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxBasketVarianceSwapInstrumentsResponse {
    result: Result;
}

export interface ListFxBasketVarianceSwapInstrumentVersionsRequest {
    key: FxBasketVarianceSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxBasketVarianceSwapInstrumentVersionsFilter | null;
}

export interface ListFxBasketVarianceSwapInstrumentVersionsResponse {
    result: Result;
    versions: FxBasketVarianceSwapInstrument[];
    total: number;
}

export interface GetFxBasketVarianceSwapInstrumentVersionRequest {
    key: FxBasketVarianceSwapInstrumentVersionKey;
}

export interface GetFxBasketVarianceSwapInstrumentVersionResponse {
    result: Result;
    version: FxBasketVarianceSwapInstrument | null;
}

export const subjects = {
    list_fx_basket_variance_swap_instruments_request:
        'trading.v1.fx_basket_variance_swap_instruments.list',
    get_fx_basket_variance_swap_instrument_request:
        'trading.v1.fx_basket_variance_swap_instruments.get',
    get_many_fx_basket_variance_swap_instruments_request:
        'trading.v1.fx_basket_variance_swap_instruments.get_many',
    put_fx_basket_variance_swap_instrument_request:
        'trading.v1.fx_basket_variance_swap_instruments.put',
    put_many_fx_basket_variance_swap_instruments_request:
        'trading.v1.fx_basket_variance_swap_instruments.put_many',
    delete_fx_basket_variance_swap_instrument_request:
        'trading.v1.fx_basket_variance_swap_instruments.delete',
    delete_many_fx_basket_variance_swap_instruments_request:
        'trading.v1.fx_basket_variance_swap_instruments.delete_many',
    list_fx_basket_variance_swap_instrument_versions_request:
        'trading.v1.fx_basket_variance_swap_instruments_versions.list',
    get_fx_basket_variance_swap_instrument_version_request:
        'trading.v1.fx_basket_variance_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_basket_variance_swap_instruments_request: true,
    get_fx_basket_variance_swap_instrument_request: true,
    get_many_fx_basket_variance_swap_instruments_request: true,
    put_fx_basket_variance_swap_instrument_request: true,
    put_many_fx_basket_variance_swap_instruments_request: true,
    delete_fx_basket_variance_swap_instrument_request: true,
    delete_many_fx_basket_variance_swap_instruments_request: true,
    list_fx_basket_variance_swap_instrument_versions_request: true,
    get_fx_basket_variance_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.fx_basket_variance_swap_instruments.created',
    updated: 'trading.v1.events.fx_basket_variance_swap_instruments.updated',
    deleted: 'trading.v1.events.fx_basket_variance_swap_instruments.deleted',
} as const;
