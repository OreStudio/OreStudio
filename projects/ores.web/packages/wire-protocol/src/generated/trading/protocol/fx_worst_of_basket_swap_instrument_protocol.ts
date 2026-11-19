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
import type { FxWorstOfBasketSwapInstrument } from '../domain/fx_worst_of_basket_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxWorstOfBasketSwapInstrumentKey {
    trade_id: string;
}

export interface FxWorstOfBasketSwapInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    long_short: string;
    currency: string;
    quantity: number;
    fixed_rate: string;
    floating_index: string;
    floating_day_count_fraction: string;
    initial_fixed_rate: string | null;
    initial_fixed_pay_date: string | null;
    knock_in_pay_date: string | null;
    bermudan_knock_in: boolean | null;
    knock_in_level: string | null;
    accumulating_fixed_coupons: boolean | null;
    accruing_fixed_coupons: boolean | null;
    floating_spread: string | null;
    floating_lookback: string | null;
    floating_rate_cutoff: string | null;
    is_averaged: boolean | null;
    include_spread: boolean | null;
    strike: string | null;
    description: string;
}

export interface FxWorstOfBasketSwapInstrumentChange {
    write: FxWorstOfBasketSwapInstrumentWrite;
    precondition: Precondition;
}

export interface FxWorstOfBasketSwapInstrumentRemoval {
    key: FxWorstOfBasketSwapInstrumentKey;
    precondition: Precondition;
}

export interface FxWorstOfBasketSwapInstrumentLookup {
    key: FxWorstOfBasketSwapInstrumentKey;
    fx_worst_of_basket_swap_instrument: FxWorstOfBasketSwapInstrument | null;
}

export interface FxWorstOfBasketSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface FxWorstOfBasketSwapInstrumentEvent {
    event_id: string;
    key: FxWorstOfBasketSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxWorstOfBasketSwapInstrumentVersionKey {
    fx_worst_of_basket_swap_instrument: FxWorstOfBasketSwapInstrumentKey;
    version: number;
}

export interface FxWorstOfBasketSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxWorstOfBasketSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxWorstOfBasketSwapInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListFxWorstOfBasketSwapInstrumentsResponse {
    result: Result;
    fx_worst_of_basket_swap_instruments: FxWorstOfBasketSwapInstrument[];
    total: number;
}

export interface GetFxWorstOfBasketSwapInstrumentRequest {
    key: FxWorstOfBasketSwapInstrumentKey;
}

export interface GetFxWorstOfBasketSwapInstrumentResponse {
    result: Result;
    fx_worst_of_basket_swap_instrument: FxWorstOfBasketSwapInstrument | null;
}

export interface GetManyFxWorstOfBasketSwapInstrumentsRequest {
    keys: FxWorstOfBasketSwapInstrumentKey[];
}

export interface GetManyFxWorstOfBasketSwapInstrumentsResponse {
    result: Result;
    entries: FxWorstOfBasketSwapInstrumentLookup[];
}

export interface PutFxWorstOfBasketSwapInstrumentRequest {
    change: FxWorstOfBasketSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFxWorstOfBasketSwapInstrumentResponse {
    result: Result;
    fx_worst_of_basket_swap_instrument: FxWorstOfBasketSwapInstrument | null;
}

export interface PutManyFxWorstOfBasketSwapInstrumentsRequest {
    changes: FxWorstOfBasketSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFxWorstOfBasketSwapInstrumentsResponse {
    result: Result;
    fx_worst_of_basket_swap_instruments: FxWorstOfBasketSwapInstrument[];
}

export interface DeleteFxWorstOfBasketSwapInstrumentRequest {
    removal: FxWorstOfBasketSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxWorstOfBasketSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyFxWorstOfBasketSwapInstrumentsRequest {
    removals: FxWorstOfBasketSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxWorstOfBasketSwapInstrumentsResponse {
    result: Result;
}

export interface ListFxWorstOfBasketSwapInstrumentVersionsRequest {
    key: FxWorstOfBasketSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxWorstOfBasketSwapInstrumentVersionsFilter | null;
}

export interface ListFxWorstOfBasketSwapInstrumentVersionsResponse {
    result: Result;
    versions: FxWorstOfBasketSwapInstrument[];
    total: number;
}

export interface GetFxWorstOfBasketSwapInstrumentVersionRequest {
    key: FxWorstOfBasketSwapInstrumentVersionKey;
}

export interface GetFxWorstOfBasketSwapInstrumentVersionResponse {
    result: Result;
    version: FxWorstOfBasketSwapInstrument | null;
}

export const subjects = {
    list_fx_worst_of_basket_swap_instruments_request:
        'trading.v1.fx_worst_of_basket_swap_instruments.list',
    get_fx_worst_of_basket_swap_instrument_request:
        'trading.v1.fx_worst_of_basket_swap_instruments.get',
    get_many_fx_worst_of_basket_swap_instruments_request:
        'trading.v1.fx_worst_of_basket_swap_instruments.get_many',
    put_fx_worst_of_basket_swap_instrument_request:
        'trading.v1.fx_worst_of_basket_swap_instruments.put',
    put_many_fx_worst_of_basket_swap_instruments_request:
        'trading.v1.fx_worst_of_basket_swap_instruments.put_many',
    delete_fx_worst_of_basket_swap_instrument_request:
        'trading.v1.fx_worst_of_basket_swap_instruments.delete',
    delete_many_fx_worst_of_basket_swap_instruments_request:
        'trading.v1.fx_worst_of_basket_swap_instruments.delete_many',
    list_fx_worst_of_basket_swap_instrument_versions_request:
        'trading.v1.fx_worst_of_basket_swap_instruments_versions.list',
    get_fx_worst_of_basket_swap_instrument_version_request:
        'trading.v1.fx_worst_of_basket_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_worst_of_basket_swap_instruments_request: true,
    get_fx_worst_of_basket_swap_instrument_request: true,
    get_many_fx_worst_of_basket_swap_instruments_request: true,
    put_fx_worst_of_basket_swap_instrument_request: true,
    put_many_fx_worst_of_basket_swap_instruments_request: true,
    delete_fx_worst_of_basket_swap_instrument_request: true,
    delete_many_fx_worst_of_basket_swap_instruments_request: true,
    list_fx_worst_of_basket_swap_instrument_versions_request: true,
    get_fx_worst_of_basket_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.fx_worst_of_basket_swap_instruments.created',
    updated: 'trading.v1.events.fx_worst_of_basket_swap_instruments.updated',
    deleted: 'trading.v1.events.fx_worst_of_basket_swap_instruments.deleted',
} as const;
