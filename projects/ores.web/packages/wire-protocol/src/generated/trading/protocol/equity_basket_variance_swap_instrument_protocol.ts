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
import type { EquityBasketVarianceSwapInstrument } from '../domain/equity_basket_variance_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityBasketVarianceSwapInstrumentKey {
    trade_id: string;
}

export interface EquityBasketVarianceSwapInstrumentWrite {
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

export interface EquityBasketVarianceSwapInstrumentChange {
    write: EquityBasketVarianceSwapInstrumentWrite;
    precondition: Precondition;
}

export interface EquityBasketVarianceSwapInstrumentRemoval {
    key: EquityBasketVarianceSwapInstrumentKey;
    precondition: Precondition;
}

export interface EquityBasketVarianceSwapInstrumentLookup {
    key: EquityBasketVarianceSwapInstrumentKey;
    equity_basket_variance_swap_instrument: EquityBasketVarianceSwapInstrument | null;
}

export interface EquityBasketVarianceSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityBasketVarianceSwapInstrumentEvent {
    event_id: string;
    key: EquityBasketVarianceSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityBasketVarianceSwapInstrumentVersionKey {
    equity_basket_variance_swap_instrument: EquityBasketVarianceSwapInstrumentKey;
    version: number;
}

export interface EquityBasketVarianceSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityBasketVarianceSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityBasketVarianceSwapInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquityBasketVarianceSwapInstrumentsResponse {
    result: Result;
    equity_basket_variance_swap_instruments: EquityBasketVarianceSwapInstrument[];
    total: number;
}

export interface GetEquityBasketVarianceSwapInstrumentRequest {
    key: EquityBasketVarianceSwapInstrumentKey;
}

export interface GetEquityBasketVarianceSwapInstrumentResponse {
    result: Result;
    equity_basket_variance_swap_instrument: EquityBasketVarianceSwapInstrument | null;
}

export interface GetManyEquityBasketVarianceSwapInstrumentsRequest {
    keys: EquityBasketVarianceSwapInstrumentKey[];
}

export interface GetManyEquityBasketVarianceSwapInstrumentsResponse {
    result: Result;
    entries: EquityBasketVarianceSwapInstrumentLookup[];
}

export interface PutEquityBasketVarianceSwapInstrumentRequest {
    change: EquityBasketVarianceSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityBasketVarianceSwapInstrumentResponse {
    result: Result;
    equity_basket_variance_swap_instrument: EquityBasketVarianceSwapInstrument | null;
}

export interface PutManyEquityBasketVarianceSwapInstrumentsRequest {
    changes: EquityBasketVarianceSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityBasketVarianceSwapInstrumentsResponse {
    result: Result;
    equity_basket_variance_swap_instruments: EquityBasketVarianceSwapInstrument[];
}

export interface DeleteEquityBasketVarianceSwapInstrumentRequest {
    removal: EquityBasketVarianceSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityBasketVarianceSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityBasketVarianceSwapInstrumentsRequest {
    removals: EquityBasketVarianceSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityBasketVarianceSwapInstrumentsResponse {
    result: Result;
}

export interface ListEquityBasketVarianceSwapInstrumentVersionsRequest {
    key: EquityBasketVarianceSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityBasketVarianceSwapInstrumentVersionsFilter | null;
}

export interface ListEquityBasketVarianceSwapInstrumentVersionsResponse {
    result: Result;
    versions: EquityBasketVarianceSwapInstrument[];
    total: number;
}

export interface GetEquityBasketVarianceSwapInstrumentVersionRequest {
    key: EquityBasketVarianceSwapInstrumentVersionKey;
}

export interface GetEquityBasketVarianceSwapInstrumentVersionResponse {
    result: Result;
    version: EquityBasketVarianceSwapInstrument | null;
}

export const subjects = {
    list_equity_basket_variance_swap_instruments_request:
        'trading.v1.equity_basket_variance_swap_instruments.list',
    get_equity_basket_variance_swap_instrument_request:
        'trading.v1.equity_basket_variance_swap_instruments.get',
    get_many_equity_basket_variance_swap_instruments_request:
        'trading.v1.equity_basket_variance_swap_instruments.get_many',
    put_equity_basket_variance_swap_instrument_request:
        'trading.v1.equity_basket_variance_swap_instruments.put',
    put_many_equity_basket_variance_swap_instruments_request:
        'trading.v1.equity_basket_variance_swap_instruments.put_many',
    delete_equity_basket_variance_swap_instrument_request:
        'trading.v1.equity_basket_variance_swap_instruments.delete',
    delete_many_equity_basket_variance_swap_instruments_request:
        'trading.v1.equity_basket_variance_swap_instruments.delete_many',
    list_equity_basket_variance_swap_instrument_versions_request:
        'trading.v1.equity_basket_variance_swap_instruments_versions.list',
    get_equity_basket_variance_swap_instrument_version_request:
        'trading.v1.equity_basket_variance_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_basket_variance_swap_instruments_request: true,
    get_equity_basket_variance_swap_instrument_request: true,
    get_many_equity_basket_variance_swap_instruments_request: true,
    put_equity_basket_variance_swap_instrument_request: true,
    put_many_equity_basket_variance_swap_instruments_request: true,
    delete_equity_basket_variance_swap_instrument_request: true,
    delete_many_equity_basket_variance_swap_instruments_request: true,
    list_equity_basket_variance_swap_instrument_versions_request: true,
    get_equity_basket_variance_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.equity_basket_variance_swap_instruments.created',
    updated: 'trading.v1.events.equity_basket_variance_swap_instruments.updated',
    deleted: 'trading.v1.events.equity_basket_variance_swap_instruments.deleted',
} as const;
