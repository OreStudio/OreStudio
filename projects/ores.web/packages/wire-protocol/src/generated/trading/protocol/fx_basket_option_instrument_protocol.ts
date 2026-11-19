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
import type { FxBasketOptionInstrument } from '../domain/fx_basket_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxBasketOptionInstrumentKey {
    trade_id: string;
}

export interface FxBasketOptionInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    currency: string;
    notional: string;
    strike: string | null;
    settlement_date: string | null;
    description: string;
}

export interface FxBasketOptionInstrumentChange {
    write: FxBasketOptionInstrumentWrite;
    precondition: Precondition;
}

export interface FxBasketOptionInstrumentRemoval {
    key: FxBasketOptionInstrumentKey;
    precondition: Precondition;
}

export interface FxBasketOptionInstrumentLookup {
    key: FxBasketOptionInstrumentKey;
    fx_basket_option_instrument: FxBasketOptionInstrument | null;
}

export interface FxBasketOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface FxBasketOptionInstrumentEvent {
    event_id: string;
    key: FxBasketOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxBasketOptionInstrumentVersionKey {
    fx_basket_option_instrument: FxBasketOptionInstrumentKey;
    version: number;
}

export interface FxBasketOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxBasketOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxBasketOptionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListFxBasketOptionInstrumentsResponse {
    result: Result;
    fx_basket_option_instruments: FxBasketOptionInstrument[];
    total: number;
}

export interface GetFxBasketOptionInstrumentRequest {
    key: FxBasketOptionInstrumentKey;
}

export interface GetFxBasketOptionInstrumentResponse {
    result: Result;
    fx_basket_option_instrument: FxBasketOptionInstrument | null;
}

export interface GetManyFxBasketOptionInstrumentsRequest {
    keys: FxBasketOptionInstrumentKey[];
}

export interface GetManyFxBasketOptionInstrumentsResponse {
    result: Result;
    entries: FxBasketOptionInstrumentLookup[];
}

export interface PutFxBasketOptionInstrumentRequest {
    change: FxBasketOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFxBasketOptionInstrumentResponse {
    result: Result;
    fx_basket_option_instrument: FxBasketOptionInstrument | null;
}

export interface PutManyFxBasketOptionInstrumentsRequest {
    changes: FxBasketOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFxBasketOptionInstrumentsResponse {
    result: Result;
    fx_basket_option_instruments: FxBasketOptionInstrument[];
}

export interface DeleteFxBasketOptionInstrumentRequest {
    removal: FxBasketOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxBasketOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyFxBasketOptionInstrumentsRequest {
    removals: FxBasketOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxBasketOptionInstrumentsResponse {
    result: Result;
}

export interface ListFxBasketOptionInstrumentVersionsRequest {
    key: FxBasketOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxBasketOptionInstrumentVersionsFilter | null;
}

export interface ListFxBasketOptionInstrumentVersionsResponse {
    result: Result;
    versions: FxBasketOptionInstrument[];
    total: number;
}

export interface GetFxBasketOptionInstrumentVersionRequest {
    key: FxBasketOptionInstrumentVersionKey;
}

export interface GetFxBasketOptionInstrumentVersionResponse {
    result: Result;
    version: FxBasketOptionInstrument | null;
}

export const subjects = {
    list_fx_basket_option_instruments_request: 'trading.v1.fx_basket_option_instruments.list',
    get_fx_basket_option_instrument_request: 'trading.v1.fx_basket_option_instruments.get',
    get_many_fx_basket_option_instruments_request:
        'trading.v1.fx_basket_option_instruments.get_many',
    put_fx_basket_option_instrument_request: 'trading.v1.fx_basket_option_instruments.put',
    put_many_fx_basket_option_instruments_request:
        'trading.v1.fx_basket_option_instruments.put_many',
    delete_fx_basket_option_instrument_request: 'trading.v1.fx_basket_option_instruments.delete',
    delete_many_fx_basket_option_instruments_request:
        'trading.v1.fx_basket_option_instruments.delete_many',
    list_fx_basket_option_instrument_versions_request:
        'trading.v1.fx_basket_option_instruments_versions.list',
    get_fx_basket_option_instrument_version_request:
        'trading.v1.fx_basket_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_basket_option_instruments_request: true,
    get_fx_basket_option_instrument_request: true,
    get_many_fx_basket_option_instruments_request: true,
    put_fx_basket_option_instrument_request: true,
    put_many_fx_basket_option_instruments_request: true,
    delete_fx_basket_option_instrument_request: true,
    delete_many_fx_basket_option_instruments_request: true,
    list_fx_basket_option_instrument_versions_request: true,
    get_fx_basket_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.fx_basket_option_instruments.created',
    updated: 'trading.v1.events.fx_basket_option_instruments.updated',
    deleted: 'trading.v1.events.fx_basket_option_instruments.deleted',
} as const;
