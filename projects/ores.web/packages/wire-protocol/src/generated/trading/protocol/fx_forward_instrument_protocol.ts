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
import type { FxForwardInstrument } from '../domain/fx_forward_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxForwardInstrumentKey {
    trade_id: string;
}

export interface FxForwardInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    bought_currency: string;
    bought_amount: string;
    sold_currency: string;
    sold_amount: string;
    value_date: string;
    settlement: string;
    description: string;
}

export interface FxForwardInstrumentChange {
    write: FxForwardInstrumentWrite;
    precondition: Precondition;
}

export interface FxForwardInstrumentRemoval {
    key: FxForwardInstrumentKey;
    precondition: Precondition;
}

export interface FxForwardInstrumentLookup {
    key: FxForwardInstrumentKey;
    fx_forward_instrument: FxForwardInstrument | null;
}

export interface FxForwardInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface FxForwardInstrumentEvent {
    event_id: string;
    key: FxForwardInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxForwardInstrumentVersionKey {
    fx_forward_instrument: FxForwardInstrumentKey;
    version: number;
}

export interface FxForwardInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxForwardInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxForwardInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListFxForwardInstrumentsResponse {
    result: Result;
    fx_forward_instruments: FxForwardInstrument[];
    total: number;
}

export interface GetFxForwardInstrumentRequest {
    key: FxForwardInstrumentKey;
}

export interface GetFxForwardInstrumentResponse {
    result: Result;
    fx_forward_instrument: FxForwardInstrument | null;
}

export interface GetManyFxForwardInstrumentsRequest {
    keys: FxForwardInstrumentKey[];
}

export interface GetManyFxForwardInstrumentsResponse {
    result: Result;
    entries: FxForwardInstrumentLookup[];
}

export interface PutFxForwardInstrumentRequest {
    change: FxForwardInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFxForwardInstrumentResponse {
    result: Result;
    fx_forward_instrument: FxForwardInstrument | null;
}

export interface PutManyFxForwardInstrumentsRequest {
    changes: FxForwardInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFxForwardInstrumentsResponse {
    result: Result;
    fx_forward_instruments: FxForwardInstrument[];
}

export interface DeleteFxForwardInstrumentRequest {
    removal: FxForwardInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxForwardInstrumentResponse {
    result: Result;
}

export interface DeleteManyFxForwardInstrumentsRequest {
    removals: FxForwardInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxForwardInstrumentsResponse {
    result: Result;
}

export interface ListFxForwardInstrumentVersionsRequest {
    key: FxForwardInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxForwardInstrumentVersionsFilter | null;
}

export interface ListFxForwardInstrumentVersionsResponse {
    result: Result;
    versions: FxForwardInstrument[];
    total: number;
}

export interface GetFxForwardInstrumentVersionRequest {
    key: FxForwardInstrumentVersionKey;
}

export interface GetFxForwardInstrumentVersionResponse {
    result: Result;
    version: FxForwardInstrument | null;
}

export const subjects = {
    list_fx_forward_instruments_request: 'trading.v1.fx_forward_instruments.list',
    get_fx_forward_instrument_request: 'trading.v1.fx_forward_instruments.get',
    get_many_fx_forward_instruments_request: 'trading.v1.fx_forward_instruments.get_many',
    put_fx_forward_instrument_request: 'trading.v1.fx_forward_instruments.put',
    put_many_fx_forward_instruments_request: 'trading.v1.fx_forward_instruments.put_many',
    delete_fx_forward_instrument_request: 'trading.v1.fx_forward_instruments.delete',
    delete_many_fx_forward_instruments_request: 'trading.v1.fx_forward_instruments.delete_many',
    list_fx_forward_instrument_versions_request: 'trading.v1.fx_forward_instruments_versions.list',
    get_fx_forward_instrument_version_request: 'trading.v1.fx_forward_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_forward_instruments_request: true,
    get_fx_forward_instrument_request: true,
    get_many_fx_forward_instruments_request: true,
    put_fx_forward_instrument_request: true,
    put_many_fx_forward_instruments_request: true,
    delete_fx_forward_instrument_request: true,
    delete_many_fx_forward_instruments_request: true,
    list_fx_forward_instrument_versions_request: true,
    get_fx_forward_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.fx_forward_instruments_events.created',
    updated: 'trading.v1.fx_forward_instruments_events.updated',
    deleted: 'trading.v1.fx_forward_instruments_events.deleted',
} as const;
