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
import type { FxAsianForwardInstrument } from '../domain/fx_asian_forward_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxAsianForwardInstrumentKey {
    trade_id: string;
}

export interface FxAsianForwardInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    fx_index: string;
    reference_currency: string;
    reference_notional: string | null;
    settlement_currency: string;
    settlement_notional: string | null;
    payment_date: string | null;
    long_short: string;
    currency: string;
    fixing_amount: string | null;
    target_amount: string | null;
    strike: number | null;
    description: string;
}

export interface FxAsianForwardInstrumentChange {
    write: FxAsianForwardInstrumentWrite;
    precondition: Precondition;
}

export interface FxAsianForwardInstrumentRemoval {
    key: FxAsianForwardInstrumentKey;
    precondition: Precondition;
}

export interface FxAsianForwardInstrumentLookup {
    key: FxAsianForwardInstrumentKey;
    fx_asian_forward_instrument: FxAsianForwardInstrument | null;
}

export interface FxAsianForwardInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface FxAsianForwardInstrumentEvent {
    event_id: string;
    key: FxAsianForwardInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxAsianForwardInstrumentVersionKey {
    fx_asian_forward_instrument: FxAsianForwardInstrumentKey;
    version: number;
}

export interface FxAsianForwardInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxAsianForwardInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxAsianForwardInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListFxAsianForwardInstrumentsResponse {
    result: Result;
    fx_asian_forward_instruments: FxAsianForwardInstrument[];
    total: number;
}

export interface GetFxAsianForwardInstrumentRequest {
    key: FxAsianForwardInstrumentKey;
}

export interface GetFxAsianForwardInstrumentResponse {
    result: Result;
    fx_asian_forward_instrument: FxAsianForwardInstrument | null;
}

export interface GetManyFxAsianForwardInstrumentsRequest {
    keys: FxAsianForwardInstrumentKey[];
}

export interface GetManyFxAsianForwardInstrumentsResponse {
    result: Result;
    entries: FxAsianForwardInstrumentLookup[];
}

export interface PutFxAsianForwardInstrumentRequest {
    change: FxAsianForwardInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFxAsianForwardInstrumentResponse {
    result: Result;
    fx_asian_forward_instrument: FxAsianForwardInstrument | null;
}

export interface PutManyFxAsianForwardInstrumentsRequest {
    changes: FxAsianForwardInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFxAsianForwardInstrumentsResponse {
    result: Result;
    fx_asian_forward_instruments: FxAsianForwardInstrument[];
}

export interface DeleteFxAsianForwardInstrumentRequest {
    removal: FxAsianForwardInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxAsianForwardInstrumentResponse {
    result: Result;
}

export interface DeleteManyFxAsianForwardInstrumentsRequest {
    removals: FxAsianForwardInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxAsianForwardInstrumentsResponse {
    result: Result;
}

export interface ListFxAsianForwardInstrumentVersionsRequest {
    key: FxAsianForwardInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxAsianForwardInstrumentVersionsFilter | null;
}

export interface ListFxAsianForwardInstrumentVersionsResponse {
    result: Result;
    versions: FxAsianForwardInstrument[];
    total: number;
}

export interface GetFxAsianForwardInstrumentVersionRequest {
    key: FxAsianForwardInstrumentVersionKey;
}

export interface GetFxAsianForwardInstrumentVersionResponse {
    result: Result;
    version: FxAsianForwardInstrument | null;
}

export const subjects = {
    list_fx_asian_forward_instruments_request: 'trading.v1.fx_asian_forward_instruments.list',
    get_fx_asian_forward_instrument_request: 'trading.v1.fx_asian_forward_instruments.get',
    get_many_fx_asian_forward_instruments_request:
        'trading.v1.fx_asian_forward_instruments.get_many',
    put_fx_asian_forward_instrument_request: 'trading.v1.fx_asian_forward_instruments.put',
    put_many_fx_asian_forward_instruments_request:
        'trading.v1.fx_asian_forward_instruments.put_many',
    delete_fx_asian_forward_instrument_request: 'trading.v1.fx_asian_forward_instruments.delete',
    delete_many_fx_asian_forward_instruments_request:
        'trading.v1.fx_asian_forward_instruments.delete_many',
    list_fx_asian_forward_instrument_versions_request:
        'trading.v1.fx_asian_forward_instruments_versions.list',
    get_fx_asian_forward_instrument_version_request:
        'trading.v1.fx_asian_forward_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_asian_forward_instruments_request: true,
    get_fx_asian_forward_instrument_request: true,
    get_many_fx_asian_forward_instruments_request: true,
    put_fx_asian_forward_instrument_request: true,
    put_many_fx_asian_forward_instruments_request: true,
    delete_fx_asian_forward_instrument_request: true,
    delete_many_fx_asian_forward_instruments_request: true,
    list_fx_asian_forward_instrument_versions_request: true,
    get_fx_asian_forward_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.fx_asian_forward_instruments_events.created',
    updated: 'trading.v1.fx_asian_forward_instruments_events.updated',
    deleted: 'trading.v1.fx_asian_forward_instruments_events.deleted',
} as const;
