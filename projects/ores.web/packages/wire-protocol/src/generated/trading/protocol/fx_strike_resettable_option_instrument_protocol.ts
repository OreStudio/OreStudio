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
import type { FxStrikeResettableOptionInstrument } from '../domain/fx_strike_resettable_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxStrikeResettableOptionInstrumentKey {
    trade_id: string;
}

export interface FxStrikeResettableOptionInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    long_short: string;
    option_type: string;
    currency: string;
    strike: string;
    reset_strike: string;
    quantity: number;
    trigger_type: string;
    trigger_price: string;
    expiry_date: string;
    settlement_date: string;
    premium: string;
    premium_date: string;
    description: string;
}

export interface FxStrikeResettableOptionInstrumentChange {
    write: FxStrikeResettableOptionInstrumentWrite;
    precondition: Precondition;
}

export interface FxStrikeResettableOptionInstrumentRemoval {
    key: FxStrikeResettableOptionInstrumentKey;
    precondition: Precondition;
}

export interface FxStrikeResettableOptionInstrumentLookup {
    key: FxStrikeResettableOptionInstrumentKey;
    fx_strike_resettable_option_instrument: FxStrikeResettableOptionInstrument | null;
}

export interface FxStrikeResettableOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface FxStrikeResettableOptionInstrumentEvent {
    event_id: string;
    key: FxStrikeResettableOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxStrikeResettableOptionInstrumentVersionKey {
    fx_strike_resettable_option_instrument: FxStrikeResettableOptionInstrumentKey;
    version: number;
}

export interface FxStrikeResettableOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxStrikeResettableOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxStrikeResettableOptionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListFxStrikeResettableOptionInstrumentsResponse {
    result: Result;
    fx_strike_resettable_option_instruments: FxStrikeResettableOptionInstrument[];
    total: number;
}

export interface GetFxStrikeResettableOptionInstrumentRequest {
    key: FxStrikeResettableOptionInstrumentKey;
}

export interface GetFxStrikeResettableOptionInstrumentResponse {
    result: Result;
    fx_strike_resettable_option_instrument: FxStrikeResettableOptionInstrument | null;
}

export interface GetManyFxStrikeResettableOptionInstrumentsRequest {
    keys: FxStrikeResettableOptionInstrumentKey[];
}

export interface GetManyFxStrikeResettableOptionInstrumentsResponse {
    result: Result;
    entries: FxStrikeResettableOptionInstrumentLookup[];
}

export interface PutFxStrikeResettableOptionInstrumentRequest {
    change: FxStrikeResettableOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFxStrikeResettableOptionInstrumentResponse {
    result: Result;
    fx_strike_resettable_option_instrument: FxStrikeResettableOptionInstrument | null;
}

export interface PutManyFxStrikeResettableOptionInstrumentsRequest {
    changes: FxStrikeResettableOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFxStrikeResettableOptionInstrumentsResponse {
    result: Result;
    fx_strike_resettable_option_instruments: FxStrikeResettableOptionInstrument[];
}

export interface DeleteFxStrikeResettableOptionInstrumentRequest {
    removal: FxStrikeResettableOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxStrikeResettableOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyFxStrikeResettableOptionInstrumentsRequest {
    removals: FxStrikeResettableOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxStrikeResettableOptionInstrumentsResponse {
    result: Result;
}

export interface ListFxStrikeResettableOptionInstrumentVersionsRequest {
    key: FxStrikeResettableOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxStrikeResettableOptionInstrumentVersionsFilter | null;
}

export interface ListFxStrikeResettableOptionInstrumentVersionsResponse {
    result: Result;
    versions: FxStrikeResettableOptionInstrument[];
    total: number;
}

export interface GetFxStrikeResettableOptionInstrumentVersionRequest {
    key: FxStrikeResettableOptionInstrumentVersionKey;
}

export interface GetFxStrikeResettableOptionInstrumentVersionResponse {
    result: Result;
    version: FxStrikeResettableOptionInstrument | null;
}

export const subjects = {
    list_fx_strike_resettable_option_instruments_request:
        'trading.v1.fx_strike_resettable_option_instruments.list',
    get_fx_strike_resettable_option_instrument_request:
        'trading.v1.fx_strike_resettable_option_instruments.get',
    get_many_fx_strike_resettable_option_instruments_request:
        'trading.v1.fx_strike_resettable_option_instruments.get_many',
    put_fx_strike_resettable_option_instrument_request:
        'trading.v1.fx_strike_resettable_option_instruments.put',
    put_many_fx_strike_resettable_option_instruments_request:
        'trading.v1.fx_strike_resettable_option_instruments.put_many',
    delete_fx_strike_resettable_option_instrument_request:
        'trading.v1.fx_strike_resettable_option_instruments.delete',
    delete_many_fx_strike_resettable_option_instruments_request:
        'trading.v1.fx_strike_resettable_option_instruments.delete_many',
    list_fx_strike_resettable_option_instrument_versions_request:
        'trading.v1.fx_strike_resettable_option_instruments_versions.list',
    get_fx_strike_resettable_option_instrument_version_request:
        'trading.v1.fx_strike_resettable_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_strike_resettable_option_instruments_request: true,
    get_fx_strike_resettable_option_instrument_request: true,
    get_many_fx_strike_resettable_option_instruments_request: true,
    put_fx_strike_resettable_option_instrument_request: true,
    put_many_fx_strike_resettable_option_instruments_request: true,
    delete_fx_strike_resettable_option_instrument_request: true,
    delete_many_fx_strike_resettable_option_instruments_request: true,
    list_fx_strike_resettable_option_instrument_versions_request: true,
    get_fx_strike_resettable_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.fx_strike_resettable_option_instruments.created',
    updated: 'trading.v1.events.fx_strike_resettable_option_instruments.updated',
    deleted: 'trading.v1.events.fx_strike_resettable_option_instruments.deleted',
} as const;
