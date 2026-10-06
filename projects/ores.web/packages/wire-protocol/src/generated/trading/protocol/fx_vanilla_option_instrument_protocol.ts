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
import type { FxVanillaOptionInstrument } from '../domain/fx_vanilla_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxVanillaOptionInstrumentKey {
    trade_id: string;
}

export interface FxVanillaOptionInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    bought_currency: string;
    bought_amount: string;
    sold_currency: string;
    sold_amount: string;
    option_type: string;
    expiry_date: string;
    exercise_style: string;
    settlement: string;
    description: string;
}

export interface FxVanillaOptionInstrumentChange {
    write: FxVanillaOptionInstrumentWrite;
    precondition: Precondition;
}

export interface FxVanillaOptionInstrumentRemoval {
    key: FxVanillaOptionInstrumentKey;
    precondition: Precondition;
}

export interface FxVanillaOptionInstrumentLookup {
    key: FxVanillaOptionInstrumentKey;
    fx_vanilla_option_instrument: FxVanillaOptionInstrument | null;
}

export interface FxVanillaOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface FxVanillaOptionInstrumentEvent {
    event_id: string;
    key: FxVanillaOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxVanillaOptionInstrumentVersionKey {
    fx_vanilla_option_instrument: FxVanillaOptionInstrumentKey;
    version: number;
}

export interface FxVanillaOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxVanillaOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxVanillaOptionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListFxVanillaOptionInstrumentsResponse {
    result: Result;
    fx_vanilla_option_instruments: FxVanillaOptionInstrument[];
    total: number;
}

export interface GetFxVanillaOptionInstrumentRequest {
    key: FxVanillaOptionInstrumentKey;
}

export interface GetFxVanillaOptionInstrumentResponse {
    result: Result;
    fx_vanilla_option_instrument: FxVanillaOptionInstrument | null;
}

export interface GetManyFxVanillaOptionInstrumentsRequest {
    keys: FxVanillaOptionInstrumentKey[];
}

export interface GetManyFxVanillaOptionInstrumentsResponse {
    result: Result;
    entries: FxVanillaOptionInstrumentLookup[];
}

export interface PutFxVanillaOptionInstrumentRequest {
    change: FxVanillaOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFxVanillaOptionInstrumentResponse {
    result: Result;
    fx_vanilla_option_instrument: FxVanillaOptionInstrument | null;
}

export interface PutManyFxVanillaOptionInstrumentsRequest {
    changes: FxVanillaOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFxVanillaOptionInstrumentsResponse {
    result: Result;
    fx_vanilla_option_instruments: FxVanillaOptionInstrument[];
}

export interface DeleteFxVanillaOptionInstrumentRequest {
    removal: FxVanillaOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxVanillaOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyFxVanillaOptionInstrumentsRequest {
    removals: FxVanillaOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxVanillaOptionInstrumentsResponse {
    result: Result;
}

export interface ListFxVanillaOptionInstrumentVersionsRequest {
    key: FxVanillaOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVanillaOptionInstrumentVersionsFilter | null;
}

export interface ListFxVanillaOptionInstrumentVersionsResponse {
    result: Result;
    versions: FxVanillaOptionInstrument[];
    total: number;
}

export interface GetFxVanillaOptionInstrumentVersionRequest {
    key: FxVanillaOptionInstrumentVersionKey;
}

export interface GetFxVanillaOptionInstrumentVersionResponse {
    result: Result;
    version: FxVanillaOptionInstrument | null;
}

export const subjects = {
    list_fx_vanilla_option_instruments_request: 'trading.v1.fx_vanilla_option_instruments.list',
    get_fx_vanilla_option_instrument_request: 'trading.v1.fx_vanilla_option_instruments.get',
    get_many_fx_vanilla_option_instruments_request:
        'trading.v1.fx_vanilla_option_instruments.get_many',
    put_fx_vanilla_option_instrument_request: 'trading.v1.fx_vanilla_option_instruments.put',
    put_many_fx_vanilla_option_instruments_request:
        'trading.v1.fx_vanilla_option_instruments.put_many',
    delete_fx_vanilla_option_instrument_request: 'trading.v1.fx_vanilla_option_instruments.delete',
    delete_many_fx_vanilla_option_instruments_request:
        'trading.v1.fx_vanilla_option_instruments.delete_many',
    list_fx_vanilla_option_instrument_versions_request:
        'trading.v1.fx_vanilla_option_instruments_versions.list',
    get_fx_vanilla_option_instrument_version_request:
        'trading.v1.fx_vanilla_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_vanilla_option_instruments_request: true,
    get_fx_vanilla_option_instrument_request: true,
    get_many_fx_vanilla_option_instruments_request: true,
    put_fx_vanilla_option_instrument_request: true,
    put_many_fx_vanilla_option_instruments_request: true,
    delete_fx_vanilla_option_instrument_request: true,
    delete_many_fx_vanilla_option_instruments_request: true,
    list_fx_vanilla_option_instrument_versions_request: true,
    get_fx_vanilla_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.fx_vanilla_option_instruments_events.created',
    updated: 'trading.v1.fx_vanilla_option_instruments_events.updated',
    deleted: 'trading.v1.fx_vanilla_option_instruments_events.deleted',
} as const;
