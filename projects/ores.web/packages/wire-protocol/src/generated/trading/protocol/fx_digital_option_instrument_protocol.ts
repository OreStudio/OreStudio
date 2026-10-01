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
import type { FxDigitalOptionInstrument } from '../domain/fx_digital_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxDigitalOptionInstrumentKey {
    trade_id: string;
}

export interface FxDigitalOptionInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    foreign_currency: string;
    domestic_currency: string;
    payoff_currency: string;
    payoff_amount: string;
    option_type: string;
    expiry_date: string;
    long_short: string;
    strike: number | null;
    barrier_type: string;
    lower_barrier: number | null;
    upper_barrier: number | null;
    description: string;
}

export interface FxDigitalOptionInstrumentChange {
    write: FxDigitalOptionInstrumentWrite;
    precondition: Precondition;
}

export interface FxDigitalOptionInstrumentRemoval {
    key: FxDigitalOptionInstrumentKey;
    precondition: Precondition;
}

export interface FxDigitalOptionInstrumentLookup {
    key: FxDigitalOptionInstrumentKey;
    fx_digital_option_instrument: FxDigitalOptionInstrument | null;
}

export interface FxDigitalOptionInstrumentEvent {
    event_id: string;
    key: FxDigitalOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxDigitalOptionInstrumentVersionKey {
    fx_digital_option_instrument: FxDigitalOptionInstrumentKey;
    version: number;
}

export interface FxDigitalOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxDigitalOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListFxDigitalOptionInstrumentsResponse {
    result: Result;
    fx_digital_option_instruments: FxDigitalOptionInstrument[];
    total: number;
}

export interface GetFxDigitalOptionInstrumentRequest {
    key: FxDigitalOptionInstrumentKey;
}

export interface GetFxDigitalOptionInstrumentResponse {
    result: Result;
    fx_digital_option_instrument: FxDigitalOptionInstrument | null;
}

export interface GetManyFxDigitalOptionInstrumentsRequest {
    keys: FxDigitalOptionInstrumentKey[];
}

export interface GetManyFxDigitalOptionInstrumentsResponse {
    result: Result;
    entries: FxDigitalOptionInstrumentLookup[];
}

export interface PutFxDigitalOptionInstrumentRequest {
    change: FxDigitalOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFxDigitalOptionInstrumentResponse {
    result: Result;
    fx_digital_option_instrument: FxDigitalOptionInstrument | null;
}

export interface PutManyFxDigitalOptionInstrumentsRequest {
    changes: FxDigitalOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFxDigitalOptionInstrumentsResponse {
    result: Result;
    fx_digital_option_instruments: FxDigitalOptionInstrument[];
}

export interface DeleteFxDigitalOptionInstrumentRequest {
    removal: FxDigitalOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxDigitalOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyFxDigitalOptionInstrumentsRequest {
    removals: FxDigitalOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxDigitalOptionInstrumentsResponse {
    result: Result;
}

export interface ListFxDigitalOptionInstrumentVersionsRequest {
    key: FxDigitalOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxDigitalOptionInstrumentVersionsFilter | null;
}

export interface ListFxDigitalOptionInstrumentVersionsResponse {
    result: Result;
    versions: FxDigitalOptionInstrument[];
    total: number;
}

export interface GetFxDigitalOptionInstrumentVersionRequest {
    key: FxDigitalOptionInstrumentVersionKey;
}

export interface GetFxDigitalOptionInstrumentVersionResponse {
    result: Result;
    version: FxDigitalOptionInstrument | null;
}

export const subjects = {
    list_fx_digital_option_instruments_request: 'trading.v1.fx_digital_option_instruments.list',
    get_fx_digital_option_instrument_request: 'trading.v1.fx_digital_option_instruments.get',
    get_many_fx_digital_option_instruments_request:
        'trading.v1.fx_digital_option_instruments.get_many',
    put_fx_digital_option_instrument_request: 'trading.v1.fx_digital_option_instruments.put',
    put_many_fx_digital_option_instruments_request:
        'trading.v1.fx_digital_option_instruments.put_many',
    delete_fx_digital_option_instrument_request: 'trading.v1.fx_digital_option_instruments.delete',
    delete_many_fx_digital_option_instruments_request:
        'trading.v1.fx_digital_option_instruments.delete_many',
    list_fx_digital_option_instrument_versions_request:
        'trading.v1.fx_digital_option_instruments_versions.list',
    get_fx_digital_option_instrument_version_request:
        'trading.v1.fx_digital_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_digital_option_instruments_request: true,
    get_fx_digital_option_instrument_request: true,
    get_many_fx_digital_option_instruments_request: true,
    put_fx_digital_option_instrument_request: true,
    put_many_fx_digital_option_instruments_request: true,
    delete_fx_digital_option_instrument_request: true,
    delete_many_fx_digital_option_instruments_request: true,
    list_fx_digital_option_instrument_versions_request: true,
    get_fx_digital_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.fx_digital_option_instruments_events.created',
    updated: 'trading.v1.fx_digital_option_instruments_events.updated',
    deleted: 'trading.v1.fx_digital_option_instruments_events.deleted',
} as const;
