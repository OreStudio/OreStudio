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
import type { AutocallableInstrument } from '../domain/autocallable_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface AutocallableInstrumentKey {
    trade_id: string;
}

export interface AutocallableInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    notional_amount: string;
    determination_level: string;
    trigger_level: string;
    long_short: string;
    pay_currency: string;
    cap: string;
    description: string;
}

export interface AutocallableInstrumentChange {
    write: AutocallableInstrumentWrite;
    precondition: Precondition;
}

export interface AutocallableInstrumentRemoval {
    key: AutocallableInstrumentKey;
    precondition: Precondition;
}

export interface AutocallableInstrumentLookup {
    key: AutocallableInstrumentKey;
    autocallable_instrument: AutocallableInstrument | null;
}

export interface AutocallableInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface AutocallableInstrumentEvent {
    event_id: string;
    key: AutocallableInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AutocallableInstrumentVersionKey {
    autocallable_instrument: AutocallableInstrumentKey;
    version: number;
}

export interface AutocallableInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAutocallableInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: AutocallableInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListAutocallableInstrumentsResponse {
    result: Result;
    autocallable_instruments: AutocallableInstrument[];
    total: number;
}

export interface GetAutocallableInstrumentRequest {
    key: AutocallableInstrumentKey;
}

export interface GetAutocallableInstrumentResponse {
    result: Result;
    autocallable_instrument: AutocallableInstrument | null;
}

export interface GetManyAutocallableInstrumentsRequest {
    keys: AutocallableInstrumentKey[];
}

export interface GetManyAutocallableInstrumentsResponse {
    result: Result;
    entries: AutocallableInstrumentLookup[];
}

export interface PutAutocallableInstrumentRequest {
    change: AutocallableInstrumentChange;
    intent: ChangeIntent;
}

export interface PutAutocallableInstrumentResponse {
    result: Result;
    autocallable_instrument: AutocallableInstrument | null;
}

export interface PutManyAutocallableInstrumentsRequest {
    changes: AutocallableInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyAutocallableInstrumentsResponse {
    result: Result;
    autocallable_instruments: AutocallableInstrument[];
}

export interface DeleteAutocallableInstrumentRequest {
    removal: AutocallableInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteAutocallableInstrumentResponse {
    result: Result;
}

export interface DeleteManyAutocallableInstrumentsRequest {
    removals: AutocallableInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAutocallableInstrumentsResponse {
    result: Result;
}

export interface ListAutocallableInstrumentVersionsRequest {
    key: AutocallableInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AutocallableInstrumentVersionsFilter | null;
}

export interface ListAutocallableInstrumentVersionsResponse {
    result: Result;
    versions: AutocallableInstrument[];
    total: number;
}

export interface GetAutocallableInstrumentVersionRequest {
    key: AutocallableInstrumentVersionKey;
}

export interface GetAutocallableInstrumentVersionResponse {
    result: Result;
    version: AutocallableInstrument | null;
}

export const subjects = {
    list_autocallable_instruments_request: 'trading.v1.autocallable_instruments.list',
    get_autocallable_instrument_request: 'trading.v1.autocallable_instruments.get',
    get_many_autocallable_instruments_request: 'trading.v1.autocallable_instruments.get_many',
    put_autocallable_instrument_request: 'trading.v1.autocallable_instruments.put',
    put_many_autocallable_instruments_request: 'trading.v1.autocallable_instruments.put_many',
    delete_autocallable_instrument_request: 'trading.v1.autocallable_instruments.delete',
    delete_many_autocallable_instruments_request: 'trading.v1.autocallable_instruments.delete_many',
    list_autocallable_instrument_versions_request:
        'trading.v1.autocallable_instruments_versions.list',
    get_autocallable_instrument_version_request: 'trading.v1.autocallable_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_autocallable_instruments_request: true,
    get_autocallable_instrument_request: true,
    get_many_autocallable_instruments_request: true,
    put_autocallable_instrument_request: true,
    put_many_autocallable_instruments_request: true,
    delete_autocallable_instrument_request: true,
    delete_many_autocallable_instruments_request: true,
    list_autocallable_instrument_versions_request: true,
    get_autocallable_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.autocallable_instruments.created',
    updated: 'trading.v1.events.autocallable_instruments.updated',
    deleted: 'trading.v1.events.autocallable_instruments.deleted',
} as const;
