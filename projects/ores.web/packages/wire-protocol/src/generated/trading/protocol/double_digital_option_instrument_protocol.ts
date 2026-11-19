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
import type { DoubleDigitalOptionInstrument } from '../domain/double_digital_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DoubleDigitalOptionInstrumentKey {
    trade_id: string;
}

export interface DoubleDigitalOptionInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    expiry: string;
    settlement: string;
    binary_payout: string;
    binary_level_1: string;
    binary_level_2: string;
    binary_level_upper_1: string | null;
    binary_level_upper_2: string | null;
    type_1: string;
    type_2: string;
    long_short: string;
    name_1: string | null;
    name_2: string | null;
    pay_currency: string;
    description: string;
}

export interface DoubleDigitalOptionInstrumentChange {
    write: DoubleDigitalOptionInstrumentWrite;
    precondition: Precondition;
}

export interface DoubleDigitalOptionInstrumentRemoval {
    key: DoubleDigitalOptionInstrumentKey;
    precondition: Precondition;
}

export interface DoubleDigitalOptionInstrumentLookup {
    key: DoubleDigitalOptionInstrumentKey;
    double_digital_option_instrument: DoubleDigitalOptionInstrument | null;
}

export interface DoubleDigitalOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface DoubleDigitalOptionInstrumentEvent {
    event_id: string;
    key: DoubleDigitalOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DoubleDigitalOptionInstrumentVersionKey {
    double_digital_option_instrument: DoubleDigitalOptionInstrumentKey;
    version: number;
}

export interface DoubleDigitalOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDoubleDigitalOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: DoubleDigitalOptionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListDoubleDigitalOptionInstrumentsResponse {
    result: Result;
    double_digital_option_instruments: DoubleDigitalOptionInstrument[];
    total: number;
}

export interface GetDoubleDigitalOptionInstrumentRequest {
    key: DoubleDigitalOptionInstrumentKey;
}

export interface GetDoubleDigitalOptionInstrumentResponse {
    result: Result;
    double_digital_option_instrument: DoubleDigitalOptionInstrument | null;
}

export interface GetManyDoubleDigitalOptionInstrumentsRequest {
    keys: DoubleDigitalOptionInstrumentKey[];
}

export interface GetManyDoubleDigitalOptionInstrumentsResponse {
    result: Result;
    entries: DoubleDigitalOptionInstrumentLookup[];
}

export interface PutDoubleDigitalOptionInstrumentRequest {
    change: DoubleDigitalOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutDoubleDigitalOptionInstrumentResponse {
    result: Result;
    double_digital_option_instrument: DoubleDigitalOptionInstrument | null;
}

export interface PutManyDoubleDigitalOptionInstrumentsRequest {
    changes: DoubleDigitalOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyDoubleDigitalOptionInstrumentsResponse {
    result: Result;
    double_digital_option_instruments: DoubleDigitalOptionInstrument[];
}

export interface DeleteDoubleDigitalOptionInstrumentRequest {
    removal: DoubleDigitalOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteDoubleDigitalOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyDoubleDigitalOptionInstrumentsRequest {
    removals: DoubleDigitalOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDoubleDigitalOptionInstrumentsResponse {
    result: Result;
}

export interface ListDoubleDigitalOptionInstrumentVersionsRequest {
    key: DoubleDigitalOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DoubleDigitalOptionInstrumentVersionsFilter | null;
}

export interface ListDoubleDigitalOptionInstrumentVersionsResponse {
    result: Result;
    versions: DoubleDigitalOptionInstrument[];
    total: number;
}

export interface GetDoubleDigitalOptionInstrumentVersionRequest {
    key: DoubleDigitalOptionInstrumentVersionKey;
}

export interface GetDoubleDigitalOptionInstrumentVersionResponse {
    result: Result;
    version: DoubleDigitalOptionInstrument | null;
}

export const subjects = {
    list_double_digital_option_instruments_request:
        'trading.v1.double_digital_option_instruments.list',
    get_double_digital_option_instrument_request:
        'trading.v1.double_digital_option_instruments.get',
    get_many_double_digital_option_instruments_request:
        'trading.v1.double_digital_option_instruments.get_many',
    put_double_digital_option_instrument_request:
        'trading.v1.double_digital_option_instruments.put',
    put_many_double_digital_option_instruments_request:
        'trading.v1.double_digital_option_instruments.put_many',
    delete_double_digital_option_instrument_request:
        'trading.v1.double_digital_option_instruments.delete',
    delete_many_double_digital_option_instruments_request:
        'trading.v1.double_digital_option_instruments.delete_many',
    list_double_digital_option_instrument_versions_request:
        'trading.v1.double_digital_option_instruments_versions.list',
    get_double_digital_option_instrument_version_request:
        'trading.v1.double_digital_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_double_digital_option_instruments_request: true,
    get_double_digital_option_instrument_request: true,
    get_many_double_digital_option_instruments_request: true,
    put_double_digital_option_instrument_request: true,
    put_many_double_digital_option_instruments_request: true,
    delete_double_digital_option_instrument_request: true,
    delete_many_double_digital_option_instruments_request: true,
    list_double_digital_option_instrument_versions_request: true,
    get_double_digital_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.double_digital_option_instruments.created',
    updated: 'trading.v1.events.double_digital_option_instruments.updated',
    deleted: 'trading.v1.events.double_digital_option_instruments.deleted',
} as const;
