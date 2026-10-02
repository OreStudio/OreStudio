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
import type { InstrumentOptionPaymentDate } from '../domain/instrument_option_payment_date.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InstrumentOptionPaymentDateKey {
    trade_id: string;
    sequence_number: number;
}

export interface InstrumentOptionPaymentDateWrite {
    trade_id: string;
    sequence_number: number;
    payment_date: string;
}

export interface InstrumentOptionPaymentDateChange {
    write: InstrumentOptionPaymentDateWrite;
    precondition: Precondition;
}

export interface InstrumentOptionPaymentDateRemoval {
    key: InstrumentOptionPaymentDateKey;
    precondition: Precondition;
}

export interface InstrumentOptionPaymentDateLookup {
    key: InstrumentOptionPaymentDateKey;
    instrument_option_payment_date: InstrumentOptionPaymentDate | null;
}

export interface InstrumentOptionPaymentDateEvent {
    event_id: string;
    key: InstrumentOptionPaymentDateKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InstrumentOptionPaymentDateVersionKey {
    instrument_option_payment_date: InstrumentOptionPaymentDateKey;
    version: number;
}

export interface InstrumentOptionPaymentDateVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInstrumentOptionPaymentDatesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListInstrumentOptionPaymentDatesResponse {
    result: Result;
    option_payment_dates: InstrumentOptionPaymentDate[];
    total: number;
}

export interface GetInstrumentOptionPaymentDateRequest {
    key: InstrumentOptionPaymentDateKey;
}

export interface GetInstrumentOptionPaymentDateResponse {
    result: Result;
    instrument_option_payment_date: InstrumentOptionPaymentDate | null;
}

export interface GetManyInstrumentOptionPaymentDatesRequest {
    keys: InstrumentOptionPaymentDateKey[];
}

export interface GetManyInstrumentOptionPaymentDatesResponse {
    result: Result;
    entries: InstrumentOptionPaymentDateLookup[];
}

export interface PutInstrumentOptionPaymentDateRequest {
    change: InstrumentOptionPaymentDateChange;
    intent: ChangeIntent;
}

export interface PutInstrumentOptionPaymentDateResponse {
    result: Result;
    instrument_option_payment_date: InstrumentOptionPaymentDate | null;
}

export interface PutManyInstrumentOptionPaymentDatesRequest {
    changes: InstrumentOptionPaymentDateChange[];
    intent: ChangeIntent;
}

export interface PutManyInstrumentOptionPaymentDatesResponse {
    result: Result;
    option_payment_dates: InstrumentOptionPaymentDate[];
}

export interface DeleteInstrumentOptionPaymentDateRequest {
    removal: InstrumentOptionPaymentDateRemoval;
    intent: ChangeIntent;
}

export interface DeleteInstrumentOptionPaymentDateResponse {
    result: Result;
}

export interface DeleteManyInstrumentOptionPaymentDatesRequest {
    removals: InstrumentOptionPaymentDateRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInstrumentOptionPaymentDatesResponse {
    result: Result;
}

export interface ListInstrumentOptionPaymentDateVersionsRequest {
    key: InstrumentOptionPaymentDateKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentOptionPaymentDateVersionsFilter | null;
}

export interface ListInstrumentOptionPaymentDateVersionsResponse {
    result: Result;
    versions: InstrumentOptionPaymentDate[];
    total: number;
}

export interface GetInstrumentOptionPaymentDateVersionRequest {
    key: InstrumentOptionPaymentDateVersionKey;
}

export interface GetInstrumentOptionPaymentDateVersionResponse {
    result: Result;
    version: InstrumentOptionPaymentDate | null;
}

export const subjects = {
    list_instrument_option_payment_dates_request: 'trading.v1.instrument_option_payment_dates.list',
    get_instrument_option_payment_date_request: 'trading.v1.instrument_option_payment_dates.get',
    get_many_instrument_option_payment_dates_request:
        'trading.v1.instrument_option_payment_dates.get_many',
    put_instrument_option_payment_date_request: 'trading.v1.instrument_option_payment_dates.put',
    put_many_instrument_option_payment_dates_request:
        'trading.v1.instrument_option_payment_dates.put_many',
    delete_instrument_option_payment_date_request:
        'trading.v1.instrument_option_payment_dates.delete',
    delete_many_instrument_option_payment_dates_request:
        'trading.v1.instrument_option_payment_dates.delete_many',
    list_instrument_option_payment_date_versions_request:
        'trading.v1.instrument_option_payment_dates_versions.list',
    get_instrument_option_payment_date_version_request:
        'trading.v1.instrument_option_payment_dates_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_instrument_option_payment_dates_request: true,
    get_instrument_option_payment_date_request: true,
    get_many_instrument_option_payment_dates_request: true,
    put_instrument_option_payment_date_request: true,
    put_many_instrument_option_payment_dates_request: true,
    delete_instrument_option_payment_date_request: true,
    delete_many_instrument_option_payment_dates_request: true,
    list_instrument_option_payment_date_versions_request: true,
    get_instrument_option_payment_date_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.instrument_option_payment_dates_events.created',
    updated: 'trading.v1.instrument_option_payment_dates_events.updated',
    deleted: 'trading.v1.instrument_option_payment_dates_events.deleted',
} as const;
