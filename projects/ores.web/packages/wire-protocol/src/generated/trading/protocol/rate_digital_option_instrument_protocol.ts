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
import type { RateDigitalOptionInstrument } from '../domain/rate_digital_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RateDigitalOptionInstrumentKey {
    trade_id: string;
}

export interface RateDigitalOptionInstrumentWrite {
    trade_id: string;
    trade_activity_id: string;
    index_name: string;
    strike: string;
    payoff_amount: string;
    payoff_currency: string;
    fixing_date: string;
    payment_date: string | null;
}

export interface RateDigitalOptionInstrumentChange {
    write: RateDigitalOptionInstrumentWrite;
    precondition: Precondition;
}

export interface RateDigitalOptionInstrumentRemoval {
    key: RateDigitalOptionInstrumentKey;
    precondition: Precondition;
}

export interface RateDigitalOptionInstrumentLookup {
    key: RateDigitalOptionInstrumentKey;
    rate_digital_option_instrument: RateDigitalOptionInstrument | null;
}

export interface RateDigitalOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface RateDigitalOptionInstrumentEvent {
    event_id: string;
    key: RateDigitalOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface RateDigitalOptionInstrumentVersionKey {
    rate_digital_option_instrument: RateDigitalOptionInstrumentKey;
    version: number;
}

export interface RateDigitalOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRateDigitalOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RateDigitalOptionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListRateDigitalOptionInstrumentsResponse {
    result: Result;
    rate_digital_option_instruments: RateDigitalOptionInstrument[];
    total: number;
}

export interface GetRateDigitalOptionInstrumentRequest {
    key: RateDigitalOptionInstrumentKey;
}

export interface GetRateDigitalOptionInstrumentResponse {
    result: Result;
    rate_digital_option_instrument: RateDigitalOptionInstrument | null;
}

export interface GetManyRateDigitalOptionInstrumentsRequest {
    keys: RateDigitalOptionInstrumentKey[];
}

export interface GetManyRateDigitalOptionInstrumentsResponse {
    result: Result;
    entries: RateDigitalOptionInstrumentLookup[];
}

export interface PutRateDigitalOptionInstrumentRequest {
    change: RateDigitalOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutRateDigitalOptionInstrumentResponse {
    result: Result;
    rate_digital_option_instrument: RateDigitalOptionInstrument | null;
}

export interface PutManyRateDigitalOptionInstrumentsRequest {
    changes: RateDigitalOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyRateDigitalOptionInstrumentsResponse {
    result: Result;
    rate_digital_option_instruments: RateDigitalOptionInstrument[];
}

export interface DeleteRateDigitalOptionInstrumentRequest {
    removal: RateDigitalOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteRateDigitalOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyRateDigitalOptionInstrumentsRequest {
    removals: RateDigitalOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRateDigitalOptionInstrumentsResponse {
    result: Result;
}

export interface ListRateDigitalOptionInstrumentVersionsRequest {
    key: RateDigitalOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RateDigitalOptionInstrumentVersionsFilter | null;
}

export interface ListRateDigitalOptionInstrumentVersionsResponse {
    result: Result;
    versions: RateDigitalOptionInstrument[];
    total: number;
}

export interface GetRateDigitalOptionInstrumentVersionRequest {
    key: RateDigitalOptionInstrumentVersionKey;
}

export interface GetRateDigitalOptionInstrumentVersionResponse {
    result: Result;
    version: RateDigitalOptionInstrument | null;
}

export const subjects = {
    list_rate_digital_option_instruments_request: 'trading.v1.rate_digital_option_instruments.list',
    get_rate_digital_option_instrument_request: 'trading.v1.rate_digital_option_instruments.get',
    get_many_rate_digital_option_instruments_request:
        'trading.v1.rate_digital_option_instruments.get_many',
    put_rate_digital_option_instrument_request: 'trading.v1.rate_digital_option_instruments.put',
    put_many_rate_digital_option_instruments_request:
        'trading.v1.rate_digital_option_instruments.put_many',
    delete_rate_digital_option_instrument_request:
        'trading.v1.rate_digital_option_instruments.delete',
    delete_many_rate_digital_option_instruments_request:
        'trading.v1.rate_digital_option_instruments.delete_many',
    list_rate_digital_option_instrument_versions_request:
        'trading.v1.rate_digital_option_instruments_versions.list',
    get_rate_digital_option_instrument_version_request:
        'trading.v1.rate_digital_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_rate_digital_option_instruments_request: true,
    get_rate_digital_option_instrument_request: true,
    get_many_rate_digital_option_instruments_request: true,
    put_rate_digital_option_instrument_request: true,
    put_many_rate_digital_option_instruments_request: true,
    delete_rate_digital_option_instrument_request: true,
    delete_many_rate_digital_option_instruments_request: true,
    list_rate_digital_option_instrument_versions_request: true,
    get_rate_digital_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.rate_digital_option_instruments.created',
    updated: 'trading.v1.events.rate_digital_option_instruments.updated',
    deleted: 'trading.v1.events.rate_digital_option_instruments.deleted',
} as const;
