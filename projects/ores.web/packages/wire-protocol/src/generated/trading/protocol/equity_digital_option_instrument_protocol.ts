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
import type { EquityDigitalOptionInstrument } from '../domain/equity_digital_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityDigitalOptionInstrumentKey {
    trade_id: string;
}

export interface EquityDigitalOptionInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    underlying_name: string;
    currency: string;
    notional: string;
    option_type: string;
    strike: string | null;
    barrier_level: string | null;
    barrier_type: string;
    expiry_date: string;
    long_short: string;
    payout_amount: string | null;
    description: string;
}

export interface EquityDigitalOptionInstrumentChange {
    write: EquityDigitalOptionInstrumentWrite;
    precondition: Precondition;
}

export interface EquityDigitalOptionInstrumentRemoval {
    key: EquityDigitalOptionInstrumentKey;
    precondition: Precondition;
}

export interface EquityDigitalOptionInstrumentLookup {
    key: EquityDigitalOptionInstrumentKey;
    equity_digital_option_instrument: EquityDigitalOptionInstrument | null;
}

export interface EquityDigitalOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityDigitalOptionInstrumentEvent {
    event_id: string;
    key: EquityDigitalOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityDigitalOptionInstrumentVersionKey {
    equity_digital_option_instrument: EquityDigitalOptionInstrumentKey;
    version: number;
}

export interface EquityDigitalOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityDigitalOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityDigitalOptionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquityDigitalOptionInstrumentsResponse {
    result: Result;
    equity_digital_option_instruments: EquityDigitalOptionInstrument[];
    total: number;
}

export interface GetEquityDigitalOptionInstrumentRequest {
    key: EquityDigitalOptionInstrumentKey;
}

export interface GetEquityDigitalOptionInstrumentResponse {
    result: Result;
    equity_digital_option_instrument: EquityDigitalOptionInstrument | null;
}

export interface GetManyEquityDigitalOptionInstrumentsRequest {
    keys: EquityDigitalOptionInstrumentKey[];
}

export interface GetManyEquityDigitalOptionInstrumentsResponse {
    result: Result;
    entries: EquityDigitalOptionInstrumentLookup[];
}

export interface PutEquityDigitalOptionInstrumentRequest {
    change: EquityDigitalOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityDigitalOptionInstrumentResponse {
    result: Result;
    equity_digital_option_instrument: EquityDigitalOptionInstrument | null;
}

export interface PutManyEquityDigitalOptionInstrumentsRequest {
    changes: EquityDigitalOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityDigitalOptionInstrumentsResponse {
    result: Result;
    equity_digital_option_instruments: EquityDigitalOptionInstrument[];
}

export interface DeleteEquityDigitalOptionInstrumentRequest {
    removal: EquityDigitalOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityDigitalOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityDigitalOptionInstrumentsRequest {
    removals: EquityDigitalOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityDigitalOptionInstrumentsResponse {
    result: Result;
}

export interface ListEquityDigitalOptionInstrumentVersionsRequest {
    key: EquityDigitalOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityDigitalOptionInstrumentVersionsFilter | null;
}

export interface ListEquityDigitalOptionInstrumentVersionsResponse {
    result: Result;
    versions: EquityDigitalOptionInstrument[];
    total: number;
}

export interface GetEquityDigitalOptionInstrumentVersionRequest {
    key: EquityDigitalOptionInstrumentVersionKey;
}

export interface GetEquityDigitalOptionInstrumentVersionResponse {
    result: Result;
    version: EquityDigitalOptionInstrument | null;
}

export const subjects = {
    list_equity_digital_option_instruments_request:
        'trading.v1.equity_digital_option_instruments.list',
    get_equity_digital_option_instrument_request:
        'trading.v1.equity_digital_option_instruments.get',
    get_many_equity_digital_option_instruments_request:
        'trading.v1.equity_digital_option_instruments.get_many',
    put_equity_digital_option_instrument_request:
        'trading.v1.equity_digital_option_instruments.put',
    put_many_equity_digital_option_instruments_request:
        'trading.v1.equity_digital_option_instruments.put_many',
    delete_equity_digital_option_instrument_request:
        'trading.v1.equity_digital_option_instruments.delete',
    delete_many_equity_digital_option_instruments_request:
        'trading.v1.equity_digital_option_instruments.delete_many',
    list_equity_digital_option_instrument_versions_request:
        'trading.v1.equity_digital_option_instruments_versions.list',
    get_equity_digital_option_instrument_version_request:
        'trading.v1.equity_digital_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_digital_option_instruments_request: true,
    get_equity_digital_option_instrument_request: true,
    get_many_equity_digital_option_instruments_request: true,
    put_equity_digital_option_instrument_request: true,
    put_many_equity_digital_option_instruments_request: true,
    delete_equity_digital_option_instrument_request: true,
    delete_many_equity_digital_option_instruments_request: true,
    list_equity_digital_option_instrument_versions_request: true,
    get_equity_digital_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_digital_option_instruments_events.created',
    updated: 'trading.v1.equity_digital_option_instruments_events.updated',
    deleted: 'trading.v1.equity_digital_option_instruments_events.deleted',
} as const;
