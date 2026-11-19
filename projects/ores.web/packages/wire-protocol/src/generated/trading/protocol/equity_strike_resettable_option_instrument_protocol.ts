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
import type { EquityStrikeResettableOptionInstrument } from '../domain/equity_strike_resettable_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityStrikeResettableOptionInstrumentKey {
    trade_id: string;
}

export interface EquityStrikeResettableOptionInstrumentWrite {
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

export interface EquityStrikeResettableOptionInstrumentChange {
    write: EquityStrikeResettableOptionInstrumentWrite;
    precondition: Precondition;
}

export interface EquityStrikeResettableOptionInstrumentRemoval {
    key: EquityStrikeResettableOptionInstrumentKey;
    precondition: Precondition;
}

export interface EquityStrikeResettableOptionInstrumentLookup {
    key: EquityStrikeResettableOptionInstrumentKey;
    equity_strike_resettable_option_instrument: EquityStrikeResettableOptionInstrument | null;
}

export interface EquityStrikeResettableOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityStrikeResettableOptionInstrumentEvent {
    event_id: string;
    key: EquityStrikeResettableOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityStrikeResettableOptionInstrumentVersionKey {
    equity_strike_resettable_option_instrument: EquityStrikeResettableOptionInstrumentKey;
    version: number;
}

export interface EquityStrikeResettableOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityStrikeResettableOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityStrikeResettableOptionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquityStrikeResettableOptionInstrumentsResponse {
    result: Result;
    equity_strike_resettable_option_instruments: EquityStrikeResettableOptionInstrument[];
    total: number;
}

export interface GetEquityStrikeResettableOptionInstrumentRequest {
    key: EquityStrikeResettableOptionInstrumentKey;
}

export interface GetEquityStrikeResettableOptionInstrumentResponse {
    result: Result;
    equity_strike_resettable_option_instrument: EquityStrikeResettableOptionInstrument | null;
}

export interface GetManyEquityStrikeResettableOptionInstrumentsRequest {
    keys: EquityStrikeResettableOptionInstrumentKey[];
}

export interface GetManyEquityStrikeResettableOptionInstrumentsResponse {
    result: Result;
    entries: EquityStrikeResettableOptionInstrumentLookup[];
}

export interface PutEquityStrikeResettableOptionInstrumentRequest {
    change: EquityStrikeResettableOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityStrikeResettableOptionInstrumentResponse {
    result: Result;
    equity_strike_resettable_option_instrument: EquityStrikeResettableOptionInstrument | null;
}

export interface PutManyEquityStrikeResettableOptionInstrumentsRequest {
    changes: EquityStrikeResettableOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityStrikeResettableOptionInstrumentsResponse {
    result: Result;
    equity_strike_resettable_option_instruments: EquityStrikeResettableOptionInstrument[];
}

export interface DeleteEquityStrikeResettableOptionInstrumentRequest {
    removal: EquityStrikeResettableOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityStrikeResettableOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityStrikeResettableOptionInstrumentsRequest {
    removals: EquityStrikeResettableOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityStrikeResettableOptionInstrumentsResponse {
    result: Result;
}

export interface ListEquityStrikeResettableOptionInstrumentVersionsRequest {
    key: EquityStrikeResettableOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityStrikeResettableOptionInstrumentVersionsFilter | null;
}

export interface ListEquityStrikeResettableOptionInstrumentVersionsResponse {
    result: Result;
    versions: EquityStrikeResettableOptionInstrument[];
    total: number;
}

export interface GetEquityStrikeResettableOptionInstrumentVersionRequest {
    key: EquityStrikeResettableOptionInstrumentVersionKey;
}

export interface GetEquityStrikeResettableOptionInstrumentVersionResponse {
    result: Result;
    version: EquityStrikeResettableOptionInstrument | null;
}

export const subjects = {
    list_equity_strike_resettable_option_instruments_request:
        'trading.v1.equity_strike_resettable_option_instruments.list',
    get_equity_strike_resettable_option_instrument_request:
        'trading.v1.equity_strike_resettable_option_instruments.get',
    get_many_equity_strike_resettable_option_instruments_request:
        'trading.v1.equity_strike_resettable_option_instruments.get_many',
    put_equity_strike_resettable_option_instrument_request:
        'trading.v1.equity_strike_resettable_option_instruments.put',
    put_many_equity_strike_resettable_option_instruments_request:
        'trading.v1.equity_strike_resettable_option_instruments.put_many',
    delete_equity_strike_resettable_option_instrument_request:
        'trading.v1.equity_strike_resettable_option_instruments.delete',
    delete_many_equity_strike_resettable_option_instruments_request:
        'trading.v1.equity_strike_resettable_option_instruments.delete_many',
    list_equity_strike_resettable_option_instrument_versions_request:
        'trading.v1.equity_strike_resettable_option_instruments_versions.list',
    get_equity_strike_resettable_option_instrument_version_request:
        'trading.v1.equity_strike_resettable_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_strike_resettable_option_instruments_request: true,
    get_equity_strike_resettable_option_instrument_request: true,
    get_many_equity_strike_resettable_option_instruments_request: true,
    put_equity_strike_resettable_option_instrument_request: true,
    put_many_equity_strike_resettable_option_instruments_request: true,
    delete_equity_strike_resettable_option_instrument_request: true,
    delete_many_equity_strike_resettable_option_instruments_request: true,
    list_equity_strike_resettable_option_instrument_versions_request: true,
    get_equity_strike_resettable_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.equity_strike_resettable_option_instruments.created',
    updated: 'trading.v1.events.equity_strike_resettable_option_instruments.updated',
    deleted: 'trading.v1.events.equity_strike_resettable_option_instruments.deleted',
} as const;
