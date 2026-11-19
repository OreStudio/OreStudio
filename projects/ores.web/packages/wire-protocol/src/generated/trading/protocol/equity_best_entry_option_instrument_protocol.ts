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
import type { EquityBestEntryOptionInstrument } from '../domain/equity_best_entry_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityBestEntryOptionInstrumentKey {
    trade_id: string;
}

export interface EquityBestEntryOptionInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    long_short: string;
    currency: string;
    notional: string;
    multiplier: string;
    cap: string;
    strike: string;
    trigger_level: string;
    reset_minimum: string;
    premium: string;
    expiry_date: string;
    strike_date: string;
    premium_date: string;
    settlement_date: string;
    description: string;
}

export interface EquityBestEntryOptionInstrumentChange {
    write: EquityBestEntryOptionInstrumentWrite;
    precondition: Precondition;
}

export interface EquityBestEntryOptionInstrumentRemoval {
    key: EquityBestEntryOptionInstrumentKey;
    precondition: Precondition;
}

export interface EquityBestEntryOptionInstrumentLookup {
    key: EquityBestEntryOptionInstrumentKey;
    equity_best_entry_option_instrument: EquityBestEntryOptionInstrument | null;
}

export interface EquityBestEntryOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityBestEntryOptionInstrumentEvent {
    event_id: string;
    key: EquityBestEntryOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityBestEntryOptionInstrumentVersionKey {
    equity_best_entry_option_instrument: EquityBestEntryOptionInstrumentKey;
    version: number;
}

export interface EquityBestEntryOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityBestEntryOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityBestEntryOptionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquityBestEntryOptionInstrumentsResponse {
    result: Result;
    equity_best_entry_option_instruments: EquityBestEntryOptionInstrument[];
    total: number;
}

export interface GetEquityBestEntryOptionInstrumentRequest {
    key: EquityBestEntryOptionInstrumentKey;
}

export interface GetEquityBestEntryOptionInstrumentResponse {
    result: Result;
    equity_best_entry_option_instrument: EquityBestEntryOptionInstrument | null;
}

export interface GetManyEquityBestEntryOptionInstrumentsRequest {
    keys: EquityBestEntryOptionInstrumentKey[];
}

export interface GetManyEquityBestEntryOptionInstrumentsResponse {
    result: Result;
    entries: EquityBestEntryOptionInstrumentLookup[];
}

export interface PutEquityBestEntryOptionInstrumentRequest {
    change: EquityBestEntryOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityBestEntryOptionInstrumentResponse {
    result: Result;
    equity_best_entry_option_instrument: EquityBestEntryOptionInstrument | null;
}

export interface PutManyEquityBestEntryOptionInstrumentsRequest {
    changes: EquityBestEntryOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityBestEntryOptionInstrumentsResponse {
    result: Result;
    equity_best_entry_option_instruments: EquityBestEntryOptionInstrument[];
}

export interface DeleteEquityBestEntryOptionInstrumentRequest {
    removal: EquityBestEntryOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityBestEntryOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityBestEntryOptionInstrumentsRequest {
    removals: EquityBestEntryOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityBestEntryOptionInstrumentsResponse {
    result: Result;
}

export interface ListEquityBestEntryOptionInstrumentVersionsRequest {
    key: EquityBestEntryOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityBestEntryOptionInstrumentVersionsFilter | null;
}

export interface ListEquityBestEntryOptionInstrumentVersionsResponse {
    result: Result;
    versions: EquityBestEntryOptionInstrument[];
    total: number;
}

export interface GetEquityBestEntryOptionInstrumentVersionRequest {
    key: EquityBestEntryOptionInstrumentVersionKey;
}

export interface GetEquityBestEntryOptionInstrumentVersionResponse {
    result: Result;
    version: EquityBestEntryOptionInstrument | null;
}

export const subjects = {
    list_equity_best_entry_option_instruments_request:
        'trading.v1.equity_best_entry_option_instruments.list',
    get_equity_best_entry_option_instrument_request:
        'trading.v1.equity_best_entry_option_instruments.get',
    get_many_equity_best_entry_option_instruments_request:
        'trading.v1.equity_best_entry_option_instruments.get_many',
    put_equity_best_entry_option_instrument_request:
        'trading.v1.equity_best_entry_option_instruments.put',
    put_many_equity_best_entry_option_instruments_request:
        'trading.v1.equity_best_entry_option_instruments.put_many',
    delete_equity_best_entry_option_instrument_request:
        'trading.v1.equity_best_entry_option_instruments.delete',
    delete_many_equity_best_entry_option_instruments_request:
        'trading.v1.equity_best_entry_option_instruments.delete_many',
    list_equity_best_entry_option_instrument_versions_request:
        'trading.v1.equity_best_entry_option_instruments_versions.list',
    get_equity_best_entry_option_instrument_version_request:
        'trading.v1.equity_best_entry_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_best_entry_option_instruments_request: true,
    get_equity_best_entry_option_instrument_request: true,
    get_many_equity_best_entry_option_instruments_request: true,
    put_equity_best_entry_option_instrument_request: true,
    put_many_equity_best_entry_option_instruments_request: true,
    delete_equity_best_entry_option_instrument_request: true,
    delete_many_equity_best_entry_option_instruments_request: true,
    list_equity_best_entry_option_instrument_versions_request: true,
    get_equity_best_entry_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.equity_best_entry_option_instruments.created',
    updated: 'trading.v1.events.equity_best_entry_option_instruments.updated',
    deleted: 'trading.v1.events.equity_best_entry_option_instruments.deleted',
} as const;
