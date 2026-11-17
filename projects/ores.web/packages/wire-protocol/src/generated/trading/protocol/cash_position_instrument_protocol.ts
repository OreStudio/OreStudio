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
import type { CashPositionInstrument } from '../domain/cash_position_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CashPositionInstrumentKey {
    trade_id: string;
}

export interface CashPositionInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    currency: string;
    amount: string;
    description: string;
}

export interface CashPositionInstrumentChange {
    write: CashPositionInstrumentWrite;
    precondition: Precondition;
}

export interface CashPositionInstrumentRemoval {
    key: CashPositionInstrumentKey;
    precondition: Precondition;
}

export interface CashPositionInstrumentLookup {
    key: CashPositionInstrumentKey;
    cash_position_instrument: CashPositionInstrument | null;
}

export interface CashPositionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface CashPositionInstrumentEvent {
    event_id: string;
    key: CashPositionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CashPositionInstrumentVersionKey {
    cash_position_instrument: CashPositionInstrumentKey;
    version: number;
}

export interface CashPositionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCashPositionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CashPositionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListCashPositionInstrumentsResponse {
    result: Result;
    cash_position_instruments: CashPositionInstrument[];
    total: number;
}

export interface GetCashPositionInstrumentRequest {
    key: CashPositionInstrumentKey;
}

export interface GetCashPositionInstrumentResponse {
    result: Result;
    cash_position_instrument: CashPositionInstrument | null;
}

export interface GetManyCashPositionInstrumentsRequest {
    keys: CashPositionInstrumentKey[];
}

export interface GetManyCashPositionInstrumentsResponse {
    result: Result;
    entries: CashPositionInstrumentLookup[];
}

export interface PutCashPositionInstrumentRequest {
    change: CashPositionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutCashPositionInstrumentResponse {
    result: Result;
    cash_position_instrument: CashPositionInstrument | null;
}

export interface PutManyCashPositionInstrumentsRequest {
    changes: CashPositionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyCashPositionInstrumentsResponse {
    result: Result;
    cash_position_instruments: CashPositionInstrument[];
}

export interface DeleteCashPositionInstrumentRequest {
    removal: CashPositionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteCashPositionInstrumentResponse {
    result: Result;
}

export interface DeleteManyCashPositionInstrumentsRequest {
    removals: CashPositionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCashPositionInstrumentsResponse {
    result: Result;
}

export interface ListCashPositionInstrumentVersionsRequest {
    key: CashPositionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CashPositionInstrumentVersionsFilter | null;
}

export interface ListCashPositionInstrumentVersionsResponse {
    result: Result;
    versions: CashPositionInstrument[];
    total: number;
}

export interface GetCashPositionInstrumentVersionRequest {
    key: CashPositionInstrumentVersionKey;
}

export interface GetCashPositionInstrumentVersionResponse {
    result: Result;
    version: CashPositionInstrument | null;
}

export const subjects = {
    list_cash_position_instruments_request: 'trading.v1.cash_position_instruments.list',
    get_cash_position_instrument_request: 'trading.v1.cash_position_instruments.get',
    get_many_cash_position_instruments_request: 'trading.v1.cash_position_instruments.get_many',
    put_cash_position_instrument_request: 'trading.v1.cash_position_instruments.put',
    put_many_cash_position_instruments_request: 'trading.v1.cash_position_instruments.put_many',
    delete_cash_position_instrument_request: 'trading.v1.cash_position_instruments.delete',
    delete_many_cash_position_instruments_request:
        'trading.v1.cash_position_instruments.delete_many',
    list_cash_position_instrument_versions_request:
        'trading.v1.cash_position_instruments_versions.list',
    get_cash_position_instrument_version_request:
        'trading.v1.cash_position_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cash_position_instruments_request: true,
    get_cash_position_instrument_request: true,
    get_many_cash_position_instruments_request: true,
    put_cash_position_instrument_request: true,
    put_many_cash_position_instruments_request: true,
    delete_cash_position_instrument_request: true,
    delete_many_cash_position_instruments_request: true,
    list_cash_position_instrument_versions_request: true,
    get_cash_position_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.cash_position_instruments.created',
    updated: 'trading.v1.events.cash_position_instruments.updated',
    deleted: 'trading.v1.events.cash_position_instruments.deleted',
} as const;
