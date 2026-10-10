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
import type { BalanceGuaranteedSwapInstrument } from '../domain/balance_guaranteed_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BalanceGuaranteedSwapInstrumentKey {
    trade_id: string;
}

export interface BalanceGuaranteedSwapInstrumentWrite {
    trade_id: string;
    trade_activity_id: string;
    reference_security: string;
    lockout_days: number | null;
}

export interface BalanceGuaranteedSwapInstrumentChange {
    write: BalanceGuaranteedSwapInstrumentWrite;
    precondition: Precondition;
}

export interface BalanceGuaranteedSwapInstrumentRemoval {
    key: BalanceGuaranteedSwapInstrumentKey;
    precondition: Precondition;
}

export interface BalanceGuaranteedSwapInstrumentLookup {
    key: BalanceGuaranteedSwapInstrumentKey;
    balance_guaranteed_swap_instrument: BalanceGuaranteedSwapInstrument | null;
}

export interface BalanceGuaranteedSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface BalanceGuaranteedSwapInstrumentEvent {
    event_id: string;
    key: BalanceGuaranteedSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BalanceGuaranteedSwapInstrumentVersionKey {
    balance_guaranteed_swap_instrument: BalanceGuaranteedSwapInstrumentKey;
    version: number;
}

export interface BalanceGuaranteedSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBalanceGuaranteedSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BalanceGuaranteedSwapInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListBalanceGuaranteedSwapInstrumentsResponse {
    result: Result;
    balance_guaranteed_swap_instruments: BalanceGuaranteedSwapInstrument[];
    total: number;
}

export interface GetBalanceGuaranteedSwapInstrumentRequest {
    key: BalanceGuaranteedSwapInstrumentKey;
}

export interface GetBalanceGuaranteedSwapInstrumentResponse {
    result: Result;
    balance_guaranteed_swap_instrument: BalanceGuaranteedSwapInstrument | null;
}

export interface GetManyBalanceGuaranteedSwapInstrumentsRequest {
    keys: BalanceGuaranteedSwapInstrumentKey[];
}

export interface GetManyBalanceGuaranteedSwapInstrumentsResponse {
    result: Result;
    entries: BalanceGuaranteedSwapInstrumentLookup[];
}

export interface PutBalanceGuaranteedSwapInstrumentRequest {
    change: BalanceGuaranteedSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutBalanceGuaranteedSwapInstrumentResponse {
    result: Result;
    balance_guaranteed_swap_instrument: BalanceGuaranteedSwapInstrument | null;
}

export interface PutManyBalanceGuaranteedSwapInstrumentsRequest {
    changes: BalanceGuaranteedSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyBalanceGuaranteedSwapInstrumentsResponse {
    result: Result;
    balance_guaranteed_swap_instruments: BalanceGuaranteedSwapInstrument[];
}

export interface DeleteBalanceGuaranteedSwapInstrumentRequest {
    removal: BalanceGuaranteedSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteBalanceGuaranteedSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyBalanceGuaranteedSwapInstrumentsRequest {
    removals: BalanceGuaranteedSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBalanceGuaranteedSwapInstrumentsResponse {
    result: Result;
}

export interface ListBalanceGuaranteedSwapInstrumentVersionsRequest {
    key: BalanceGuaranteedSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BalanceGuaranteedSwapInstrumentVersionsFilter | null;
}

export interface ListBalanceGuaranteedSwapInstrumentVersionsResponse {
    result: Result;
    versions: BalanceGuaranteedSwapInstrument[];
    total: number;
}

export interface GetBalanceGuaranteedSwapInstrumentVersionRequest {
    key: BalanceGuaranteedSwapInstrumentVersionKey;
}

export interface GetBalanceGuaranteedSwapInstrumentVersionResponse {
    result: Result;
    version: BalanceGuaranteedSwapInstrument | null;
}

export const subjects = {
    list_balance_guaranteed_swap_instruments_request:
        'trading.v1.balance_guaranteed_swap_instruments.list',
    get_balance_guaranteed_swap_instrument_request:
        'trading.v1.balance_guaranteed_swap_instruments.get',
    get_many_balance_guaranteed_swap_instruments_request:
        'trading.v1.balance_guaranteed_swap_instruments.get_many',
    put_balance_guaranteed_swap_instrument_request:
        'trading.v1.balance_guaranteed_swap_instruments.put',
    put_many_balance_guaranteed_swap_instruments_request:
        'trading.v1.balance_guaranteed_swap_instruments.put_many',
    delete_balance_guaranteed_swap_instrument_request:
        'trading.v1.balance_guaranteed_swap_instruments.delete',
    delete_many_balance_guaranteed_swap_instruments_request:
        'trading.v1.balance_guaranteed_swap_instruments.delete_many',
    list_balance_guaranteed_swap_instrument_versions_request:
        'trading.v1.balance_guaranteed_swap_instruments_versions.list',
    get_balance_guaranteed_swap_instrument_version_request:
        'trading.v1.balance_guaranteed_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_balance_guaranteed_swap_instruments_request: true,
    get_balance_guaranteed_swap_instrument_request: true,
    get_many_balance_guaranteed_swap_instruments_request: true,
    put_balance_guaranteed_swap_instrument_request: true,
    put_many_balance_guaranteed_swap_instruments_request: true,
    delete_balance_guaranteed_swap_instrument_request: true,
    delete_many_balance_guaranteed_swap_instruments_request: true,
    list_balance_guaranteed_swap_instrument_versions_request: true,
    get_balance_guaranteed_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.balance_guaranteed_swap_instruments_events.created',
    updated: 'trading.v1.balance_guaranteed_swap_instruments_events.updated',
    deleted: 'trading.v1.balance_guaranteed_swap_instruments_events.deleted',
} as const;
