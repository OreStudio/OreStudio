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
import type { IndexCreditDefaultSwapInstrument } from '../domain/index_credit_default_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface IndexCreditDefaultSwapInstrumentKey {
    trade_id: string;
}

export interface IndexCreditDefaultSwapInstrumentWrite {
    trade_id: string;
    trade_activity_id: string;
    credit_curve_id: string;
}

export interface IndexCreditDefaultSwapInstrumentChange {
    write: IndexCreditDefaultSwapInstrumentWrite;
    precondition: Precondition;
}

export interface IndexCreditDefaultSwapInstrumentRemoval {
    key: IndexCreditDefaultSwapInstrumentKey;
    precondition: Precondition;
}

export interface IndexCreditDefaultSwapInstrumentLookup {
    key: IndexCreditDefaultSwapInstrumentKey;
    index_credit_default_swap_instrument: IndexCreditDefaultSwapInstrument | null;
}

export interface IndexCreditDefaultSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface IndexCreditDefaultSwapInstrumentEvent {
    event_id: string;
    key: IndexCreditDefaultSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface IndexCreditDefaultSwapInstrumentVersionKey {
    index_credit_default_swap_instrument: IndexCreditDefaultSwapInstrumentKey;
    version: number;
}

export interface IndexCreditDefaultSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListIndexCreditDefaultSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: IndexCreditDefaultSwapInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListIndexCreditDefaultSwapInstrumentsResponse {
    result: Result;
    index_cds_instruments: IndexCreditDefaultSwapInstrument[];
    total: number;
}

export interface GetIndexCreditDefaultSwapInstrumentRequest {
    key: IndexCreditDefaultSwapInstrumentKey;
}

export interface GetIndexCreditDefaultSwapInstrumentResponse {
    result: Result;
    index_credit_default_swap_instrument: IndexCreditDefaultSwapInstrument | null;
}

export interface GetManyIndexCreditDefaultSwapInstrumentsRequest {
    keys: IndexCreditDefaultSwapInstrumentKey[];
}

export interface GetManyIndexCreditDefaultSwapInstrumentsResponse {
    result: Result;
    entries: IndexCreditDefaultSwapInstrumentLookup[];
}

export interface PutIndexCreditDefaultSwapInstrumentRequest {
    change: IndexCreditDefaultSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutIndexCreditDefaultSwapInstrumentResponse {
    result: Result;
    index_credit_default_swap_instrument: IndexCreditDefaultSwapInstrument | null;
}

export interface PutManyIndexCreditDefaultSwapInstrumentsRequest {
    changes: IndexCreditDefaultSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyIndexCreditDefaultSwapInstrumentsResponse {
    result: Result;
    index_cds_instruments: IndexCreditDefaultSwapInstrument[];
}

export interface DeleteIndexCreditDefaultSwapInstrumentRequest {
    removal: IndexCreditDefaultSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteIndexCreditDefaultSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyIndexCreditDefaultSwapInstrumentsRequest {
    removals: IndexCreditDefaultSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyIndexCreditDefaultSwapInstrumentsResponse {
    result: Result;
}

export interface ListIndexCreditDefaultSwapInstrumentVersionsRequest {
    key: IndexCreditDefaultSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: IndexCreditDefaultSwapInstrumentVersionsFilter | null;
}

export interface ListIndexCreditDefaultSwapInstrumentVersionsResponse {
    result: Result;
    versions: IndexCreditDefaultSwapInstrument[];
    total: number;
}

export interface GetIndexCreditDefaultSwapInstrumentVersionRequest {
    key: IndexCreditDefaultSwapInstrumentVersionKey;
}

export interface GetIndexCreditDefaultSwapInstrumentVersionResponse {
    result: Result;
    version: IndexCreditDefaultSwapInstrument | null;
}

export const subjects = {
    list_index_credit_default_swap_instruments_request:
        'trading.v1.index_credit_default_swap_instruments.list',
    get_index_credit_default_swap_instrument_request:
        'trading.v1.index_credit_default_swap_instruments.get',
    get_many_index_credit_default_swap_instruments_request:
        'trading.v1.index_credit_default_swap_instruments.get_many',
    put_index_credit_default_swap_instrument_request:
        'trading.v1.index_credit_default_swap_instruments.put',
    put_many_index_credit_default_swap_instruments_request:
        'trading.v1.index_credit_default_swap_instruments.put_many',
    delete_index_credit_default_swap_instrument_request:
        'trading.v1.index_credit_default_swap_instruments.delete',
    delete_many_index_credit_default_swap_instruments_request:
        'trading.v1.index_credit_default_swap_instruments.delete_many',
    list_index_credit_default_swap_instrument_versions_request:
        'trading.v1.index_credit_default_swap_instruments_versions.list',
    get_index_credit_default_swap_instrument_version_request:
        'trading.v1.index_credit_default_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_index_credit_default_swap_instruments_request: true,
    get_index_credit_default_swap_instrument_request: true,
    get_many_index_credit_default_swap_instruments_request: true,
    put_index_credit_default_swap_instrument_request: true,
    put_many_index_credit_default_swap_instruments_request: true,
    delete_index_credit_default_swap_instrument_request: true,
    delete_many_index_credit_default_swap_instruments_request: true,
    list_index_credit_default_swap_instrument_versions_request: true,
    get_index_credit_default_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.index_credit_default_swap_instruments_events.created',
    updated: 'trading.v1.index_credit_default_swap_instruments_events.updated',
    deleted: 'trading.v1.index_credit_default_swap_instruments_events.deleted',
} as const;
