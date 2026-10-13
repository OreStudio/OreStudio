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
import type { IndexCreditDefaultSwapOptionInstrument } from '../domain/index_credit_default_swap_option_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface IndexCreditDefaultSwapOptionInstrumentKey {
    trade_id: string;
}

export interface IndexCreditDefaultSwapOptionInstrumentWrite {
    trade_id: string;
    trade_activity_id: string;
    knock_out: boolean | null;
    index_term: string | null;
    strike_type: string | null;
    trade_date: string | null;
    front_end_protection_start_date: string | null;
}

export interface IndexCreditDefaultSwapOptionInstrumentChange {
    write: IndexCreditDefaultSwapOptionInstrumentWrite;
    precondition: Precondition;
}

export interface IndexCreditDefaultSwapOptionInstrumentRemoval {
    key: IndexCreditDefaultSwapOptionInstrumentKey;
    precondition: Precondition;
}

export interface IndexCreditDefaultSwapOptionInstrumentLookup {
    key: IndexCreditDefaultSwapOptionInstrumentKey;
    index_credit_default_swap_option_instrument: IndexCreditDefaultSwapOptionInstrument | null;
}

export interface IndexCreditDefaultSwapOptionInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface IndexCreditDefaultSwapOptionInstrumentEvent {
    event_id: string;
    key: IndexCreditDefaultSwapOptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface IndexCreditDefaultSwapOptionInstrumentVersionKey {
    index_credit_default_swap_option_instrument: IndexCreditDefaultSwapOptionInstrumentKey;
    version: number;
}

export interface IndexCreditDefaultSwapOptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListIndexCreditDefaultSwapOptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: IndexCreditDefaultSwapOptionInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListIndexCreditDefaultSwapOptionInstrumentsResponse {
    result: Result;
    index_cds_option_instruments: IndexCreditDefaultSwapOptionInstrument[];
    total: number;
}

export interface GetIndexCreditDefaultSwapOptionInstrumentRequest {
    key: IndexCreditDefaultSwapOptionInstrumentKey;
}

export interface GetIndexCreditDefaultSwapOptionInstrumentResponse {
    result: Result;
    index_credit_default_swap_option_instrument: IndexCreditDefaultSwapOptionInstrument | null;
}

export interface GetManyIndexCreditDefaultSwapOptionInstrumentsRequest {
    keys: IndexCreditDefaultSwapOptionInstrumentKey[];
}

export interface GetManyIndexCreditDefaultSwapOptionInstrumentsResponse {
    result: Result;
    entries: IndexCreditDefaultSwapOptionInstrumentLookup[];
}

export interface PutIndexCreditDefaultSwapOptionInstrumentRequest {
    change: IndexCreditDefaultSwapOptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutIndexCreditDefaultSwapOptionInstrumentResponse {
    result: Result;
    index_credit_default_swap_option_instrument: IndexCreditDefaultSwapOptionInstrument | null;
}

export interface PutManyIndexCreditDefaultSwapOptionInstrumentsRequest {
    changes: IndexCreditDefaultSwapOptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyIndexCreditDefaultSwapOptionInstrumentsResponse {
    result: Result;
    index_cds_option_instruments: IndexCreditDefaultSwapOptionInstrument[];
}

export interface DeleteIndexCreditDefaultSwapOptionInstrumentRequest {
    removal: IndexCreditDefaultSwapOptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteIndexCreditDefaultSwapOptionInstrumentResponse {
    result: Result;
}

export interface DeleteManyIndexCreditDefaultSwapOptionInstrumentsRequest {
    removals: IndexCreditDefaultSwapOptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyIndexCreditDefaultSwapOptionInstrumentsResponse {
    result: Result;
}

export interface ListIndexCreditDefaultSwapOptionInstrumentVersionsRequest {
    key: IndexCreditDefaultSwapOptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: IndexCreditDefaultSwapOptionInstrumentVersionsFilter | null;
}

export interface ListIndexCreditDefaultSwapOptionInstrumentVersionsResponse {
    result: Result;
    versions: IndexCreditDefaultSwapOptionInstrument[];
    total: number;
}

export interface GetIndexCreditDefaultSwapOptionInstrumentVersionRequest {
    key: IndexCreditDefaultSwapOptionInstrumentVersionKey;
}

export interface GetIndexCreditDefaultSwapOptionInstrumentVersionResponse {
    result: Result;
    version: IndexCreditDefaultSwapOptionInstrument | null;
}

export const subjects = {
    list_index_credit_default_swap_option_instruments_request:
        'trading.v1.index_credit_default_swap_option_instruments.list',
    get_index_credit_default_swap_option_instrument_request:
        'trading.v1.index_credit_default_swap_option_instruments.get',
    get_many_index_credit_default_swap_option_instruments_request:
        'trading.v1.index_credit_default_swap_option_instruments.get_many',
    put_index_credit_default_swap_option_instrument_request:
        'trading.v1.index_credit_default_swap_option_instruments.put',
    put_many_index_credit_default_swap_option_instruments_request:
        'trading.v1.index_credit_default_swap_option_instruments.put_many',
    delete_index_credit_default_swap_option_instrument_request:
        'trading.v1.index_credit_default_swap_option_instruments.delete',
    delete_many_index_credit_default_swap_option_instruments_request:
        'trading.v1.index_credit_default_swap_option_instruments.delete_many',
    list_index_credit_default_swap_option_instrument_versions_request:
        'trading.v1.index_credit_default_swap_option_instruments_versions.list',
    get_index_credit_default_swap_option_instrument_version_request:
        'trading.v1.index_credit_default_swap_option_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_index_credit_default_swap_option_instruments_request: true,
    get_index_credit_default_swap_option_instrument_request: true,
    get_many_index_credit_default_swap_option_instruments_request: true,
    put_index_credit_default_swap_option_instrument_request: true,
    put_many_index_credit_default_swap_option_instruments_request: true,
    delete_index_credit_default_swap_option_instrument_request: true,
    delete_many_index_credit_default_swap_option_instruments_request: true,
    list_index_credit_default_swap_option_instrument_versions_request: true,
    get_index_credit_default_swap_option_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.index_credit_default_swap_option_instruments_events.created',
    updated: 'trading.v1.index_credit_default_swap_option_instruments_events.updated',
    deleted: 'trading.v1.index_credit_default_swap_option_instruments_events.deleted',
} as const;
