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
import type { CreditLinkedSwapInstrument } from '../domain/credit_linked_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CreditLinkedSwapInstrumentKey {
    trade_id: string;
}

export interface CreditLinkedSwapInstrumentWrite {
    trade_id: string;
    trade_activity_id: string;
    credit_curve_id: string;
    recovery_rate: number | null;
    settles_accrual: boolean | null;
    default_payment_time: string | null;
}

export interface CreditLinkedSwapInstrumentChange {
    write: CreditLinkedSwapInstrumentWrite;
    precondition: Precondition;
}

export interface CreditLinkedSwapInstrumentRemoval {
    key: CreditLinkedSwapInstrumentKey;
    precondition: Precondition;
}

export interface CreditLinkedSwapInstrumentLookup {
    key: CreditLinkedSwapInstrumentKey;
    credit_linked_swap_instrument: CreditLinkedSwapInstrument | null;
}

export interface CreditLinkedSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface CreditLinkedSwapInstrumentEvent {
    event_id: string;
    key: CreditLinkedSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CreditLinkedSwapInstrumentVersionKey {
    credit_linked_swap_instrument: CreditLinkedSwapInstrumentKey;
    version: number;
}

export interface CreditLinkedSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCreditLinkedSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CreditLinkedSwapInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListCreditLinkedSwapInstrumentsResponse {
    result: Result;
    credit_linked_swap_instruments: CreditLinkedSwapInstrument[];
    total: number;
}

export interface GetCreditLinkedSwapInstrumentRequest {
    key: CreditLinkedSwapInstrumentKey;
}

export interface GetCreditLinkedSwapInstrumentResponse {
    result: Result;
    credit_linked_swap_instrument: CreditLinkedSwapInstrument | null;
}

export interface GetManyCreditLinkedSwapInstrumentsRequest {
    keys: CreditLinkedSwapInstrumentKey[];
}

export interface GetManyCreditLinkedSwapInstrumentsResponse {
    result: Result;
    entries: CreditLinkedSwapInstrumentLookup[];
}

export interface PutCreditLinkedSwapInstrumentRequest {
    change: CreditLinkedSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutCreditLinkedSwapInstrumentResponse {
    result: Result;
    credit_linked_swap_instrument: CreditLinkedSwapInstrument | null;
}

export interface PutManyCreditLinkedSwapInstrumentsRequest {
    changes: CreditLinkedSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyCreditLinkedSwapInstrumentsResponse {
    result: Result;
    credit_linked_swap_instruments: CreditLinkedSwapInstrument[];
}

export interface DeleteCreditLinkedSwapInstrumentRequest {
    removal: CreditLinkedSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteCreditLinkedSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyCreditLinkedSwapInstrumentsRequest {
    removals: CreditLinkedSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCreditLinkedSwapInstrumentsResponse {
    result: Result;
}

export interface ListCreditLinkedSwapInstrumentVersionsRequest {
    key: CreditLinkedSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditLinkedSwapInstrumentVersionsFilter | null;
}

export interface ListCreditLinkedSwapInstrumentVersionsResponse {
    result: Result;
    versions: CreditLinkedSwapInstrument[];
    total: number;
}

export interface GetCreditLinkedSwapInstrumentVersionRequest {
    key: CreditLinkedSwapInstrumentVersionKey;
}

export interface GetCreditLinkedSwapInstrumentVersionResponse {
    result: Result;
    version: CreditLinkedSwapInstrument | null;
}

export const subjects = {
    list_credit_linked_swap_instruments_request: 'trading.v1.credit_linked_swap_instruments.list',
    get_credit_linked_swap_instrument_request: 'trading.v1.credit_linked_swap_instruments.get',
    get_many_credit_linked_swap_instruments_request:
        'trading.v1.credit_linked_swap_instruments.get_many',
    put_credit_linked_swap_instrument_request: 'trading.v1.credit_linked_swap_instruments.put',
    put_many_credit_linked_swap_instruments_request:
        'trading.v1.credit_linked_swap_instruments.put_many',
    delete_credit_linked_swap_instrument_request:
        'trading.v1.credit_linked_swap_instruments.delete',
    delete_many_credit_linked_swap_instruments_request:
        'trading.v1.credit_linked_swap_instruments.delete_many',
    list_credit_linked_swap_instrument_versions_request:
        'trading.v1.credit_linked_swap_instruments_versions.list',
    get_credit_linked_swap_instrument_version_request:
        'trading.v1.credit_linked_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_credit_linked_swap_instruments_request: true,
    get_credit_linked_swap_instrument_request: true,
    get_many_credit_linked_swap_instruments_request: true,
    put_credit_linked_swap_instrument_request: true,
    put_many_credit_linked_swap_instruments_request: true,
    delete_credit_linked_swap_instrument_request: true,
    delete_many_credit_linked_swap_instruments_request: true,
    list_credit_linked_swap_instrument_versions_request: true,
    get_credit_linked_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.credit_linked_swap_instruments_events.created',
    updated: 'trading.v1.credit_linked_swap_instruments_events.updated',
    deleted: 'trading.v1.credit_linked_swap_instruments_events.deleted',
} as const;
