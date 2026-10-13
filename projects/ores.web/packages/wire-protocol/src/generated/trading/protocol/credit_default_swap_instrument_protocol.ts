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
import type { CreditDefaultSwapInstrument } from '../domain/credit_default_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CreditDefaultSwapInstrumentKey {
    trade_id: string;
}

export interface CreditDefaultSwapInstrumentWrite {
    trade_id: string;
    trade_activity_id: string;
    credit_curve_id: string | null;
    issuer_id: string | null;
    recovery_rate: number | null;
    reference_obligation: string | null;
    settles_accrual: boolean | null;
    rebates_accrual: boolean | null;
    pays_at_default_time: boolean | null;
    protection_payment_time: string | null;
    protection_start_date: string | null;
    upfront_date: string | null;
    upfront_fee: string | null;
    trade_date: string | null;
    cash_settlement_days: number | null;
}

export interface CreditDefaultSwapInstrumentChange {
    write: CreditDefaultSwapInstrumentWrite;
    precondition: Precondition;
}

export interface CreditDefaultSwapInstrumentRemoval {
    key: CreditDefaultSwapInstrumentKey;
    precondition: Precondition;
}

export interface CreditDefaultSwapInstrumentLookup {
    key: CreditDefaultSwapInstrumentKey;
    credit_default_swap_instrument: CreditDefaultSwapInstrument | null;
}

export interface CreditDefaultSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface CreditDefaultSwapInstrumentEvent {
    event_id: string;
    key: CreditDefaultSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CreditDefaultSwapInstrumentVersionKey {
    credit_default_swap_instrument: CreditDefaultSwapInstrumentKey;
    version: number;
}

export interface CreditDefaultSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCreditDefaultSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CreditDefaultSwapInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListCreditDefaultSwapInstrumentsResponse {
    result: Result;
    cds_instruments: CreditDefaultSwapInstrument[];
    total: number;
}

export interface GetCreditDefaultSwapInstrumentRequest {
    key: CreditDefaultSwapInstrumentKey;
}

export interface GetCreditDefaultSwapInstrumentResponse {
    result: Result;
    credit_default_swap_instrument: CreditDefaultSwapInstrument | null;
}

export interface GetManyCreditDefaultSwapInstrumentsRequest {
    keys: CreditDefaultSwapInstrumentKey[];
}

export interface GetManyCreditDefaultSwapInstrumentsResponse {
    result: Result;
    entries: CreditDefaultSwapInstrumentLookup[];
}

export interface PutCreditDefaultSwapInstrumentRequest {
    change: CreditDefaultSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutCreditDefaultSwapInstrumentResponse {
    result: Result;
    credit_default_swap_instrument: CreditDefaultSwapInstrument | null;
}

export interface PutManyCreditDefaultSwapInstrumentsRequest {
    changes: CreditDefaultSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyCreditDefaultSwapInstrumentsResponse {
    result: Result;
    cds_instruments: CreditDefaultSwapInstrument[];
}

export interface DeleteCreditDefaultSwapInstrumentRequest {
    removal: CreditDefaultSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteCreditDefaultSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyCreditDefaultSwapInstrumentsRequest {
    removals: CreditDefaultSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCreditDefaultSwapInstrumentsResponse {
    result: Result;
}

export interface ListCreditDefaultSwapInstrumentVersionsRequest {
    key: CreditDefaultSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditDefaultSwapInstrumentVersionsFilter | null;
}

export interface ListCreditDefaultSwapInstrumentVersionsResponse {
    result: Result;
    versions: CreditDefaultSwapInstrument[];
    total: number;
}

export interface GetCreditDefaultSwapInstrumentVersionRequest {
    key: CreditDefaultSwapInstrumentVersionKey;
}

export interface GetCreditDefaultSwapInstrumentVersionResponse {
    result: Result;
    version: CreditDefaultSwapInstrument | null;
}

export const subjects = {
    list_credit_default_swap_instruments_request: 'trading.v1.credit_default_swap_instruments.list',
    get_credit_default_swap_instrument_request: 'trading.v1.credit_default_swap_instruments.get',
    get_many_credit_default_swap_instruments_request:
        'trading.v1.credit_default_swap_instruments.get_many',
    put_credit_default_swap_instrument_request: 'trading.v1.credit_default_swap_instruments.put',
    put_many_credit_default_swap_instruments_request:
        'trading.v1.credit_default_swap_instruments.put_many',
    delete_credit_default_swap_instrument_request:
        'trading.v1.credit_default_swap_instruments.delete',
    delete_many_credit_default_swap_instruments_request:
        'trading.v1.credit_default_swap_instruments.delete_many',
    list_credit_default_swap_instrument_versions_request:
        'trading.v1.credit_default_swap_instruments_versions.list',
    get_credit_default_swap_instrument_version_request:
        'trading.v1.credit_default_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_credit_default_swap_instruments_request: true,
    get_credit_default_swap_instrument_request: true,
    get_many_credit_default_swap_instruments_request: true,
    put_credit_default_swap_instrument_request: true,
    put_many_credit_default_swap_instruments_request: true,
    delete_credit_default_swap_instrument_request: true,
    delete_many_credit_default_swap_instruments_request: true,
    list_credit_default_swap_instrument_versions_request: true,
    get_credit_default_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.credit_default_swap_instruments_events.created',
    updated: 'trading.v1.credit_default_swap_instruments_events.updated',
    deleted: 'trading.v1.credit_default_swap_instruments_events.deleted',
} as const;
