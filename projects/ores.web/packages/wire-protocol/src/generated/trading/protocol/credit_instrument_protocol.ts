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
import type { CreditInstrument } from '../domain/credit_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CreditInstrumentKey {
    trade_id: string;
}

export interface CreditInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    reference_entity: string;
    currency: string;
    notional: string;
    spread: number;
    recovery_rate: number;
    tenor: string;
    start_date: string;
    maturity_date: string;
    day_count_fraction_code: string;
    payment_frequency_code: string;
    index_name: string;
    index_series: number | null;
    seniority: string;
    restructuring: string;
    description: string;
    option_type: string;
    option_expiry_date: string | null;
    option_strike: number | null;
    linked_asset_code: string;
    tranche_attachment: number | null;
    tranche_detachment: number | null;
}

export interface CreditInstrumentChange {
    write: CreditInstrumentWrite;
    precondition: Precondition;
}

export interface CreditInstrumentRemoval {
    key: CreditInstrumentKey;
    precondition: Precondition;
}

export interface CreditInstrumentLookup {
    key: CreditInstrumentKey;
    credit_instrument: CreditInstrument | null;
}

export interface CreditInstrumentEvent {
    event_id: string;
    key: CreditInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CreditInstrumentVersionKey {
    credit_instrument: CreditInstrumentKey;
    version: number;
}

export interface CreditInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCreditInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCreditInstrumentsResponse {
    result: Result;
    credit_instruments: CreditInstrument[];
    total: number;
}

export interface GetCreditInstrumentRequest {
    key: CreditInstrumentKey;
}

export interface GetCreditInstrumentResponse {
    result: Result;
    credit_instrument: CreditInstrument | null;
}

export interface GetManyCreditInstrumentsRequest {
    keys: CreditInstrumentKey[];
}

export interface GetManyCreditInstrumentsResponse {
    result: Result;
    entries: CreditInstrumentLookup[];
}

export interface PutCreditInstrumentRequest {
    change: CreditInstrumentChange;
    intent: ChangeIntent;
}

export interface PutCreditInstrumentResponse {
    result: Result;
    credit_instrument: CreditInstrument | null;
}

export interface PutManyCreditInstrumentsRequest {
    changes: CreditInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyCreditInstrumentsResponse {
    result: Result;
    credit_instruments: CreditInstrument[];
}

export interface DeleteCreditInstrumentRequest {
    removal: CreditInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteCreditInstrumentResponse {
    result: Result;
}

export interface DeleteManyCreditInstrumentsRequest {
    removals: CreditInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCreditInstrumentsResponse {
    result: Result;
}

export interface ListCreditInstrumentVersionsRequest {
    key: CreditInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditInstrumentVersionsFilter | null;
}

export interface ListCreditInstrumentVersionsResponse {
    result: Result;
    versions: CreditInstrument[];
    total: number;
}

export interface GetCreditInstrumentVersionRequest {
    key: CreditInstrumentVersionKey;
}

export interface GetCreditInstrumentVersionResponse {
    result: Result;
    version: CreditInstrument | null;
}

export const subjects = {
    list_credit_instruments_request: 'trading.v1.credit_instruments.list',
    get_credit_instrument_request: 'trading.v1.credit_instruments.get',
    get_many_credit_instruments_request: 'trading.v1.credit_instruments.get_many',
    put_credit_instrument_request: 'trading.v1.credit_instruments.put',
    put_many_credit_instruments_request: 'trading.v1.credit_instruments.put_many',
    delete_credit_instrument_request: 'trading.v1.credit_instruments.delete',
    delete_many_credit_instruments_request: 'trading.v1.credit_instruments.delete_many',
    list_credit_instrument_versions_request: 'trading.v1.credit_instruments_versions.list',
    get_credit_instrument_version_request: 'trading.v1.credit_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_credit_instruments_request: true,
    get_credit_instrument_request: true,
    get_many_credit_instruments_request: true,
    put_credit_instrument_request: true,
    put_many_credit_instruments_request: true,
    delete_credit_instrument_request: true,
    delete_many_credit_instruments_request: true,
    list_credit_instrument_versions_request: true,
    get_credit_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.credit_instruments_events.created',
    updated: 'trading.v1.credit_instruments_events.updated',
    deleted: 'trading.v1.credit_instruments_events.deleted',
} as const;
