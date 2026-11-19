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
import type { EquityForwardVolatilityAgreementInstrument } from '../domain/equity_forward_volatility_agreement_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityForwardVolatilityAgreementInstrumentKey {
    trade_id: string;
}

export interface EquityForwardVolatilityAgreementInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    fva_date: string;
    option_expiry: string;
    premium_date: string;
    long_short: string;
    implied_vol_strike: number;
    quantity: number;
    pay_currency: string;
    settlement_date: string;
    underlying_strike: string | null;
    day_count_fraction: string | null;
    dividend_yield: string | null;
    fixed_rate: string | null;
    payoff_type: string | null;
    description: string;
}

export interface EquityForwardVolatilityAgreementInstrumentChange {
    write: EquityForwardVolatilityAgreementInstrumentWrite;
    precondition: Precondition;
}

export interface EquityForwardVolatilityAgreementInstrumentRemoval {
    key: EquityForwardVolatilityAgreementInstrumentKey;
    precondition: Precondition;
}

export interface EquityForwardVolatilityAgreementInstrumentLookup {
    key: EquityForwardVolatilityAgreementInstrumentKey;
    equity_forward_volatility_agreement_instrument: EquityForwardVolatilityAgreementInstrument | null;
}

export interface EquityForwardVolatilityAgreementInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface EquityForwardVolatilityAgreementInstrumentEvent {
    event_id: string;
    key: EquityForwardVolatilityAgreementInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityForwardVolatilityAgreementInstrumentVersionKey {
    equity_forward_volatility_agreement_instrument: EquityForwardVolatilityAgreementInstrumentKey;
    version: number;
}

export interface EquityForwardVolatilityAgreementInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityForwardVolatilityAgreementInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EquityForwardVolatilityAgreementInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListEquityForwardVolatilityAgreementInstrumentsResponse {
    result: Result;
    equity_forward_volatility_agreement_instruments: EquityForwardVolatilityAgreementInstrument[];
    total: number;
}

export interface GetEquityForwardVolatilityAgreementInstrumentRequest {
    key: EquityForwardVolatilityAgreementInstrumentKey;
}

export interface GetEquityForwardVolatilityAgreementInstrumentResponse {
    result: Result;
    equity_forward_volatility_agreement_instrument: EquityForwardVolatilityAgreementInstrument | null;
}

export interface GetManyEquityForwardVolatilityAgreementInstrumentsRequest {
    keys: EquityForwardVolatilityAgreementInstrumentKey[];
}

export interface GetManyEquityForwardVolatilityAgreementInstrumentsResponse {
    result: Result;
    entries: EquityForwardVolatilityAgreementInstrumentLookup[];
}

export interface PutEquityForwardVolatilityAgreementInstrumentRequest {
    change: EquityForwardVolatilityAgreementInstrumentChange;
    intent: ChangeIntent;
}

export interface PutEquityForwardVolatilityAgreementInstrumentResponse {
    result: Result;
    equity_forward_volatility_agreement_instrument: EquityForwardVolatilityAgreementInstrument | null;
}

export interface PutManyEquityForwardVolatilityAgreementInstrumentsRequest {
    changes: EquityForwardVolatilityAgreementInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityForwardVolatilityAgreementInstrumentsResponse {
    result: Result;
    equity_forward_volatility_agreement_instruments: EquityForwardVolatilityAgreementInstrument[];
}

export interface DeleteEquityForwardVolatilityAgreementInstrumentRequest {
    removal: EquityForwardVolatilityAgreementInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityForwardVolatilityAgreementInstrumentResponse {
    result: Result;
}

export interface DeleteManyEquityForwardVolatilityAgreementInstrumentsRequest {
    removals: EquityForwardVolatilityAgreementInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityForwardVolatilityAgreementInstrumentsResponse {
    result: Result;
}

export interface ListEquityForwardVolatilityAgreementInstrumentVersionsRequest {
    key: EquityForwardVolatilityAgreementInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityForwardVolatilityAgreementInstrumentVersionsFilter | null;
}

export interface ListEquityForwardVolatilityAgreementInstrumentVersionsResponse {
    result: Result;
    versions: EquityForwardVolatilityAgreementInstrument[];
    total: number;
}

export interface GetEquityForwardVolatilityAgreementInstrumentVersionRequest {
    key: EquityForwardVolatilityAgreementInstrumentVersionKey;
}

export interface GetEquityForwardVolatilityAgreementInstrumentVersionResponse {
    result: Result;
    version: EquityForwardVolatilityAgreementInstrument | null;
}

export const subjects = {
    list_equity_forward_volatility_agreement_instruments_request:
        'trading.v1.equity_forward_volatility_agreement_instruments.list',
    get_equity_forward_volatility_agreement_instrument_request:
        'trading.v1.equity_forward_volatility_agreement_instruments.get',
    get_many_equity_forward_volatility_agreement_instruments_request:
        'trading.v1.equity_forward_volatility_agreement_instruments.get_many',
    put_equity_forward_volatility_agreement_instrument_request:
        'trading.v1.equity_forward_volatility_agreement_instruments.put',
    put_many_equity_forward_volatility_agreement_instruments_request:
        'trading.v1.equity_forward_volatility_agreement_instruments.put_many',
    delete_equity_forward_volatility_agreement_instrument_request:
        'trading.v1.equity_forward_volatility_agreement_instruments.delete',
    delete_many_equity_forward_volatility_agreement_instruments_request:
        'trading.v1.equity_forward_volatility_agreement_instruments.delete_many',
    list_equity_forward_volatility_agreement_instrument_versions_request:
        'trading.v1.equity_forward_volatility_agreement_instruments_versions.list',
    get_equity_forward_volatility_agreement_instrument_version_request:
        'trading.v1.equity_forward_volatility_agreement_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_forward_volatility_agreement_instruments_request: true,
    get_equity_forward_volatility_agreement_instrument_request: true,
    get_many_equity_forward_volatility_agreement_instruments_request: true,
    put_equity_forward_volatility_agreement_instrument_request: true,
    put_many_equity_forward_volatility_agreement_instruments_request: true,
    delete_equity_forward_volatility_agreement_instrument_request: true,
    delete_many_equity_forward_volatility_agreement_instruments_request: true,
    list_equity_forward_volatility_agreement_instrument_versions_request: true,
    get_equity_forward_volatility_agreement_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.equity_forward_volatility_agreement_instruments.created',
    updated: 'trading.v1.events.equity_forward_volatility_agreement_instruments.updated',
    deleted: 'trading.v1.events.equity_forward_volatility_agreement_instruments.deleted',
} as const;
