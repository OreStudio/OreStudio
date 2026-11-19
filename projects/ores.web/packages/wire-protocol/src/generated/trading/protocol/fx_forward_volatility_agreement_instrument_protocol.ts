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
import type { FxForwardVolatilityAgreementInstrument } from '../domain/fx_forward_volatility_agreement_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxForwardVolatilityAgreementInstrumentKey {
    trade_id: string;
}

export interface FxForwardVolatilityAgreementInstrumentWrite {
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

export interface FxForwardVolatilityAgreementInstrumentChange {
    write: FxForwardVolatilityAgreementInstrumentWrite;
    precondition: Precondition;
}

export interface FxForwardVolatilityAgreementInstrumentRemoval {
    key: FxForwardVolatilityAgreementInstrumentKey;
    precondition: Precondition;
}

export interface FxForwardVolatilityAgreementInstrumentLookup {
    key: FxForwardVolatilityAgreementInstrumentKey;
    fx_forward_volatility_agreement_instrument: FxForwardVolatilityAgreementInstrument | null;
}

export interface FxForwardVolatilityAgreementInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface FxForwardVolatilityAgreementInstrumentEvent {
    event_id: string;
    key: FxForwardVolatilityAgreementInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxForwardVolatilityAgreementInstrumentVersionKey {
    fx_forward_volatility_agreement_instrument: FxForwardVolatilityAgreementInstrumentKey;
    version: number;
}

export interface FxForwardVolatilityAgreementInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxForwardVolatilityAgreementInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxForwardVolatilityAgreementInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListFxForwardVolatilityAgreementInstrumentsResponse {
    result: Result;
    fx_forward_volatility_agreement_instruments: FxForwardVolatilityAgreementInstrument[];
    total: number;
}

export interface GetFxForwardVolatilityAgreementInstrumentRequest {
    key: FxForwardVolatilityAgreementInstrumentKey;
}

export interface GetFxForwardVolatilityAgreementInstrumentResponse {
    result: Result;
    fx_forward_volatility_agreement_instrument: FxForwardVolatilityAgreementInstrument | null;
}

export interface GetManyFxForwardVolatilityAgreementInstrumentsRequest {
    keys: FxForwardVolatilityAgreementInstrumentKey[];
}

export interface GetManyFxForwardVolatilityAgreementInstrumentsResponse {
    result: Result;
    entries: FxForwardVolatilityAgreementInstrumentLookup[];
}

export interface PutFxForwardVolatilityAgreementInstrumentRequest {
    change: FxForwardVolatilityAgreementInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFxForwardVolatilityAgreementInstrumentResponse {
    result: Result;
    fx_forward_volatility_agreement_instrument: FxForwardVolatilityAgreementInstrument | null;
}

export interface PutManyFxForwardVolatilityAgreementInstrumentsRequest {
    changes: FxForwardVolatilityAgreementInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFxForwardVolatilityAgreementInstrumentsResponse {
    result: Result;
    fx_forward_volatility_agreement_instruments: FxForwardVolatilityAgreementInstrument[];
}

export interface DeleteFxForwardVolatilityAgreementInstrumentRequest {
    removal: FxForwardVolatilityAgreementInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxForwardVolatilityAgreementInstrumentResponse {
    result: Result;
}

export interface DeleteManyFxForwardVolatilityAgreementInstrumentsRequest {
    removals: FxForwardVolatilityAgreementInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxForwardVolatilityAgreementInstrumentsResponse {
    result: Result;
}

export interface ListFxForwardVolatilityAgreementInstrumentVersionsRequest {
    key: FxForwardVolatilityAgreementInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxForwardVolatilityAgreementInstrumentVersionsFilter | null;
}

export interface ListFxForwardVolatilityAgreementInstrumentVersionsResponse {
    result: Result;
    versions: FxForwardVolatilityAgreementInstrument[];
    total: number;
}

export interface GetFxForwardVolatilityAgreementInstrumentVersionRequest {
    key: FxForwardVolatilityAgreementInstrumentVersionKey;
}

export interface GetFxForwardVolatilityAgreementInstrumentVersionResponse {
    result: Result;
    version: FxForwardVolatilityAgreementInstrument | null;
}

export const subjects = {
    list_fx_forward_volatility_agreement_instruments_request:
        'trading.v1.fx_forward_volatility_agreement_instruments.list',
    get_fx_forward_volatility_agreement_instrument_request:
        'trading.v1.fx_forward_volatility_agreement_instruments.get',
    get_many_fx_forward_volatility_agreement_instruments_request:
        'trading.v1.fx_forward_volatility_agreement_instruments.get_many',
    put_fx_forward_volatility_agreement_instrument_request:
        'trading.v1.fx_forward_volatility_agreement_instruments.put',
    put_many_fx_forward_volatility_agreement_instruments_request:
        'trading.v1.fx_forward_volatility_agreement_instruments.put_many',
    delete_fx_forward_volatility_agreement_instrument_request:
        'trading.v1.fx_forward_volatility_agreement_instruments.delete',
    delete_many_fx_forward_volatility_agreement_instruments_request:
        'trading.v1.fx_forward_volatility_agreement_instruments.delete_many',
    list_fx_forward_volatility_agreement_instrument_versions_request:
        'trading.v1.fx_forward_volatility_agreement_instruments_versions.list',
    get_fx_forward_volatility_agreement_instrument_version_request:
        'trading.v1.fx_forward_volatility_agreement_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_forward_volatility_agreement_instruments_request: true,
    get_fx_forward_volatility_agreement_instrument_request: true,
    get_many_fx_forward_volatility_agreement_instruments_request: true,
    put_fx_forward_volatility_agreement_instrument_request: true,
    put_many_fx_forward_volatility_agreement_instruments_request: true,
    delete_fx_forward_volatility_agreement_instrument_request: true,
    delete_many_fx_forward_volatility_agreement_instruments_request: true,
    list_fx_forward_volatility_agreement_instrument_versions_request: true,
    get_fx_forward_volatility_agreement_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.fx_forward_volatility_agreement_instruments.created',
    updated: 'trading.v1.events.fx_forward_volatility_agreement_instruments.updated',
    deleted: 'trading.v1.events.fx_forward_volatility_agreement_instruments.deleted',
} as const;
