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
import type { InstrumentOption } from '../domain/instrument_option.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InstrumentOptionKey {
    trade_id: string;
}

export interface InstrumentOptionWrite {
    trade_id: string;
    long_short: string;
    option_type: string | null;
    payoff_type: string | null;
    payoff_type_2: string | null;
    style: string | null;
    notice_period: string | null;
    notice_calendar: string | null;
    notice_convention: string | null;
    mid_coupon_exercise: string | null;
    settlement: string | null;
    settlement_method: string | null;
    pay_off_at_expiry: string | null;
    premium_amount: string | null;
    premium_currency: string | null;
    premium_pay_date: string | null;
    exercise_prices: string | null;
    exercise_fee_settlement_period: string | null;
    exercise_fee_settlement_calendar: string | null;
    exercise_fee_settlement_convention: string | null;
    automatic_exercise: string | null;
    has_exercise_data: boolean;
    exercise_date: string | null;
    exercise_price: string | null;
    has_payment_data: boolean;
    payment_lag: number | null;
    payment_calendar: string | null;
    payment_convention: string | null;
    payment_relative_to: string | null;
    has_settlement_data: boolean;
    settlement_pay_currency: string | null;
    settlement_fx_index: string | null;
    settlement_fixing_date: string | null;
}

export interface InstrumentOptionChange {
    write: InstrumentOptionWrite;
    precondition: Precondition;
}

export interface InstrumentOptionRemoval {
    key: InstrumentOptionKey;
    precondition: Precondition;
}

export interface InstrumentOptionLookup {
    key: InstrumentOptionKey;
    instrument_option: InstrumentOption | null;
}

export interface InstrumentOptionsFilter {
    trade_id_one_of: string[] | null;
}

export interface InstrumentOptionEvent {
    event_id: string;
    key: InstrumentOptionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InstrumentOptionVersionKey {
    instrument_option: InstrumentOptionKey;
    version: number;
}

export interface InstrumentOptionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInstrumentOptionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentOptionsFilter | null;
}

export interface ListInstrumentOptionsResponse {
    result: Result;
    instrument_options: InstrumentOption[];
    total: number;
}

export interface GetInstrumentOptionRequest {
    key: InstrumentOptionKey;
}

export interface GetInstrumentOptionResponse {
    result: Result;
    instrument_option: InstrumentOption | null;
}

export interface GetManyInstrumentOptionsRequest {
    keys: InstrumentOptionKey[];
}

export interface GetManyInstrumentOptionsResponse {
    result: Result;
    entries: InstrumentOptionLookup[];
}

export interface PutInstrumentOptionRequest {
    change: InstrumentOptionChange;
    intent: ChangeIntent;
}

export interface PutInstrumentOptionResponse {
    result: Result;
    instrument_option: InstrumentOption | null;
}

export interface PutManyInstrumentOptionsRequest {
    changes: InstrumentOptionChange[];
    intent: ChangeIntent;
}

export interface PutManyInstrumentOptionsResponse {
    result: Result;
    instrument_options: InstrumentOption[];
}

export interface DeleteInstrumentOptionRequest {
    removal: InstrumentOptionRemoval;
    intent: ChangeIntent;
}

export interface DeleteInstrumentOptionResponse {
    result: Result;
}

export interface DeleteManyInstrumentOptionsRequest {
    removals: InstrumentOptionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInstrumentOptionsResponse {
    result: Result;
}

export interface ListInstrumentOptionVersionsRequest {
    key: InstrumentOptionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentOptionVersionsFilter | null;
}

export interface ListInstrumentOptionVersionsResponse {
    result: Result;
    versions: InstrumentOption[];
    total: number;
}

export interface GetInstrumentOptionVersionRequest {
    key: InstrumentOptionVersionKey;
}

export interface GetInstrumentOptionVersionResponse {
    result: Result;
    version: InstrumentOption | null;
}

export const subjects = {
    list_instrument_options_request: 'trading.v1.instrument_options.list',
    get_instrument_option_request: 'trading.v1.instrument_options.get',
    get_many_instrument_options_request: 'trading.v1.instrument_options.get_many',
    put_instrument_option_request: 'trading.v1.instrument_options.put',
    put_many_instrument_options_request: 'trading.v1.instrument_options.put_many',
    delete_instrument_option_request: 'trading.v1.instrument_options.delete',
    delete_many_instrument_options_request: 'trading.v1.instrument_options.delete_many',
    list_instrument_option_versions_request: 'trading.v1.instrument_options_versions.list',
    get_instrument_option_version_request: 'trading.v1.instrument_options_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_instrument_options_request: true,
    get_instrument_option_request: true,
    get_many_instrument_options_request: true,
    put_instrument_option_request: true,
    put_many_instrument_options_request: true,
    delete_instrument_option_request: true,
    delete_many_instrument_options_request: true,
    list_instrument_option_versions_request: true,
    get_instrument_option_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.instrument_options_events.created',
    updated: 'trading.v1.instrument_options_events.updated',
    deleted: 'trading.v1.instrument_options_events.deleted',
} as const;
