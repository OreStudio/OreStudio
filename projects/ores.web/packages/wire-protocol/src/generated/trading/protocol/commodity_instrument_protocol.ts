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
import type { CommodityInstrument } from '../domain/commodity_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CommodityInstrumentKey {
    trade_id: string;
}

export interface CommodityInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    commodity_code: string;
    currency: string;
    quantity: number;
    unit: string;
    start_date: string | null;
    maturity_date: string | null;
    fixed_price: string | null;
    option_type: string;
    strike_price: string | null;
    exercise_type: string;
    average_type: string;
    averaging_start_date: string | null;
    averaging_end_date: string | null;
    spread_commodity_code: string;
    spread_amount: string | null;
    strip_frequency_code: string;
    variance_strike: number | null;
    accumulation_amount: string | null;
    knock_out_barrier: string | null;
    barrier_type: string;
    lower_barrier: string | null;
    upper_barrier: string | null;
    day_count_fraction_code: string;
    payment_frequency_code: string;
    swaption_expiry_date: string | null;
    description: string;
}

export interface CommodityInstrumentChange {
    write: CommodityInstrumentWrite;
    precondition: Precondition;
}

export interface CommodityInstrumentRemoval {
    key: CommodityInstrumentKey;
    precondition: Precondition;
}

export interface CommodityInstrumentLookup {
    key: CommodityInstrumentKey;
    commodity_instrument: CommodityInstrument | null;
}

export interface CommodityInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface CommodityInstrumentEvent {
    event_id: string;
    key: CommodityInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityInstrumentVersionKey {
    commodity_instrument: CommodityInstrumentKey;
    version: number;
}

export interface CommodityInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListCommodityInstrumentsResponse {
    result: Result;
    commodity_instruments: CommodityInstrument[];
    total: number;
}

export interface GetCommodityInstrumentRequest {
    key: CommodityInstrumentKey;
}

export interface GetCommodityInstrumentResponse {
    result: Result;
    commodity_instrument: CommodityInstrument | null;
}

export interface GetManyCommodityInstrumentsRequest {
    keys: CommodityInstrumentKey[];
}

export interface GetManyCommodityInstrumentsResponse {
    result: Result;
    entries: CommodityInstrumentLookup[];
}

export interface PutCommodityInstrumentRequest {
    change: CommodityInstrumentChange;
    intent: ChangeIntent;
}

export interface PutCommodityInstrumentResponse {
    result: Result;
    commodity_instrument: CommodityInstrument | null;
}

export interface PutManyCommodityInstrumentsRequest {
    changes: CommodityInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityInstrumentsResponse {
    result: Result;
    commodity_instruments: CommodityInstrument[];
}

export interface DeleteCommodityInstrumentRequest {
    removal: CommodityInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityInstrumentResponse {
    result: Result;
}

export interface DeleteManyCommodityInstrumentsRequest {
    removals: CommodityInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityInstrumentsResponse {
    result: Result;
}

export interface ListCommodityInstrumentVersionsRequest {
    key: CommodityInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityInstrumentVersionsFilter | null;
}

export interface ListCommodityInstrumentVersionsResponse {
    result: Result;
    versions: CommodityInstrument[];
    total: number;
}

export interface GetCommodityInstrumentVersionRequest {
    key: CommodityInstrumentVersionKey;
}

export interface GetCommodityInstrumentVersionResponse {
    result: Result;
    version: CommodityInstrument | null;
}

export const subjects = {
    list_commodity_instruments_request: 'trading.v1.commodity_instruments.list',
    get_commodity_instrument_request: 'trading.v1.commodity_instruments.get',
    get_many_commodity_instruments_request: 'trading.v1.commodity_instruments.get_many',
    put_commodity_instrument_request: 'trading.v1.commodity_instruments.put',
    put_many_commodity_instruments_request: 'trading.v1.commodity_instruments.put_many',
    delete_commodity_instrument_request: 'trading.v1.commodity_instruments.delete',
    delete_many_commodity_instruments_request: 'trading.v1.commodity_instruments.delete_many',
    list_commodity_instrument_versions_request: 'trading.v1.commodity_instruments_versions.list',
    get_commodity_instrument_version_request: 'trading.v1.commodity_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_instruments_request: true,
    get_commodity_instrument_request: true,
    get_many_commodity_instruments_request: true,
    put_commodity_instrument_request: true,
    put_many_commodity_instruments_request: true,
    delete_commodity_instrument_request: true,
    delete_many_commodity_instruments_request: true,
    list_commodity_instrument_versions_request: true,
    get_commodity_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.commodity_instruments_events.created',
    updated: 'trading.v1.commodity_instruments_events.updated',
    deleted: 'trading.v1.commodity_instruments_events.deleted',
} as const;
