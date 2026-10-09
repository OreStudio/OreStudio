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
import type { InstrumentOptionExercisePrice } from '../domain/instrument_option_exercise_price.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InstrumentOptionExercisePriceKey {
    trade_id: string;
    sequence_number: number;
}

export interface InstrumentOptionExercisePriceWrite {
    trade_id: string;
    sequence_number: number;
    trade_activity_id: string;
    exercise_date: string;
    price: string;
}

export interface InstrumentOptionExercisePriceChange {
    write: InstrumentOptionExercisePriceWrite;
    precondition: Precondition;
}

export interface InstrumentOptionExercisePriceRemoval {
    key: InstrumentOptionExercisePriceKey;
    precondition: Precondition;
}

export interface InstrumentOptionExercisePriceLookup {
    key: InstrumentOptionExercisePriceKey;
    instrument_option_exercise_price: InstrumentOptionExercisePrice | null;
}

export interface InstrumentOptionExercisePriceEvent {
    event_id: string;
    key: InstrumentOptionExercisePriceKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InstrumentOptionExercisePriceVersionKey {
    instrument_option_exercise_price: InstrumentOptionExercisePriceKey;
    version: number;
}

export interface InstrumentOptionExercisePriceVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInstrumentOptionExercisePricesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListInstrumentOptionExercisePricesResponse {
    result: Result;
    exercise_prices: InstrumentOptionExercisePrice[];
    total: number;
}

export interface GetInstrumentOptionExercisePriceRequest {
    key: InstrumentOptionExercisePriceKey;
}

export interface GetInstrumentOptionExercisePriceResponse {
    result: Result;
    instrument_option_exercise_price: InstrumentOptionExercisePrice | null;
}

export interface GetManyInstrumentOptionExercisePricesRequest {
    keys: InstrumentOptionExercisePriceKey[];
}

export interface GetManyInstrumentOptionExercisePricesResponse {
    result: Result;
    entries: InstrumentOptionExercisePriceLookup[];
}

export interface PutInstrumentOptionExercisePriceRequest {
    change: InstrumentOptionExercisePriceChange;
    intent: ChangeIntent;
}

export interface PutInstrumentOptionExercisePriceResponse {
    result: Result;
    instrument_option_exercise_price: InstrumentOptionExercisePrice | null;
}

export interface PutManyInstrumentOptionExercisePricesRequest {
    changes: InstrumentOptionExercisePriceChange[];
    intent: ChangeIntent;
}

export interface PutManyInstrumentOptionExercisePricesResponse {
    result: Result;
    exercise_prices: InstrumentOptionExercisePrice[];
}

export interface DeleteInstrumentOptionExercisePriceRequest {
    removal: InstrumentOptionExercisePriceRemoval;
    intent: ChangeIntent;
}

export interface DeleteInstrumentOptionExercisePriceResponse {
    result: Result;
}

export interface DeleteManyInstrumentOptionExercisePricesRequest {
    removals: InstrumentOptionExercisePriceRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInstrumentOptionExercisePricesResponse {
    result: Result;
}

export interface ListInstrumentOptionExercisePriceVersionsRequest {
    key: InstrumentOptionExercisePriceKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentOptionExercisePriceVersionsFilter | null;
}

export interface ListInstrumentOptionExercisePriceVersionsResponse {
    result: Result;
    versions: InstrumentOptionExercisePrice[];
    total: number;
}

export interface GetInstrumentOptionExercisePriceVersionRequest {
    key: InstrumentOptionExercisePriceVersionKey;
}

export interface GetInstrumentOptionExercisePriceVersionResponse {
    result: Result;
    version: InstrumentOptionExercisePrice | null;
}

export const subjects = {
    list_instrument_option_exercise_prices_request:
        'trading.v1.instrument_option_exercise_prices.list',
    get_instrument_option_exercise_price_request:
        'trading.v1.instrument_option_exercise_prices.get',
    get_many_instrument_option_exercise_prices_request:
        'trading.v1.instrument_option_exercise_prices.get_many',
    put_instrument_option_exercise_price_request:
        'trading.v1.instrument_option_exercise_prices.put',
    put_many_instrument_option_exercise_prices_request:
        'trading.v1.instrument_option_exercise_prices.put_many',
    delete_instrument_option_exercise_price_request:
        'trading.v1.instrument_option_exercise_prices.delete',
    delete_many_instrument_option_exercise_prices_request:
        'trading.v1.instrument_option_exercise_prices.delete_many',
    list_instrument_option_exercise_price_versions_request:
        'trading.v1.instrument_option_exercise_prices_versions.list',
    get_instrument_option_exercise_price_version_request:
        'trading.v1.instrument_option_exercise_prices_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_instrument_option_exercise_prices_request: true,
    get_instrument_option_exercise_price_request: true,
    get_many_instrument_option_exercise_prices_request: true,
    put_instrument_option_exercise_price_request: true,
    put_many_instrument_option_exercise_prices_request: true,
    delete_instrument_option_exercise_price_request: true,
    delete_many_instrument_option_exercise_prices_request: true,
    list_instrument_option_exercise_price_versions_request: true,
    get_instrument_option_exercise_price_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.instrument_option_exercise_prices_events.created',
    updated: 'trading.v1.instrument_option_exercise_prices_events.updated',
    deleted: 'trading.v1.instrument_option_exercise_prices_events.deleted',
} as const;
