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
import type { InstrumentOptionExerciseFee } from '../domain/instrument_option_exercise_fee.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InstrumentOptionExerciseFeeKey {
    trade_id: string;
    sequence_number: number;
}

export interface InstrumentOptionExerciseFeeWrite {
    trade_id: string;
    sequence_number: number;
    amount: string;
    type: string | null;
    start_date: string | null;
    currency: string | null;
}

export interface InstrumentOptionExerciseFeeChange {
    write: InstrumentOptionExerciseFeeWrite;
    precondition: Precondition;
}

export interface InstrumentOptionExerciseFeeRemoval {
    key: InstrumentOptionExerciseFeeKey;
    precondition: Precondition;
}

export interface InstrumentOptionExerciseFeeLookup {
    key: InstrumentOptionExerciseFeeKey;
    instrument_option_exercise_fee: InstrumentOptionExerciseFee | null;
}

export interface InstrumentOptionExerciseFeeEvent {
    event_id: string;
    key: InstrumentOptionExerciseFeeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InstrumentOptionExerciseFeeVersionKey {
    instrument_option_exercise_fee: InstrumentOptionExerciseFeeKey;
    version: number;
}

export interface InstrumentOptionExerciseFeeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInstrumentOptionExerciseFeesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListInstrumentOptionExerciseFeesResponse {
    result: Result;
    exercise_fees: InstrumentOptionExerciseFee[];
    total: number;
}

export interface GetInstrumentOptionExerciseFeeRequest {
    key: InstrumentOptionExerciseFeeKey;
}

export interface GetInstrumentOptionExerciseFeeResponse {
    result: Result;
    instrument_option_exercise_fee: InstrumentOptionExerciseFee | null;
}

export interface GetManyInstrumentOptionExerciseFeesRequest {
    keys: InstrumentOptionExerciseFeeKey[];
}

export interface GetManyInstrumentOptionExerciseFeesResponse {
    result: Result;
    entries: InstrumentOptionExerciseFeeLookup[];
}

export interface PutInstrumentOptionExerciseFeeRequest {
    change: InstrumentOptionExerciseFeeChange;
    intent: ChangeIntent;
}

export interface PutInstrumentOptionExerciseFeeResponse {
    result: Result;
    instrument_option_exercise_fee: InstrumentOptionExerciseFee | null;
}

export interface PutManyInstrumentOptionExerciseFeesRequest {
    changes: InstrumentOptionExerciseFeeChange[];
    intent: ChangeIntent;
}

export interface PutManyInstrumentOptionExerciseFeesResponse {
    result: Result;
    exercise_fees: InstrumentOptionExerciseFee[];
}

export interface DeleteInstrumentOptionExerciseFeeRequest {
    removal: InstrumentOptionExerciseFeeRemoval;
    intent: ChangeIntent;
}

export interface DeleteInstrumentOptionExerciseFeeResponse {
    result: Result;
}

export interface DeleteManyInstrumentOptionExerciseFeesRequest {
    removals: InstrumentOptionExerciseFeeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInstrumentOptionExerciseFeesResponse {
    result: Result;
}

export interface ListInstrumentOptionExerciseFeeVersionsRequest {
    key: InstrumentOptionExerciseFeeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentOptionExerciseFeeVersionsFilter | null;
}

export interface ListInstrumentOptionExerciseFeeVersionsResponse {
    result: Result;
    versions: InstrumentOptionExerciseFee[];
    total: number;
}

export interface GetInstrumentOptionExerciseFeeVersionRequest {
    key: InstrumentOptionExerciseFeeVersionKey;
}

export interface GetInstrumentOptionExerciseFeeVersionResponse {
    result: Result;
    version: InstrumentOptionExerciseFee | null;
}

export const subjects = {
    list_instrument_option_exercise_fees_request: 'trading.v1.instrument_option_exercise_fees.list',
    get_instrument_option_exercise_fee_request: 'trading.v1.instrument_option_exercise_fees.get',
    get_many_instrument_option_exercise_fees_request:
        'trading.v1.instrument_option_exercise_fees.get_many',
    put_instrument_option_exercise_fee_request: 'trading.v1.instrument_option_exercise_fees.put',
    put_many_instrument_option_exercise_fees_request:
        'trading.v1.instrument_option_exercise_fees.put_many',
    delete_instrument_option_exercise_fee_request:
        'trading.v1.instrument_option_exercise_fees.delete',
    delete_many_instrument_option_exercise_fees_request:
        'trading.v1.instrument_option_exercise_fees.delete_many',
    list_instrument_option_exercise_fee_versions_request:
        'trading.v1.instrument_option_exercise_fees_versions.list',
    get_instrument_option_exercise_fee_version_request:
        'trading.v1.instrument_option_exercise_fees_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_instrument_option_exercise_fees_request: true,
    get_instrument_option_exercise_fee_request: true,
    get_many_instrument_option_exercise_fees_request: true,
    put_instrument_option_exercise_fee_request: true,
    put_many_instrument_option_exercise_fees_request: true,
    delete_instrument_option_exercise_fee_request: true,
    delete_many_instrument_option_exercise_fees_request: true,
    list_instrument_option_exercise_fee_versions_request: true,
    get_instrument_option_exercise_fee_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.instrument_option_exercise_fees_events.created',
    updated: 'trading.v1.instrument_option_exercise_fees_events.updated',
    deleted: 'trading.v1.instrument_option_exercise_fees_events.deleted',
} as const;
