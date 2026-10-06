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
import type { CallableSwapCallDate } from '../domain/callable_swap_call_date.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CallableSwapCallDateKey {
    trade_id: string;
    sequence_number: number;
}

export interface CallableSwapCallDateWrite {
    trade_id: string;
    sequence_number: number;
    call_date: string;
}

export interface CallableSwapCallDateChange {
    write: CallableSwapCallDateWrite;
    precondition: Precondition;
}

export interface CallableSwapCallDateRemoval {
    key: CallableSwapCallDateKey;
    precondition: Precondition;
}

export interface CallableSwapCallDateLookup {
    key: CallableSwapCallDateKey;
    callable_swap_call_date: CallableSwapCallDate | null;
}

export interface CallableSwapCallDateEvent {
    event_id: string;
    key: CallableSwapCallDateKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CallableSwapCallDateVersionKey {
    callable_swap_call_date: CallableSwapCallDateKey;
    version: number;
}

export interface CallableSwapCallDateVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCallableSwapCallDatesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListCallableSwapCallDatesResponse {
    result: Result;
    callable_swap_call_dates: CallableSwapCallDate[];
    total: number;
}

export interface GetCallableSwapCallDateRequest {
    key: CallableSwapCallDateKey;
}

export interface GetCallableSwapCallDateResponse {
    result: Result;
    callable_swap_call_date: CallableSwapCallDate | null;
}

export interface GetManyCallableSwapCallDatesRequest {
    keys: CallableSwapCallDateKey[];
}

export interface GetManyCallableSwapCallDatesResponse {
    result: Result;
    entries: CallableSwapCallDateLookup[];
}

export interface PutCallableSwapCallDateRequest {
    change: CallableSwapCallDateChange;
    intent: ChangeIntent;
}

export interface PutCallableSwapCallDateResponse {
    result: Result;
    callable_swap_call_date: CallableSwapCallDate | null;
}

export interface PutManyCallableSwapCallDatesRequest {
    changes: CallableSwapCallDateChange[];
    intent: ChangeIntent;
}

export interface PutManyCallableSwapCallDatesResponse {
    result: Result;
    callable_swap_call_dates: CallableSwapCallDate[];
}

export interface DeleteCallableSwapCallDateRequest {
    removal: CallableSwapCallDateRemoval;
    intent: ChangeIntent;
}

export interface DeleteCallableSwapCallDateResponse {
    result: Result;
}

export interface DeleteManyCallableSwapCallDatesRequest {
    removals: CallableSwapCallDateRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCallableSwapCallDatesResponse {
    result: Result;
}

export interface ListCallableSwapCallDateVersionsRequest {
    key: CallableSwapCallDateKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CallableSwapCallDateVersionsFilter | null;
}

export interface ListCallableSwapCallDateVersionsResponse {
    result: Result;
    versions: CallableSwapCallDate[];
    total: number;
}

export interface GetCallableSwapCallDateVersionRequest {
    key: CallableSwapCallDateVersionKey;
}

export interface GetCallableSwapCallDateVersionResponse {
    result: Result;
    version: CallableSwapCallDate | null;
}

export const subjects = {
    list_callable_swap_call_dates_request: 'trading.v1.callable_swap_call_dates.list',
    get_callable_swap_call_date_request: 'trading.v1.callable_swap_call_dates.get',
    get_many_callable_swap_call_dates_request: 'trading.v1.callable_swap_call_dates.get_many',
    put_callable_swap_call_date_request: 'trading.v1.callable_swap_call_dates.put',
    put_many_callable_swap_call_dates_request: 'trading.v1.callable_swap_call_dates.put_many',
    delete_callable_swap_call_date_request: 'trading.v1.callable_swap_call_dates.delete',
    delete_many_callable_swap_call_dates_request: 'trading.v1.callable_swap_call_dates.delete_many',
    list_callable_swap_call_date_versions_request:
        'trading.v1.callable_swap_call_dates_versions.list',
    get_callable_swap_call_date_version_request: 'trading.v1.callable_swap_call_dates_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_callable_swap_call_dates_request: true,
    get_callable_swap_call_date_request: true,
    get_many_callable_swap_call_dates_request: true,
    put_callable_swap_call_date_request: true,
    put_many_callable_swap_call_dates_request: true,
    delete_callable_swap_call_date_request: true,
    delete_many_callable_swap_call_dates_request: true,
    list_callable_swap_call_date_versions_request: true,
    get_callable_swap_call_date_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.callable_swap_call_dates_events.created',
    updated: 'trading.v1.callable_swap_call_dates_events.updated',
    deleted: 'trading.v1.callable_swap_call_dates_events.deleted',
} as const;
