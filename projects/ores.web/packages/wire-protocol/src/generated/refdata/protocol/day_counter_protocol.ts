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
import type { DayCounter } from '../domain/day_counter.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DayCounterKey {
    code: string;
}

export interface DayCounterWrite {
    code: string;
    description: string;
}

export interface DayCounterChange {
    write: DayCounterWrite;
    precondition: Precondition;
}

export interface DayCounterRemoval {
    key: DayCounterKey;
    precondition: Precondition;
}

export interface DayCounterLookup {
    key: DayCounterKey;
    day_counter: DayCounter | null;
}

export interface DayCountersFilter {
    code_one_of: string[] | null;
}

export interface DayCounterEvent {
    event_id: string;
    key: DayCounterKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DayCounterVersionKey {
    day_counter: DayCounterKey;
    version: number;
}

export interface DayCounterVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDayCountersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: DayCountersFilter | null;
}

export interface ListDayCountersResponse {
    result: Result;
    day_counters: DayCounter[];
    total: number;
}

export interface GetDayCounterRequest {
    key: DayCounterKey;
}

export interface GetDayCounterResponse {
    result: Result;
    day_counter: DayCounter | null;
}

export interface GetManyDayCountersRequest {
    keys: DayCounterKey[];
}

export interface GetManyDayCountersResponse {
    result: Result;
    entries: DayCounterLookup[];
}

export interface PutDayCounterRequest {
    change: DayCounterChange;
    intent: ChangeIntent;
}

export interface PutDayCounterResponse {
    result: Result;
    day_counter: DayCounter | null;
}

export interface PutManyDayCountersRequest {
    changes: DayCounterChange[];
    intent: ChangeIntent;
}

export interface PutManyDayCountersResponse {
    result: Result;
    day_counters: DayCounter[];
}

export interface DeleteDayCounterRequest {
    removal: DayCounterRemoval;
    intent: ChangeIntent;
}

export interface DeleteDayCounterResponse {
    result: Result;
}

export interface DeleteManyDayCountersRequest {
    removals: DayCounterRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDayCountersResponse {
    result: Result;
}

export interface ListDayCounterVersionsRequest {
    key: DayCounterKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DayCounterVersionsFilter | null;
}

export interface ListDayCounterVersionsResponse {
    result: Result;
    versions: DayCounter[];
    total: number;
}

export interface GetDayCounterVersionRequest {
    key: DayCounterVersionKey;
}

export interface GetDayCounterVersionResponse {
    result: Result;
    version: DayCounter | null;
}

export const subjects = {
    list_day_counters_request: 'refdata.v1.day_counters.list',
    get_day_counter_request: 'refdata.v1.day_counters.get',
    get_many_day_counters_request: 'refdata.v1.day_counters.get_many',
    put_day_counter_request: 'refdata.v1.day_counters.put',
    put_many_day_counters_request: 'refdata.v1.day_counters.put_many',
    delete_day_counter_request: 'refdata.v1.day_counters.delete',
    delete_many_day_counters_request: 'refdata.v1.day_counters.delete_many',
    list_day_counter_versions_request: 'refdata.v1.day_counters_versions.list',
    get_day_counter_version_request: 'refdata.v1.day_counters_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_day_counters_request: true,
    get_day_counter_request: true,
    get_many_day_counters_request: true,
    put_day_counter_request: true,
    put_many_day_counters_request: true,
    delete_day_counter_request: true,
    delete_many_day_counters_request: true,
    list_day_counter_versions_request: true,
    get_day_counter_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.day_counters_events.created',
    updated: 'refdata.v1.day_counters_events.updated',
    deleted: 'refdata.v1.day_counters_events.deleted',
} as const;
