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
import type { InstrumentSchedule } from '../domain/instrument_schedule.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InstrumentScheduleKey {
    trade_id: string;
    owner_role: string;
    owner_number: number;
    schedule_role: string;
    sequence_number: number;
}

export interface InstrumentScheduleWrite {
    trade_id: string;
    owner_role: string;
    owner_number: number;
    schedule_role: string;
    sequence_number: number;
    schedule_kind: string;
    start_date: string | null;
    end_date: string | null;
    adjust_end_date_to_previous_month_end: string | null;
    tenor: string | null;
    calendar: string | null;
    convention: string | null;
    term_convention: string | null;
    rule: string | null;
    end_of_month: string | null;
    end_of_month_convention: string | null;
    first_date: string | null;
    last_date: string | null;
    remove_first_date: boolean | null;
    remove_last_date: boolean | null;
    include_duplicate_dates: string | null;
}

export interface InstrumentScheduleChange {
    write: InstrumentScheduleWrite;
    precondition: Precondition;
}

export interface InstrumentScheduleRemoval {
    key: InstrumentScheduleKey;
    precondition: Precondition;
}

export interface InstrumentScheduleLookup {
    key: InstrumentScheduleKey;
    instrument_schedule: InstrumentSchedule | null;
}

export interface InstrumentScheduleEvent {
    event_id: string;
    key: InstrumentScheduleKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InstrumentScheduleVersionKey {
    instrument_schedule: InstrumentScheduleKey;
    version: number;
}

export interface InstrumentScheduleVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInstrumentSchedulesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListInstrumentSchedulesResponse {
    result: Result;
    instrument_schedules: InstrumentSchedule[];
    total: number;
}

export interface GetInstrumentScheduleRequest {
    key: InstrumentScheduleKey;
}

export interface GetInstrumentScheduleResponse {
    result: Result;
    instrument_schedule: InstrumentSchedule | null;
}

export interface GetManyInstrumentSchedulesRequest {
    keys: InstrumentScheduleKey[];
}

export interface GetManyInstrumentSchedulesResponse {
    result: Result;
    entries: InstrumentScheduleLookup[];
}

export interface PutInstrumentScheduleRequest {
    change: InstrumentScheduleChange;
    intent: ChangeIntent;
}

export interface PutInstrumentScheduleResponse {
    result: Result;
    instrument_schedule: InstrumentSchedule | null;
}

export interface PutManyInstrumentSchedulesRequest {
    changes: InstrumentScheduleChange[];
    intent: ChangeIntent;
}

export interface PutManyInstrumentSchedulesResponse {
    result: Result;
    instrument_schedules: InstrumentSchedule[];
}

export interface DeleteInstrumentScheduleRequest {
    removal: InstrumentScheduleRemoval;
    intent: ChangeIntent;
}

export interface DeleteInstrumentScheduleResponse {
    result: Result;
}

export interface DeleteManyInstrumentSchedulesRequest {
    removals: InstrumentScheduleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInstrumentSchedulesResponse {
    result: Result;
}

export interface ListInstrumentScheduleVersionsRequest {
    key: InstrumentScheduleKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentScheduleVersionsFilter | null;
}

export interface ListInstrumentScheduleVersionsResponse {
    result: Result;
    versions: InstrumentSchedule[];
    total: number;
}

export interface GetInstrumentScheduleVersionRequest {
    key: InstrumentScheduleVersionKey;
}

export interface GetInstrumentScheduleVersionResponse {
    result: Result;
    version: InstrumentSchedule | null;
}

export const subjects = {
    list_instrument_schedules_request: 'trading.v1.instrument_schedules.list',
    get_instrument_schedule_request: 'trading.v1.instrument_schedules.get',
    get_many_instrument_schedules_request: 'trading.v1.instrument_schedules.get_many',
    put_instrument_schedule_request: 'trading.v1.instrument_schedules.put',
    put_many_instrument_schedules_request: 'trading.v1.instrument_schedules.put_many',
    delete_instrument_schedule_request: 'trading.v1.instrument_schedules.delete',
    delete_many_instrument_schedules_request: 'trading.v1.instrument_schedules.delete_many',
    list_instrument_schedule_versions_request: 'trading.v1.instrument_schedules_versions.list',
    get_instrument_schedule_version_request: 'trading.v1.instrument_schedules_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_instrument_schedules_request: true,
    get_instrument_schedule_request: true,
    get_many_instrument_schedules_request: true,
    put_instrument_schedule_request: true,
    put_many_instrument_schedules_request: true,
    delete_instrument_schedule_request: true,
    delete_many_instrument_schedules_request: true,
    list_instrument_schedule_versions_request: true,
    get_instrument_schedule_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.instrument_schedules_events.created',
    updated: 'trading.v1.instrument_schedules_events.updated',
    deleted: 'trading.v1.instrument_schedules_events.deleted',
} as const;
