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
import type { TenorSchedule } from '../domain/tenor_schedule.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface TenorScheduleKey {
    code: string;
}

export interface TenorScheduleWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
    schedule_source: string;
    calendar_code: string | null;
    diary_entry_type: string | null;
}

export interface TenorScheduleChange {
    write: TenorScheduleWrite;
    precondition: Precondition;
}

export interface TenorScheduleRemoval {
    key: TenorScheduleKey;
    precondition: Precondition;
}

export interface TenorScheduleLookup {
    key: TenorScheduleKey;
    tenor_schedule: TenorSchedule | null;
}

export interface TenorSchedulesFilter {
    calendar_code: string | null | null;
    diary_entry_type: string | null | null;
}

export interface TenorScheduleEvent {
    event_id: string;
    key: TenorScheduleKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TenorScheduleVersionKey {
    tenor_schedule: TenorScheduleKey;
    version: number;
}

export interface TenorScheduleVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTenorSchedulesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TenorSchedulesFilter | null;
}

export interface ListTenorSchedulesResponse {
    result: Result;
    schedules: TenorSchedule[];
    total: number;
}

export interface GetTenorScheduleRequest {
    key: TenorScheduleKey;
}

export interface GetTenorScheduleResponse {
    result: Result;
    tenor_schedule: TenorSchedule | null;
}

export interface GetManyTenorSchedulesRequest {
    keys: TenorScheduleKey[];
}

export interface GetManyTenorSchedulesResponse {
    result: Result;
    entries: TenorScheduleLookup[];
}

export interface PutTenorScheduleRequest {
    change: TenorScheduleChange;
    intent: ChangeIntent;
}

export interface PutTenorScheduleResponse {
    result: Result;
    tenor_schedule: TenorSchedule;
}

export interface PutManyTenorSchedulesRequest {
    changes: TenorScheduleChange[];
    intent: ChangeIntent;
}

export interface PutManyTenorSchedulesResponse {
    result: Result;
    schedules: TenorSchedule[];
}

export interface DeleteTenorScheduleRequest {
    removal: TenorScheduleRemoval;
    intent: ChangeIntent;
}

export interface DeleteTenorScheduleResponse {
    result: Result;
}

export interface DeleteManyTenorSchedulesRequest {
    removals: TenorScheduleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTenorSchedulesResponse {
    result: Result;
}

export interface ListByCalendarCodeTenorSchedulesRequest {
    calendar_code: string | null;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorSchedulesFilter | null;
}

export interface ListByCalendarCodeTenorSchedulesResponse {
    result: Result;
    schedules: TenorSchedule[];
    total: number;
}

export interface ListByDiaryEntryTypeTenorSchedulesRequest {
    diary_entry_type: string | null;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorSchedulesFilter | null;
}

export interface ListByDiaryEntryTypeTenorSchedulesResponse {
    result: Result;
    schedules: TenorSchedule[];
    total: number;
}

export interface ListTenorScheduleVersionsRequest {
    key: TenorScheduleKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorScheduleVersionsFilter | null;
}

export interface ListTenorScheduleVersionsResponse {
    result: Result;
    versions: TenorSchedule[];
    total: number;
}

export interface GetTenorScheduleVersionRequest {
    key: TenorScheduleVersionKey;
}

export interface GetTenorScheduleVersionResponse {
    result: Result;
    version: TenorSchedule;
}

export const subjects = {
    list_tenor_schedules_request: "refdata.v1.tenor_schedules.list",
    get_tenor_schedule_request: "refdata.v1.tenor_schedules.get",
    get_many_tenor_schedules_request: "refdata.v1.tenor_schedules.get_many",
    put_tenor_schedule_request: "refdata.v1.tenor_schedules.put",
    put_many_tenor_schedules_request: "refdata.v1.tenor_schedules.put_many",
    delete_tenor_schedule_request: "refdata.v1.tenor_schedules.delete",
    delete_many_tenor_schedules_request: "refdata.v1.tenor_schedules.delete_many",
    list_by_calendar_code_tenor_schedules_request: "refdata.v1.tenor_schedules.list_by_calendar_code",
    list_by_diary_entry_type_tenor_schedules_request: "refdata.v1.tenor_schedules.list_by_diary_entry_type",
    list_tenor_schedule_versions_request: "refdata.v1.tenor_schedules_versions.list",
    get_tenor_schedule_version_request: "refdata.v1.tenor_schedules_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_tenor_schedules_request: true,
    get_tenor_schedule_request: true,
    get_many_tenor_schedules_request: true,
    put_tenor_schedule_request: true,
    put_many_tenor_schedules_request: true,
    delete_tenor_schedule_request: true,
    delete_many_tenor_schedules_request: true,
    list_by_calendar_code_tenor_schedules_request: true,
    list_by_diary_entry_type_tenor_schedules_request: true,
    list_tenor_schedule_versions_request: true,
    get_tenor_schedule_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.tenor_schedules_events.created",
    updated: "refdata.v1.tenor_schedules_events.updated",
    deleted: "refdata.v1.tenor_schedules_events.deleted",
} as const;
