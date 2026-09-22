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
import type { CalendarType } from '../domain/calendar_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CalendarTypeKey {
    code: string;
}

export interface CalendarTypeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface CalendarTypeChange {
    write: CalendarTypeWrite;
    precondition: Precondition;
}

export interface CalendarTypeRemoval {
    key: CalendarTypeKey;
    precondition: Precondition;
}

export interface CalendarTypeLookup {
    key: CalendarTypeKey;
    calendar_type: CalendarType | null;
}

export interface CalendarTypeEvent {
    event_id: string;
    key: CalendarTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CalendarTypeVersionKey {
    calendar_type: CalendarTypeKey;
    version: number;
}

export interface CalendarTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCalendarTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCalendarTypesResponse {
    result: Result;
    types: CalendarType[];
    total: number;
}

export interface GetCalendarTypeRequest {
    key: CalendarTypeKey;
}

export interface GetCalendarTypeResponse {
    result: Result;
    calendar_type: CalendarType | null;
}

export interface GetManyCalendarTypesRequest {
    keys: CalendarTypeKey[];
}

export interface GetManyCalendarTypesResponse {
    result: Result;
    entries: CalendarTypeLookup[];
}

export interface PutCalendarTypeRequest {
    change: CalendarTypeChange;
    intent: ChangeIntent;
}

export interface PutCalendarTypeResponse {
    result: Result;
    calendar_type: CalendarType;
}

export interface PutManyCalendarTypesRequest {
    changes: CalendarTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyCalendarTypesResponse {
    result: Result;
    types: CalendarType[];
}

export interface DeleteCalendarTypeRequest {
    removal: CalendarTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteCalendarTypeResponse {
    result: Result;
}

export interface DeleteManyCalendarTypesRequest {
    removals: CalendarTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCalendarTypesResponse {
    result: Result;
}

export interface ListCalendarTypeVersionsRequest {
    key: CalendarTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CalendarTypeVersionsFilter | null;
}

export interface ListCalendarTypeVersionsResponse {
    result: Result;
    versions: CalendarType[];
    total: number;
}

export interface GetCalendarTypeVersionRequest {
    key: CalendarTypeVersionKey;
}

export interface GetCalendarTypeVersionResponse {
    result: Result;
    version: CalendarType;
}

export const subjects = {
    list_calendar_types_request: "refdata.v1.calendar_types.list",
    get_calendar_type_request: "refdata.v1.calendar_types.get",
    get_many_calendar_types_request: "refdata.v1.calendar_types.get_many",
    put_calendar_type_request: "refdata.v1.calendar_types.put",
    put_many_calendar_types_request: "refdata.v1.calendar_types.put_many",
    delete_calendar_type_request: "refdata.v1.calendar_types.delete",
    delete_many_calendar_types_request: "refdata.v1.calendar_types.delete_many",
    list_calendar_type_versions_request: "refdata.v1.calendar_types_versions.list",
    get_calendar_type_version_request: "refdata.v1.calendar_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_calendar_types_request: true,
    get_calendar_type_request: true,
    get_many_calendar_types_request: true,
    put_calendar_type_request: true,
    put_many_calendar_types_request: true,
    delete_calendar_type_request: true,
    delete_many_calendar_types_request: true,
    list_calendar_type_versions_request: true,
    get_calendar_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.calendar_types_events.created",
    updated: "refdata.v1.calendar_types_events.updated",
    deleted: "refdata.v1.calendar_types_events.deleted",
} as const;
