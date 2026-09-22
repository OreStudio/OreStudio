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
import type { Calendar } from '../domain/calendar.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CalendarKey {
    code: string;
}

export interface CalendarWrite {
    code: string;
    name: string;
    calendar_type: string;
    country_code: string;
    image_id: string | null;
    source: string;
    is_editable: boolean;
    base_calendar_code: string | null;
}

export interface CalendarChange {
    write: CalendarWrite;
    precondition: Precondition;
}

export interface CalendarRemoval {
    key: CalendarKey;
    precondition: Precondition;
}

export interface CalendarLookup {
    key: CalendarKey;
    calendar: Calendar | null;
}

export interface CalendarEvent {
    event_id: string;
    key: CalendarKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CalendarVersionKey {
    calendar: CalendarKey;
    version: number;
}

export interface CalendarVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCalendarsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCalendarsResponse {
    result: Result;
    calendars: Calendar[];
    total: number;
}

export interface GetCalendarRequest {
    key: CalendarKey;
}

export interface GetCalendarResponse {
    result: Result;
    calendar: Calendar | null;
}

export interface GetManyCalendarsRequest {
    keys: CalendarKey[];
}

export interface GetManyCalendarsResponse {
    result: Result;
    entries: CalendarLookup[];
}

export interface PutCalendarRequest {
    change: CalendarChange;
    intent: ChangeIntent;
}

export interface PutCalendarResponse {
    result: Result;
    calendar: Calendar;
}

export interface PutManyCalendarsRequest {
    changes: CalendarChange[];
    intent: ChangeIntent;
}

export interface PutManyCalendarsResponse {
    result: Result;
    calendars: Calendar[];
}

export interface DeleteCalendarRequest {
    removal: CalendarRemoval;
    intent: ChangeIntent;
}

export interface DeleteCalendarResponse {
    result: Result;
}

export interface DeleteManyCalendarsRequest {
    removals: CalendarRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCalendarsResponse {
    result: Result;
}

export interface ListCalendarVersionsRequest {
    key: CalendarKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CalendarVersionsFilter | null;
}

export interface ListCalendarVersionsResponse {
    result: Result;
    versions: Calendar[];
    total: number;
}

export interface GetCalendarVersionRequest {
    key: CalendarVersionKey;
}

export interface GetCalendarVersionResponse {
    result: Result;
    version: Calendar;
}

export const subjects = {
    list_calendars_request: "refdata.v1.calendars.list",
    get_calendar_request: "refdata.v1.calendars.get",
    get_many_calendars_request: "refdata.v1.calendars.get_many",
    put_calendar_request: "refdata.v1.calendars.put",
    put_many_calendars_request: "refdata.v1.calendars.put_many",
    delete_calendar_request: "refdata.v1.calendars.delete",
    delete_many_calendars_request: "refdata.v1.calendars.delete_many",
    list_calendar_versions_request: "refdata.v1.calendars_versions.list",
    get_calendar_version_request: "refdata.v1.calendars_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_calendars_request: true,
    get_calendar_request: true,
    get_many_calendars_request: true,
    put_calendar_request: true,
    put_many_calendars_request: true,
    delete_calendar_request: true,
    delete_many_calendars_request: true,
    list_calendar_versions_request: true,
    get_calendar_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.calendars_events.created",
    updated: "refdata.v1.calendars_events.updated",
    deleted: "refdata.v1.calendars_events.deleted",
} as const;
