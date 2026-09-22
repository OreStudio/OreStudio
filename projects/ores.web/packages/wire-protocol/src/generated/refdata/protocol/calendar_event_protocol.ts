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
import type { CalendarEvent } from '../domain/calendar_event.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CalendarEventKey {
    id: string;
}

export interface CalendarEventWrite {
    id: string;
    calendar_code: string;
    event_date: string;
    diary_entry_type: string;
    name: string;
    description: string | null;
    source: string | null;
}

export interface CalendarEventChange {
    write: CalendarEventWrite;
    precondition: Precondition;
}

export interface CalendarEventRemoval {
    key: CalendarEventKey;
    precondition: Precondition;
}

export interface CalendarEventLookup {
    key: CalendarEventKey;
    calendar_event: CalendarEvent | null;
}

export interface CalendarEventsFilter {
    calendar_code: string | null;
    diary_entry_type: string | null;
}

export interface CalendarEventEvent {
    event_id: string;
    key: CalendarEventKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CalendarEventVersionKey {
    calendar_event: CalendarEventKey;
    version: number;
}

export interface CalendarEventVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCalendarEventsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CalendarEventsFilter | null;
}

export interface ListCalendarEventsResponse {
    result: Result;
    calendar_events: CalendarEvent[];
    total: number;
}

export interface GetCalendarEventRequest {
    key: CalendarEventKey;
}

export interface GetCalendarEventResponse {
    result: Result;
    calendar_event: CalendarEvent | null;
}

export interface GetManyCalendarEventsRequest {
    keys: CalendarEventKey[];
}

export interface GetManyCalendarEventsResponse {
    result: Result;
    entries: CalendarEventLookup[];
}

export interface PutCalendarEventRequest {
    change: CalendarEventChange;
    intent: ChangeIntent;
}

export interface PutCalendarEventResponse {
    result: Result;
    calendar_event: CalendarEvent;
}

export interface PutManyCalendarEventsRequest {
    changes: CalendarEventChange[];
    intent: ChangeIntent;
}

export interface PutManyCalendarEventsResponse {
    result: Result;
    calendar_events: CalendarEvent[];
}

export interface DeleteCalendarEventRequest {
    removal: CalendarEventRemoval;
    intent: ChangeIntent;
}

export interface DeleteCalendarEventResponse {
    result: Result;
}

export interface DeleteManyCalendarEventsRequest {
    removals: CalendarEventRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCalendarEventsResponse {
    result: Result;
}

export interface ListByCalendarCodeCalendarEventsRequest {
    calendar_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CalendarEventsFilter | null;
}

export interface ListByCalendarCodeCalendarEventsResponse {
    result: Result;
    calendar_events: CalendarEvent[];
    total: number;
}

export interface ListByDiaryEntryTypeCalendarEventsRequest {
    diary_entry_type: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CalendarEventsFilter | null;
}

export interface ListByDiaryEntryTypeCalendarEventsResponse {
    result: Result;
    calendar_events: CalendarEvent[];
    total: number;
}

export interface ListCalendarEventVersionsRequest {
    key: CalendarEventKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CalendarEventVersionsFilter | null;
}

export interface ListCalendarEventVersionsResponse {
    result: Result;
    versions: CalendarEvent[];
    total: number;
}

export interface GetCalendarEventVersionRequest {
    key: CalendarEventVersionKey;
}

export interface GetCalendarEventVersionResponse {
    result: Result;
    version: CalendarEvent;
}

export const subjects = {
    list_calendar_events_request: "refdata.v1.calendar_events.list",
    get_calendar_event_request: "refdata.v1.calendar_events.get",
    get_many_calendar_events_request: "refdata.v1.calendar_events.get_many",
    put_calendar_event_request: "refdata.v1.calendar_events.put",
    put_many_calendar_events_request: "refdata.v1.calendar_events.put_many",
    delete_calendar_event_request: "refdata.v1.calendar_events.delete",
    delete_many_calendar_events_request: "refdata.v1.calendar_events.delete_many",
    list_by_calendar_code_calendar_events_request: "refdata.v1.calendar_events.list_by_calendar_code",
    list_by_diary_entry_type_calendar_events_request: "refdata.v1.calendar_events.list_by_diary_entry_type",
    list_calendar_event_versions_request: "refdata.v1.calendar_events_versions.list",
    get_calendar_event_version_request: "refdata.v1.calendar_events_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_calendar_events_request: true,
    get_calendar_event_request: true,
    get_many_calendar_events_request: true,
    put_calendar_event_request: true,
    put_many_calendar_events_request: true,
    delete_calendar_event_request: true,
    delete_many_calendar_events_request: true,
    list_by_calendar_code_calendar_events_request: true,
    list_by_diary_entry_type_calendar_events_request: true,
    list_calendar_event_versions_request: true,
    get_calendar_event_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.calendar_events_events.created",
    updated: "refdata.v1.calendar_events_events.updated",
    deleted: "refdata.v1.calendar_events_events.deleted",
} as const;
