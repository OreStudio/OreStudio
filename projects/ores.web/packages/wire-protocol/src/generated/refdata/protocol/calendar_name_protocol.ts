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
import type { CalendarName } from '../domain/calendar_name.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CalendarNameKey {
    code: string;
}

export interface CalendarNameWrite {
    code: string;
    description: string;
}

export interface CalendarNameChange {
    write: CalendarNameWrite;
    precondition: Precondition;
}

export interface CalendarNameRemoval {
    key: CalendarNameKey;
    precondition: Precondition;
}

export interface CalendarNameLookup {
    key: CalendarNameKey;
    calendar_name: CalendarName | null;
}

export interface CalendarNameEvent {
    event_id: string;
    key: CalendarNameKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CalendarNameVersionKey {
    calendar_name: CalendarNameKey;
    version: number;
}

export interface CalendarNameVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCalendarNamesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCalendarNamesResponse {
    result: Result;
    calendar_names: CalendarName[];
    total: number;
}

export interface GetCalendarNameRequest {
    key: CalendarNameKey;
}

export interface GetCalendarNameResponse {
    result: Result;
    calendar_name: CalendarName | null;
}

export interface GetManyCalendarNamesRequest {
    keys: CalendarNameKey[];
}

export interface GetManyCalendarNamesResponse {
    result: Result;
    entries: CalendarNameLookup[];
}

export interface PutCalendarNameRequest {
    change: CalendarNameChange;
    intent: ChangeIntent;
}

export interface PutCalendarNameResponse {
    result: Result;
    calendar_name: CalendarName | null;
}

export interface PutManyCalendarNamesRequest {
    changes: CalendarNameChange[];
    intent: ChangeIntent;
}

export interface PutManyCalendarNamesResponse {
    result: Result;
    calendar_names: CalendarName[];
}

export interface DeleteCalendarNameRequest {
    removal: CalendarNameRemoval;
    intent: ChangeIntent;
}

export interface DeleteCalendarNameResponse {
    result: Result;
}

export interface DeleteManyCalendarNamesRequest {
    removals: CalendarNameRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCalendarNamesResponse {
    result: Result;
}

export interface ListCalendarNameVersionsRequest {
    key: CalendarNameKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CalendarNameVersionsFilter | null;
}

export interface ListCalendarNameVersionsResponse {
    result: Result;
    versions: CalendarName[];
    total: number;
}

export interface GetCalendarNameVersionRequest {
    key: CalendarNameVersionKey;
}

export interface GetCalendarNameVersionResponse {
    result: Result;
    version: CalendarName | null;
}

export const subjects = {
    list_calendar_names_request: 'refdata.v1.calendar_names.list',
    get_calendar_name_request: 'refdata.v1.calendar_names.get',
    get_many_calendar_names_request: 'refdata.v1.calendar_names.get_many',
    put_calendar_name_request: 'refdata.v1.calendar_names.put',
    put_many_calendar_names_request: 'refdata.v1.calendar_names.put_many',
    delete_calendar_name_request: 'refdata.v1.calendar_names.delete',
    delete_many_calendar_names_request: 'refdata.v1.calendar_names.delete_many',
    list_calendar_name_versions_request: 'refdata.v1.calendar_names_versions.list',
    get_calendar_name_version_request: 'refdata.v1.calendar_names_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_calendar_names_request: true,
    get_calendar_name_request: true,
    get_many_calendar_names_request: true,
    put_calendar_name_request: true,
    put_many_calendar_names_request: true,
    delete_calendar_name_request: true,
    delete_many_calendar_names_request: true,
    list_calendar_name_versions_request: true,
    get_calendar_name_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.calendar_names_events.created',
    updated: 'refdata.v1.calendar_names_events.updated',
    deleted: 'refdata.v1.calendar_names_events.deleted',
} as const;
