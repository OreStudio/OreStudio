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
import type { CalendarException } from '../domain/calendar_exception.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CalendarExceptionKey {
    id: string;
}

export interface CalendarExceptionWrite {
    id: string;
    calendar_code: string;
    exception_date: string;
    is_business_day: boolean;
    description: string | null;
}

export interface CalendarExceptionChange {
    write: CalendarExceptionWrite;
    precondition: Precondition;
}

export interface CalendarExceptionRemoval {
    key: CalendarExceptionKey;
    precondition: Precondition;
}

export interface CalendarExceptionLookup {
    key: CalendarExceptionKey;
    calendar_exception: CalendarException | null;
}

export interface CalendarExceptionsFilter {
    calendar_code: string | null;
}

export interface CalendarExceptionEvent {
    event_id: string;
    key: CalendarExceptionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CalendarExceptionVersionKey {
    calendar_exception: CalendarExceptionKey;
    version: number;
}

export interface CalendarExceptionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCalendarExceptionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CalendarExceptionsFilter | null;
}

export interface ListCalendarExceptionsResponse {
    result: Result;
    calendar_exceptions: CalendarException[];
    total: number;
}

export interface GetCalendarExceptionRequest {
    key: CalendarExceptionKey;
}

export interface GetCalendarExceptionResponse {
    result: Result;
    calendar_exception: CalendarException | null;
}

export interface GetManyCalendarExceptionsRequest {
    keys: CalendarExceptionKey[];
}

export interface GetManyCalendarExceptionsResponse {
    result: Result;
    entries: CalendarExceptionLookup[];
}

export interface PutCalendarExceptionRequest {
    change: CalendarExceptionChange;
    intent: ChangeIntent;
}

export interface PutCalendarExceptionResponse {
    result: Result;
    calendar_exception: CalendarException;
}

export interface PutManyCalendarExceptionsRequest {
    changes: CalendarExceptionChange[];
    intent: ChangeIntent;
}

export interface PutManyCalendarExceptionsResponse {
    result: Result;
    calendar_exceptions: CalendarException[];
}

export interface DeleteCalendarExceptionRequest {
    removal: CalendarExceptionRemoval;
    intent: ChangeIntent;
}

export interface DeleteCalendarExceptionResponse {
    result: Result;
}

export interface DeleteManyCalendarExceptionsRequest {
    removals: CalendarExceptionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCalendarExceptionsResponse {
    result: Result;
}

export interface ListByCalendarCodeCalendarExceptionsRequest {
    calendar_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CalendarExceptionsFilter | null;
}

export interface ListByCalendarCodeCalendarExceptionsResponse {
    result: Result;
    calendar_exceptions: CalendarException[];
    total: number;
}

export interface ListCalendarExceptionVersionsRequest {
    key: CalendarExceptionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CalendarExceptionVersionsFilter | null;
}

export interface ListCalendarExceptionVersionsResponse {
    result: Result;
    versions: CalendarException[];
    total: number;
}

export interface GetCalendarExceptionVersionRequest {
    key: CalendarExceptionVersionKey;
}

export interface GetCalendarExceptionVersionResponse {
    result: Result;
    version: CalendarException;
}

export const subjects = {
    list_calendar_exceptions_request: "refdata.v1.calendar_exceptions.list",
    get_calendar_exception_request: "refdata.v1.calendar_exceptions.get",
    get_many_calendar_exceptions_request: "refdata.v1.calendar_exceptions.get_many",
    put_calendar_exception_request: "refdata.v1.calendar_exceptions.put",
    put_many_calendar_exceptions_request: "refdata.v1.calendar_exceptions.put_many",
    delete_calendar_exception_request: "refdata.v1.calendar_exceptions.delete",
    delete_many_calendar_exceptions_request: "refdata.v1.calendar_exceptions.delete_many",
    list_by_calendar_code_calendar_exceptions_request: "refdata.v1.calendar_exceptions.list_by_calendar_code",
    list_calendar_exception_versions_request: "refdata.v1.calendar_exceptions_versions.list",
    get_calendar_exception_version_request: "refdata.v1.calendar_exceptions_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_calendar_exceptions_request: true,
    get_calendar_exception_request: true,
    get_many_calendar_exceptions_request: true,
    put_calendar_exception_request: true,
    put_many_calendar_exceptions_request: true,
    delete_calendar_exception_request: true,
    delete_many_calendar_exceptions_request: true,
    list_by_calendar_code_calendar_exceptions_request: true,
    list_calendar_exception_versions_request: true,
    get_calendar_exception_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.calendar_exceptions_events.created",
    updated: "refdata.v1.calendar_exceptions_events.updated",
    deleted: "refdata.v1.calendar_exceptions_events.deleted",
} as const;
