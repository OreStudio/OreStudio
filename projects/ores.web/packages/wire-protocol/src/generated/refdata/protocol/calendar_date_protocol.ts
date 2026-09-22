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
import type { CalendarDate } from '../domain/calendar_date.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CalendarDateKey {
    calendar_code: string;
    date: string;
}

export interface CalendarDateLookup {
    key: CalendarDateKey;
    calendar_date: CalendarDate | null;
}

export interface CalendarDatesFilter {
    calendar_code: string | null;
}

export interface CalendarDateEvent {
    event_id: string;
    key: CalendarDateKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListCalendarDatesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CalendarDatesFilter | null;
}

export interface ListCalendarDatesResponse {
    result: Result;
    calendar_dates: CalendarDate[];
    total: number;
}

export interface GetCalendarDateRequest {
    key: CalendarDateKey;
}

export interface GetCalendarDateResponse {
    result: Result;
    calendar_date: CalendarDate | null;
}

export interface GetManyCalendarDatesRequest {
    keys: CalendarDateKey[];
}

export interface GetManyCalendarDatesResponse {
    result: Result;
    entries: CalendarDateLookup[];
}

export interface ListByCalendarCodeCalendarDatesRequest {
    calendar_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CalendarDatesFilter | null;
}

export interface ListByCalendarCodeCalendarDatesResponse {
    result: Result;
    calendar_dates: CalendarDate[];
    total: number;
}

export const subjects = {
    list_calendar_dates_request: "refdata.v1.calendar_dates.list",
    get_calendar_date_request: "refdata.v1.calendar_dates.get",
    get_many_calendar_dates_request: "refdata.v1.calendar_dates.get_many",
    list_by_calendar_code_calendar_dates_request: "refdata.v1.calendar_dates.list_by_calendar_code",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_calendar_dates_request: true,
    get_calendar_date_request: true,
    get_many_calendar_dates_request: true,
    list_by_calendar_code_calendar_dates_request: true,
} as const;
