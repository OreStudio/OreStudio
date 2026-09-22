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
import type { CurrencyCalendar } from '../domain/currency_calendar.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CurrencyCalendarKey {
    currency_iso_code: string;
    calendar_code: string;
}

export interface CurrencyCalendarWrite {
    currency_iso_code: string;
    calendar_code: string;
}

export interface CurrencyCalendarChange {
    write: CurrencyCalendarWrite;
    precondition: Precondition;
}

export interface CurrencyCalendarRemoval {
    key: CurrencyCalendarKey;
    precondition: Precondition;
}

export interface CurrencyCalendarLookup {
    key: CurrencyCalendarKey;
    currency_calendar: CurrencyCalendar | null;
}

export interface CurrencyCalendarsFilter {
    currency_iso_code: string | null;
}

export interface CurrencyCalendarEvent {
    event_id: string;
    key: CurrencyCalendarKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListCurrencyCalendarsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurrencyCalendarsFilter | null;
}

export interface ListCurrencyCalendarsResponse {
    result: Result;
    currency_calendars: CurrencyCalendar[];
    total: number;
}

export interface GetCurrencyCalendarRequest {
    key: CurrencyCalendarKey;
}

export interface GetCurrencyCalendarResponse {
    result: Result;
    currency_calendar: CurrencyCalendar | null;
}

export interface GetManyCurrencyCalendarsRequest {
    keys: CurrencyCalendarKey[];
}

export interface GetManyCurrencyCalendarsResponse {
    result: Result;
    entries: CurrencyCalendarLookup[];
}

export interface PutCurrencyCalendarRequest {
    change: CurrencyCalendarChange;
    intent: ChangeIntent;
}

export interface PutCurrencyCalendarResponse {
    result: Result;
    currency_calendar: CurrencyCalendar;
}

export interface PutManyCurrencyCalendarsRequest {
    changes: CurrencyCalendarChange[];
    intent: ChangeIntent;
}

export interface PutManyCurrencyCalendarsResponse {
    result: Result;
    currency_calendars: CurrencyCalendar[];
}

export interface DeleteCurrencyCalendarRequest {
    removal: CurrencyCalendarRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurrencyCalendarResponse {
    result: Result;
}

export interface DeleteManyCurrencyCalendarsRequest {
    removals: CurrencyCalendarRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurrencyCalendarsResponse {
    result: Result;
}

export interface ListByCurrencyIsoCodeCurrencyCalendarsRequest {
    currency_iso_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CurrencyCalendarsFilter | null;
}

export interface ListByCurrencyIsoCodeCurrencyCalendarsResponse {
    result: Result;
    currency_calendars: CurrencyCalendar[];
    total: number;
}

export const subjects = {
    list_currency_calendars_request: "refdata.v1.currency_calendars.list",
    get_currency_calendar_request: "refdata.v1.currency_calendars.get",
    get_many_currency_calendars_request: "refdata.v1.currency_calendars.get_many",
    put_currency_calendar_request: "refdata.v1.currency_calendars.put",
    put_many_currency_calendars_request: "refdata.v1.currency_calendars.put_many",
    delete_currency_calendar_request: "refdata.v1.currency_calendars.delete",
    delete_many_currency_calendars_request: "refdata.v1.currency_calendars.delete_many",
    list_by_currency_iso_code_currency_calendars_request: "refdata.v1.currency_calendars.list_by_currency_iso_code",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_currency_calendars_request: true,
    get_currency_calendar_request: true,
    get_many_currency_calendars_request: true,
    put_currency_calendar_request: true,
    put_many_currency_calendars_request: true,
    delete_currency_calendar_request: true,
    delete_many_currency_calendars_request: true,
    list_by_currency_iso_code_currency_calendars_request: true,
} as const;
