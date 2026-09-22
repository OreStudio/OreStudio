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
import type { CurrencyPairConventionCalendar } from '../domain/currency_pair_convention_calendar.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CurrencyPairConventionCalendarKey {
    pair_code: string;
    calendar_code: string;
}

export interface CurrencyPairConventionCalendarWrite {
    pair_code: string;
    calendar_code: string;
}

export interface CurrencyPairConventionCalendarChange {
    write: CurrencyPairConventionCalendarWrite;
    precondition: Precondition;
}

export interface CurrencyPairConventionCalendarRemoval {
    key: CurrencyPairConventionCalendarKey;
    precondition: Precondition;
}

export interface CurrencyPairConventionCalendarLookup {
    key: CurrencyPairConventionCalendarKey;
    currency_pair_convention_calendar: CurrencyPairConventionCalendar | null;
}

export interface CurrencyPairConventionCalendarsFilter {
    pair_code: string | null;
}

export interface CurrencyPairConventionCalendarEvent {
    event_id: string;
    key: CurrencyPairConventionCalendarKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListCurrencyPairConventionCalendarsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurrencyPairConventionCalendarsFilter | null;
}

export interface ListCurrencyPairConventionCalendarsResponse {
    result: Result;
    currency_pair_convention_calendars: CurrencyPairConventionCalendar[];
    total: number;
}

export interface GetCurrencyPairConventionCalendarRequest {
    key: CurrencyPairConventionCalendarKey;
}

export interface GetCurrencyPairConventionCalendarResponse {
    result: Result;
    currency_pair_convention_calendar: CurrencyPairConventionCalendar | null;
}

export interface GetManyCurrencyPairConventionCalendarsRequest {
    keys: CurrencyPairConventionCalendarKey[];
}

export interface GetManyCurrencyPairConventionCalendarsResponse {
    result: Result;
    entries: CurrencyPairConventionCalendarLookup[];
}

export interface PutCurrencyPairConventionCalendarRequest {
    change: CurrencyPairConventionCalendarChange;
    intent: ChangeIntent;
}

export interface PutCurrencyPairConventionCalendarResponse {
    result: Result;
    currency_pair_convention_calendar: CurrencyPairConventionCalendar;
}

export interface PutManyCurrencyPairConventionCalendarsRequest {
    changes: CurrencyPairConventionCalendarChange[];
    intent: ChangeIntent;
}

export interface PutManyCurrencyPairConventionCalendarsResponse {
    result: Result;
    currency_pair_convention_calendars: CurrencyPairConventionCalendar[];
}

export interface DeleteCurrencyPairConventionCalendarRequest {
    removal: CurrencyPairConventionCalendarRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurrencyPairConventionCalendarResponse {
    result: Result;
}

export interface DeleteManyCurrencyPairConventionCalendarsRequest {
    removals: CurrencyPairConventionCalendarRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurrencyPairConventionCalendarsResponse {
    result: Result;
}

export interface ListByPairCodeCurrencyPairConventionCalendarsRequest {
    pair_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CurrencyPairConventionCalendarsFilter | null;
}

export interface ListByPairCodeCurrencyPairConventionCalendarsResponse {
    result: Result;
    currency_pair_convention_calendars: CurrencyPairConventionCalendar[];
    total: number;
}

export const subjects = {
    list_currency_pair_convention_calendars_request: "refdata.v1.currency_pair_convention_calendars.list",
    get_currency_pair_convention_calendar_request: "refdata.v1.currency_pair_convention_calendars.get",
    get_many_currency_pair_convention_calendars_request: "refdata.v1.currency_pair_convention_calendars.get_many",
    put_currency_pair_convention_calendar_request: "refdata.v1.currency_pair_convention_calendars.put",
    put_many_currency_pair_convention_calendars_request: "refdata.v1.currency_pair_convention_calendars.put_many",
    delete_currency_pair_convention_calendar_request: "refdata.v1.currency_pair_convention_calendars.delete",
    delete_many_currency_pair_convention_calendars_request: "refdata.v1.currency_pair_convention_calendars.delete_many",
    list_by_pair_code_currency_pair_convention_calendars_request: "refdata.v1.currency_pair_convention_calendars.list_by_pair_code",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_currency_pair_convention_calendars_request: true,
    get_currency_pair_convention_calendar_request: true,
    get_many_currency_pair_convention_calendars_request: true,
    put_currency_pair_convention_calendar_request: true,
    put_many_currency_pair_convention_calendars_request: true,
    delete_currency_pair_convention_calendar_request: true,
    delete_many_currency_pair_convention_calendars_request: true,
    list_by_pair_code_currency_pair_convention_calendars_request: true,
} as const;
