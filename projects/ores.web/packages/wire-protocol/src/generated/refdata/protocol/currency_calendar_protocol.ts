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

export interface GetCurrencyCalendarsRequest {
    offset: number;
    limit: number;
}

export interface GetCurrencyCalendarsResponse {
    currency_calendars: CurrencyCalendar[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface GetCurrencyCalendarsByCurrencyRequest {
    currency_iso_code: string;
    offset: number;
    limit: number;
}

export interface GetCurrencyCalendarsByCurrencyResponse {
    currency_calendars: CurrencyCalendarView[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCurrencyCalendarRequest {
    currency_calendars: CurrencyCalendar[];
}

export interface SaveCurrencyCalendarResponse {
    success: boolean;
    message: string;
}

export interface DeleteCurrencyCalendarRequest {
    currency_iso_codes: string[];
    calendar_codes: string[];
}

export interface DeleteCurrencyCalendarResponse {
    success: boolean;
    message: string;
}

export interface CountCurrencyCalendarsByCurrencyRequest {
    currency_iso_code: string;
}

export interface CountCurrencyCalendarsByCurrencyResponse {
    total_available_count: number;
}

export interface CountCurrencyCalendarsByCalendarRequest {
    calendar_code: string;
}

export interface CountCurrencyCalendarsByCalendarResponse {
    total_available_count: number;
}

export interface CurrencyCalendarView {
    currency_calendar: CurrencyCalendar;
}

export const subjects = {
    get_currency_calendars_request: "refdata.v1.currency_calendars.list",
    get_currency_calendars_by_currency_request: "refdata.v1.currency_calendars.list_by_currency_iso_code",
    save_currency_calendar_request: "refdata.v1.currency_calendars.save",
    delete_currency_calendar_request: "refdata.v1.currency_calendars.delete",
    count_currency_calendars_by_currency_request: "refdata.v1.currency_calendars.count_by_currency_iso_code",
    count_currency_calendars_by_calendar_request: "refdata.v1.currency_calendars.count_by_calendar_code",
} as const;
