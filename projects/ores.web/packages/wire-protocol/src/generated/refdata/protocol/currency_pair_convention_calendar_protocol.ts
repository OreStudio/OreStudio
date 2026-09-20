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

export interface GetCurrencyPairConventionCalendarsRequest {
    offset: number;
    limit: number;
}

export interface GetCurrencyPairConventionCalendarsResponse {
    currency_pair_convention_calendars: CurrencyPairConventionCalendar[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface GetCurrencyPairConventionCalendarsByPairRequest {
    pair_code: string;
    offset: number;
    limit: number;
}

export interface GetCurrencyPairConventionCalendarsByPairResponse {
    currency_pair_convention_calendars: CurrencyPairConventionCalendarView[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCurrencyPairConventionCalendarRequest {
    currency_pair_convention_calendars: CurrencyPairConventionCalendar[];
}

export interface SaveCurrencyPairConventionCalendarResponse {
    success: boolean;
    message: string;
}

export interface DeleteCurrencyPairConventionCalendarRequest {
    pair_codes: string[];
    calendar_codes: string[];
}

export interface DeleteCurrencyPairConventionCalendarResponse {
    success: boolean;
    message: string;
}

export interface CountCurrencyPairConventionCalendarsByPairRequest {
    pair_code: string;
}

export interface CountCurrencyPairConventionCalendarsByPairResponse {
    total_available_count: number;
}

export interface CountCurrencyPairConventionCalendarsByCalendarRequest {
    calendar_code: string;
}

export interface CountCurrencyPairConventionCalendarsByCalendarResponse {
    total_available_count: number;
}

export interface CurrencyPairConventionCalendarView {
    currency_pair_convention_calendar: CurrencyPairConventionCalendar;
}

export const subjects = {
    get_currency_pair_convention_calendars_request: "refdata.v1.currency_pair_convention_calendars.list",
    get_currency_pair_convention_calendars_by_pair_request: "refdata.v1.currency_pair_convention_calendars.list_by_pair_code",
    save_currency_pair_convention_calendar_request: "refdata.v1.currency_pair_convention_calendars.save",
    delete_currency_pair_convention_calendar_request: "refdata.v1.currency_pair_convention_calendars.delete",
    count_currency_pair_convention_calendars_by_pair_request: "refdata.v1.currency_pair_convention_calendars.count_by_pair_code",
    count_currency_pair_convention_calendars_by_calendar_request: "refdata.v1.currency_pair_convention_calendars.count_by_calendar_code",
} as const;
