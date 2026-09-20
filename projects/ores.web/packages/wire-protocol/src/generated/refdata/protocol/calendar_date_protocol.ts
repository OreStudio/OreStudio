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

export interface GetCalendarDatesRequest {
    offset: number;
    limit: number;
}

export interface GetCalendarDatesResponse {
    calendar_dates: CalendarDate[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface GetCalendarDatesByCalendarRequest {
    calendar_code: string;
    offset: number;
    limit: number;
}

export interface GetCalendarDatesByCalendarResponse {
    calendar_dates: CalendarDateView[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface CountCalendarDatesByCalendarRequest {
    calendar_code: string;
}

export interface CountCalendarDatesByCalendarResponse {
    total_available_count: number;
}

export interface CountCalendarDatesByDateRequest {
    date: string;
}

export interface CountCalendarDatesByDateResponse {
    total_available_count: number;
}

export interface CalendarDateView {
    calendar_date: CalendarDate;
}

export const subjects = {
    get_calendar_dates_request: "refdata.v1.calendar_dates.list",
    get_calendar_dates_by_calendar_request: "refdata.v1.calendar_dates.list_by_calendar_code",
    count_calendar_dates_by_calendar_request: "refdata.v1.calendar_dates.count_by_calendar_code",
    count_calendar_dates_by_date_request: "refdata.v1.calendar_dates.count_by_date",
} as const;
