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

export interface GetCalendarExceptionsRequest {
    offset: number;
    limit: number;
}

export interface GetCalendarExceptionsResponse {
    calendar_exceptions: CalendarException[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCalendarExceptionRequest {
    data: CalendarException;
}

export interface SaveCalendarExceptionResponse {
    success: boolean;
    message: string;
}

export interface DeleteCalendarExceptionRequest {
    ids: string[];
}

export interface DeleteCalendarExceptionResponse {
    success: boolean;
    message: string;
}

export interface GetCalendarExceptionHistoryRequest {
    id: string;
}

export interface GetCalendarExceptionHistoryResponse {
    history: CalendarException[];
    success: boolean;
    message: string;
}

export interface GetCalendarExceptionsByCalendarCodeRequest {
    calendar_code: string;
    offset: number;
    limit: number;
}

export interface GetCalendarExceptionsByCalendarCodeResponse {
    calendar_exceptions: CalendarException[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export const subjects = {
    get_calendar_exceptions_request: "refdata.v1.calendar_exceptions.list",
    save_calendar_exception_request: "refdata.v1.calendar_exceptions.save",
    delete_calendar_exception_request: "refdata.v1.calendar_exceptions.delete",
    get_calendar_exception_history_request: "refdata.v1.calendar_exceptions.history",
    get_calendar_exceptions_by_calendar_code_request: "refdata.v1.calendar_exceptions.list_by_calendar_code",
} as const;
