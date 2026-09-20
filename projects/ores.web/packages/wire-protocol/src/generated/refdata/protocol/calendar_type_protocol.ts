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
import type { CalendarType } from '../domain/calendar_type.js';

export interface GetCalendarTypesRequest {
    offset: number;
    limit: number;
}

export interface GetCalendarTypesResponse {
    types: CalendarType[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCalendarTypeRequest {
    data: CalendarType;
}

export interface SaveCalendarTypeResponse {
    success: boolean;
    message: string;
}

export interface DeleteCalendarTypeRequest {
    codes: string[];
}

export interface DeleteCalendarTypeResponse {
    success: boolean;
    message: string;
}

export interface GetCalendarTypeHistoryRequest {
    code: string;
}

export interface GetCalendarTypeHistoryResponse {
    history: CalendarType[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_calendar_types_request: "refdata.v1.calendar_types.list",
    save_calendar_type_request: "refdata.v1.calendar_types.save",
    delete_calendar_type_request: "refdata.v1.calendar_types.delete",
    get_calendar_type_history_request: "refdata.v1.calendar_types.history",
} as const;
