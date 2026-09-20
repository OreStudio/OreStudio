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
import type { CalendarEvent } from '../domain/calendar_event.js';

export interface GetCalendarEventsRequest {
    offset: number;
    limit: number;
}

export interface GetCalendarEventsResponse {
    calendar_events: CalendarEvent[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCalendarEventRequest {
    data: CalendarEvent;
}

export interface SaveCalendarEventResponse {
    success: boolean;
    message: string;
}

export interface DeleteCalendarEventRequest {
    ids: string[];
}

export interface DeleteCalendarEventResponse {
    success: boolean;
    message: string;
}

export interface GetCalendarEventHistoryRequest {
    id: string;
}

export interface GetCalendarEventHistoryResponse {
    history: CalendarEvent[];
    success: boolean;
    message: string;
}

export interface GetCalendarEventsByCalendarCodeRequest {
    calendar_code: string;
    offset: number;
    limit: number;
}

export interface GetCalendarEventsByCalendarCodeResponse {
    calendar_events: CalendarEvent[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface GetCalendarEventsByDiaryEntryTypeRequest {
    diary_entry_type: string;
    offset: number;
    limit: number;
}

export interface GetCalendarEventsByDiaryEntryTypeResponse {
    calendar_events: CalendarEvent[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export const subjects = {
    get_calendar_events_request: "refdata.v1.calendar_events.list",
    save_calendar_event_request: "refdata.v1.calendar_events.save",
    delete_calendar_event_request: "refdata.v1.calendar_events.delete",
    get_calendar_event_history_request: "refdata.v1.calendar_events.history",
    get_calendar_events_by_calendar_code_request: "refdata.v1.calendar_events.list_by_calendar_code",
    get_calendar_events_by_diary_entry_type_request: "refdata.v1.calendar_events.list_by_diary_entry_type",
} as const;
