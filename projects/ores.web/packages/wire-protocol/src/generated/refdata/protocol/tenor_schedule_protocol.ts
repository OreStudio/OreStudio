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
import type { TenorSchedule } from '../domain/tenor_schedule.js';

export interface GetTenorSchedulesRequest {
    offset: number;
    limit: number;
}

export interface GetTenorSchedulesResponse {
    schedules: TenorSchedule[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveTenorScheduleRequest {
    data: TenorSchedule;
}

export interface SaveTenorScheduleResponse {
    success: boolean;
    message: string;
}

export interface DeleteTenorScheduleRequest {
    codes: string[];
}

export interface DeleteTenorScheduleResponse {
    success: boolean;
    message: string;
}

export interface GetTenorScheduleHistoryRequest {
    code: string;
}

export interface GetTenorScheduleHistoryResponse {
    history: TenorSchedule[];
    success: boolean;
    message: string;
}

export interface GetTenorSchedulesByCalendarCodeRequest {
    calendar_code: string;
    offset: number;
    limit: number;
}

export interface GetTenorSchedulesByCalendarCodeResponse {
    schedules: TenorSchedule[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface GetTenorSchedulesByDiaryEntryTypeRequest {
    diary_entry_type: string;
    offset: number;
    limit: number;
}

export interface GetTenorSchedulesByDiaryEntryTypeResponse {
    schedules: TenorSchedule[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export const subjects = {
    get_tenor_schedules_request: "refdata.v1.tenor_schedules.list",
    save_tenor_schedule_request: "refdata.v1.tenor_schedules.save",
    delete_tenor_schedule_request: "refdata.v1.tenor_schedules.delete",
    get_tenor_schedule_history_request: "refdata.v1.tenor_schedules.history",
    get_tenor_schedules_by_calendar_code_request: "refdata.v1.tenor_schedules.list_by_calendar_code",
    get_tenor_schedules_by_diary_entry_type_request: "refdata.v1.tenor_schedules.list_by_diary_entry_type",
} as const;
