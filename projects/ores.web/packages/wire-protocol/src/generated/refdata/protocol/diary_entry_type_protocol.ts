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
import type { DiaryEntryType } from '../domain/diary_entry_type.js';

export interface GetDiaryEntryTypesRequest {
    offset: number;
    limit: number;
}

export interface GetDiaryEntryTypesResponse {
    entry_types: DiaryEntryType[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveDiaryEntryTypeRequest {
    data: DiaryEntryType;
}

export interface SaveDiaryEntryTypeResponse {
    success: boolean;
    message: string;
}

export interface DeleteDiaryEntryTypeRequest {
    codes: string[];
}

export interface DeleteDiaryEntryTypeResponse {
    success: boolean;
    message: string;
}

export interface GetDiaryEntryTypeHistoryRequest {
    code: string;
}

export interface GetDiaryEntryTypeHistoryResponse {
    history: DiaryEntryType[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_diary_entry_types_request: "refdata.v1.diary_entry_types.list",
    save_diary_entry_type_request: "refdata.v1.diary_entry_types.save",
    delete_diary_entry_type_request: "refdata.v1.diary_entry_types.delete",
    get_diary_entry_type_history_request: "refdata.v1.diary_entry_types.history",
} as const;
