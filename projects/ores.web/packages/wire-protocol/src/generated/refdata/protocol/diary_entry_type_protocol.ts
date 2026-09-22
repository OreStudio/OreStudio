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
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DiaryEntryTypeKey {
    code: string;
}

export interface DiaryEntryTypeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface DiaryEntryTypeChange {
    write: DiaryEntryTypeWrite;
    precondition: Precondition;
}

export interface DiaryEntryTypeRemoval {
    key: DiaryEntryTypeKey;
    precondition: Precondition;
}

export interface DiaryEntryTypeLookup {
    key: DiaryEntryTypeKey;
    diary_entry_type: DiaryEntryType | null;
}

export interface DiaryEntryTypeEvent {
    event_id: string;
    key: DiaryEntryTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DiaryEntryTypeVersionKey {
    diary_entry_type: DiaryEntryTypeKey;
    version: number;
}

export interface DiaryEntryTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDiaryEntryTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListDiaryEntryTypesResponse {
    result: Result;
    entry_types: DiaryEntryType[];
    total: number;
}

export interface GetDiaryEntryTypeRequest {
    key: DiaryEntryTypeKey;
}

export interface GetDiaryEntryTypeResponse {
    result: Result;
    diary_entry_type: DiaryEntryType | null;
}

export interface GetManyDiaryEntryTypesRequest {
    keys: DiaryEntryTypeKey[];
}

export interface GetManyDiaryEntryTypesResponse {
    result: Result;
    entries: DiaryEntryTypeLookup[];
}

export interface PutDiaryEntryTypeRequest {
    change: DiaryEntryTypeChange;
    intent: ChangeIntent;
}

export interface PutDiaryEntryTypeResponse {
    result: Result;
    diary_entry_type: DiaryEntryType;
}

export interface PutManyDiaryEntryTypesRequest {
    changes: DiaryEntryTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyDiaryEntryTypesResponse {
    result: Result;
    entry_types: DiaryEntryType[];
}

export interface DeleteDiaryEntryTypeRequest {
    removal: DiaryEntryTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteDiaryEntryTypeResponse {
    result: Result;
}

export interface DeleteManyDiaryEntryTypesRequest {
    removals: DiaryEntryTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDiaryEntryTypesResponse {
    result: Result;
}

export interface ListDiaryEntryTypeVersionsRequest {
    key: DiaryEntryTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DiaryEntryTypeVersionsFilter | null;
}

export interface ListDiaryEntryTypeVersionsResponse {
    result: Result;
    versions: DiaryEntryType[];
    total: number;
}

export interface GetDiaryEntryTypeVersionRequest {
    key: DiaryEntryTypeVersionKey;
}

export interface GetDiaryEntryTypeVersionResponse {
    result: Result;
    version: DiaryEntryType;
}

export const subjects = {
    list_diary_entry_types_request: "refdata.v1.diary_entry_types.list",
    get_diary_entry_type_request: "refdata.v1.diary_entry_types.get",
    get_many_diary_entry_types_request: "refdata.v1.diary_entry_types.get_many",
    put_diary_entry_type_request: "refdata.v1.diary_entry_types.put",
    put_many_diary_entry_types_request: "refdata.v1.diary_entry_types.put_many",
    delete_diary_entry_type_request: "refdata.v1.diary_entry_types.delete",
    delete_many_diary_entry_types_request: "refdata.v1.diary_entry_types.delete_many",
    list_diary_entry_type_versions_request: "refdata.v1.diary_entry_types_versions.list",
    get_diary_entry_type_version_request: "refdata.v1.diary_entry_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_diary_entry_types_request: true,
    get_diary_entry_type_request: true,
    get_many_diary_entry_types_request: true,
    put_diary_entry_type_request: true,
    put_many_diary_entry_types_request: true,
    delete_diary_entry_type_request: true,
    delete_many_diary_entry_types_request: true,
    list_diary_entry_type_versions_request: true,
    get_diary_entry_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.diary_entry_types_events.created",
    updated: "refdata.v1.diary_entry_types_events.updated",
    deleted: "refdata.v1.diary_entry_types_events.deleted",
} as const;
