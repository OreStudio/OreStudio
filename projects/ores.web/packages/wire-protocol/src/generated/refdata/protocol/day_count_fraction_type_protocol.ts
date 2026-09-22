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
import type { DayCountFractionType } from '../domain/day_count_fraction_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DayCountFractionTypeKey {
    code: string;
}

export interface DayCountFractionTypeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface DayCountFractionTypeChange {
    write: DayCountFractionTypeWrite;
    precondition: Precondition;
}

export interface DayCountFractionTypeRemoval {
    key: DayCountFractionTypeKey;
    precondition: Precondition;
}

export interface DayCountFractionTypeLookup {
    key: DayCountFractionTypeKey;
    day_count_fraction_type: DayCountFractionType | null;
}

export interface DayCountFractionTypeEvent {
    event_id: string;
    key: DayCountFractionTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DayCountFractionTypeVersionKey {
    day_count_fraction_type: DayCountFractionTypeKey;
    version: number;
}

export interface DayCountFractionTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDayCountFractionTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListDayCountFractionTypesResponse {
    result: Result;
    types: DayCountFractionType[];
    total: number;
}

export interface GetDayCountFractionTypeRequest {
    key: DayCountFractionTypeKey;
}

export interface GetDayCountFractionTypeResponse {
    result: Result;
    day_count_fraction_type: DayCountFractionType | null;
}

export interface GetManyDayCountFractionTypesRequest {
    keys: DayCountFractionTypeKey[];
}

export interface GetManyDayCountFractionTypesResponse {
    result: Result;
    entries: DayCountFractionTypeLookup[];
}

export interface PutDayCountFractionTypeRequest {
    change: DayCountFractionTypeChange;
    intent: ChangeIntent;
}

export interface PutDayCountFractionTypeResponse {
    result: Result;
    day_count_fraction_type: DayCountFractionType;
}

export interface PutManyDayCountFractionTypesRequest {
    changes: DayCountFractionTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyDayCountFractionTypesResponse {
    result: Result;
    types: DayCountFractionType[];
}

export interface DeleteDayCountFractionTypeRequest {
    removal: DayCountFractionTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteDayCountFractionTypeResponse {
    result: Result;
}

export interface DeleteManyDayCountFractionTypesRequest {
    removals: DayCountFractionTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDayCountFractionTypesResponse {
    result: Result;
}

export interface ListDayCountFractionTypeVersionsRequest {
    key: DayCountFractionTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DayCountFractionTypeVersionsFilter | null;
}

export interface ListDayCountFractionTypeVersionsResponse {
    result: Result;
    versions: DayCountFractionType[];
    total: number;
}

export interface GetDayCountFractionTypeVersionRequest {
    key: DayCountFractionTypeVersionKey;
}

export interface GetDayCountFractionTypeVersionResponse {
    result: Result;
    version: DayCountFractionType;
}

export const subjects = {
    list_day_count_fraction_types_request: "refdata.v1.day_count_fraction_types.list",
    get_day_count_fraction_type_request: "refdata.v1.day_count_fraction_types.get",
    get_many_day_count_fraction_types_request: "refdata.v1.day_count_fraction_types.get_many",
    put_day_count_fraction_type_request: "refdata.v1.day_count_fraction_types.put",
    put_many_day_count_fraction_types_request: "refdata.v1.day_count_fraction_types.put_many",
    delete_day_count_fraction_type_request: "refdata.v1.day_count_fraction_types.delete",
    delete_many_day_count_fraction_types_request: "refdata.v1.day_count_fraction_types.delete_many",
    list_day_count_fraction_type_versions_request: "refdata.v1.day_count_fraction_types_versions.list",
    get_day_count_fraction_type_version_request: "refdata.v1.day_count_fraction_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_day_count_fraction_types_request: true,
    get_day_count_fraction_type_request: true,
    get_many_day_count_fraction_types_request: true,
    put_day_count_fraction_type_request: true,
    put_many_day_count_fraction_types_request: true,
    delete_day_count_fraction_type_request: true,
    delete_many_day_count_fraction_types_request: true,
    list_day_count_fraction_type_versions_request: true,
    get_day_count_fraction_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.day_count_fraction_types_events.created",
    updated: "refdata.v1.day_count_fraction_types_events.updated",
    deleted: "refdata.v1.day_count_fraction_types_events.deleted",
} as const;
