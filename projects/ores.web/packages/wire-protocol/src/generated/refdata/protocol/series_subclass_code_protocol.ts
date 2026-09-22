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
import type { SeriesSubclassCode } from '../domain/series_subclass_code.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SeriesSubclassCodeKey {
    code: string;
}

export interface SeriesSubclassCodeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface SeriesSubclassCodeChange {
    write: SeriesSubclassCodeWrite;
    precondition: Precondition;
}

export interface SeriesSubclassCodeRemoval {
    key: SeriesSubclassCodeKey;
    precondition: Precondition;
}

export interface SeriesSubclassCodeLookup {
    key: SeriesSubclassCodeKey;
    series_subclass_code: SeriesSubclassCode | null;
}

export interface SeriesSubclassCodeEvent {
    event_id: string;
    key: SeriesSubclassCodeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SeriesSubclassCodeVersionKey {
    series_subclass_code: SeriesSubclassCodeKey;
    version: number;
}

export interface SeriesSubclassCodeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSeriesSubclassCodesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListSeriesSubclassCodesResponse {
    result: Result;
    series_subclasses: SeriesSubclassCode[];
    total: number;
}

export interface GetSeriesSubclassCodeRequest {
    key: SeriesSubclassCodeKey;
}

export interface GetSeriesSubclassCodeResponse {
    result: Result;
    series_subclass_code: SeriesSubclassCode | null;
}

export interface GetManySeriesSubclassCodesRequest {
    keys: SeriesSubclassCodeKey[];
}

export interface GetManySeriesSubclassCodesResponse {
    result: Result;
    entries: SeriesSubclassCodeLookup[];
}

export interface PutSeriesSubclassCodeRequest {
    change: SeriesSubclassCodeChange;
    intent: ChangeIntent;
}

export interface PutSeriesSubclassCodeResponse {
    result: Result;
    series_subclass_code: SeriesSubclassCode;
}

export interface PutManySeriesSubclassCodesRequest {
    changes: SeriesSubclassCodeChange[];
    intent: ChangeIntent;
}

export interface PutManySeriesSubclassCodesResponse {
    result: Result;
    series_subclasses: SeriesSubclassCode[];
}

export interface DeleteSeriesSubclassCodeRequest {
    removal: SeriesSubclassCodeRemoval;
    intent: ChangeIntent;
}

export interface DeleteSeriesSubclassCodeResponse {
    result: Result;
}

export interface DeleteManySeriesSubclassCodesRequest {
    removals: SeriesSubclassCodeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySeriesSubclassCodesResponse {
    result: Result;
}

export interface ListSeriesSubclassCodeVersionsRequest {
    key: SeriesSubclassCodeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SeriesSubclassCodeVersionsFilter | null;
}

export interface ListSeriesSubclassCodeVersionsResponse {
    result: Result;
    versions: SeriesSubclassCode[];
    total: number;
}

export interface GetSeriesSubclassCodeVersionRequest {
    key: SeriesSubclassCodeVersionKey;
}

export interface GetSeriesSubclassCodeVersionResponse {
    result: Result;
    version: SeriesSubclassCode;
}

export const subjects = {
    list_series_subclass_codes_request: "refdata.v1.series_subclass_codes.list",
    get_series_subclass_code_request: "refdata.v1.series_subclass_codes.get",
    get_many_series_subclass_codes_request: "refdata.v1.series_subclass_codes.get_many",
    put_series_subclass_code_request: "refdata.v1.series_subclass_codes.put",
    put_many_series_subclass_codes_request: "refdata.v1.series_subclass_codes.put_many",
    delete_series_subclass_code_request: "refdata.v1.series_subclass_codes.delete",
    delete_many_series_subclass_codes_request: "refdata.v1.series_subclass_codes.delete_many",
    list_series_subclass_code_versions_request: "refdata.v1.series_subclass_codes_versions.list",
    get_series_subclass_code_version_request: "refdata.v1.series_subclass_codes_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_series_subclass_codes_request: true,
    get_series_subclass_code_request: true,
    get_many_series_subclass_codes_request: true,
    put_series_subclass_code_request: true,
    put_many_series_subclass_codes_request: true,
    delete_series_subclass_code_request: true,
    delete_many_series_subclass_codes_request: true,
    list_series_subclass_code_versions_request: true,
    get_series_subclass_code_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.series_subclass_codes_events.created",
    updated: "refdata.v1.series_subclass_codes_events.updated",
    deleted: "refdata.v1.series_subclass_codes_events.deleted",
} as const;
