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
import type { SeriesIdentityShapeEntry } from '../domain/series_identity_shape_entry.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface SeriesIdentityShapeEntryKey {
    id: string;
}

export interface SeriesIdentityShapeEntryWrite {
    id: string;
    shape_code: string;
    field: string;
    requirement: string;
    description: string;
}

export interface SeriesIdentityShapeEntryChange {
    write: SeriesIdentityShapeEntryWrite;
    precondition: Precondition;
}

export interface SeriesIdentityShapeEntryRemoval {
    key: SeriesIdentityShapeEntryKey;
    precondition: Precondition;
}

export interface SeriesIdentityShapeEntryLookup {
    key: SeriesIdentityShapeEntryKey;
    series_identity_shape_entry: SeriesIdentityShapeEntry | null;
}

export interface SeriesIdentityShapeEntriesFilter {
    shape_code: string | null;
    id_one_of: string[] | null;
    shape_code_one_of: string[] | null;
}

export interface SeriesIdentityShapeEntryEvent {
    event_id: string;
    key: SeriesIdentityShapeEntryKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SeriesIdentityShapeEntryVersionKey {
    series_identity_shape_entry: SeriesIdentityShapeEntryKey;
    version: number;
}

export interface SeriesIdentityShapeEntryVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSeriesIdentityShapeEntriesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SeriesIdentityShapeEntriesFilter | null;
    as_of: string | null;
}

export interface ListSeriesIdentityShapeEntriesResponse {
    result: Result;
    entries: SeriesIdentityShapeEntry[];
    total: number;
}

export interface GetSeriesIdentityShapeEntryRequest {
    key: SeriesIdentityShapeEntryKey;
}

export interface GetSeriesIdentityShapeEntryResponse {
    result: Result;
    series_identity_shape_entry: SeriesIdentityShapeEntry | null;
}

export interface GetManySeriesIdentityShapeEntriesRequest {
    keys: SeriesIdentityShapeEntryKey[];
}

export interface GetManySeriesIdentityShapeEntriesResponse {
    result: Result;
    entries: SeriesIdentityShapeEntryLookup[];
}

export interface PutSeriesIdentityShapeEntryRequest {
    change: SeriesIdentityShapeEntryChange;
    intent: ChangeIntent;
}

export interface PutSeriesIdentityShapeEntryResponse {
    result: Result;
    series_identity_shape_entry: SeriesIdentityShapeEntry | null;
}

export interface PutManySeriesIdentityShapeEntriesRequest {
    changes: SeriesIdentityShapeEntryChange[];
    intent: ChangeIntent;
}

export interface PutManySeriesIdentityShapeEntriesResponse {
    result: Result;
    entries: SeriesIdentityShapeEntry[];
}

export interface DeleteSeriesIdentityShapeEntryRequest {
    removal: SeriesIdentityShapeEntryRemoval;
    intent: ChangeIntent;
}

export interface DeleteSeriesIdentityShapeEntryResponse {
    result: Result;
}

export interface DeleteManySeriesIdentityShapeEntriesRequest {
    removals: SeriesIdentityShapeEntryRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySeriesIdentityShapeEntriesResponse {
    result: Result;
}

export interface ListByShapeCodeSeriesIdentityShapeEntriesRequest {
    shape_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: SeriesIdentityShapeEntriesFilter | null;
}

export interface ListByShapeCodeSeriesIdentityShapeEntriesResponse {
    result: Result;
    entries: SeriesIdentityShapeEntry[];
    total: number;
}

export interface ListSeriesIdentityShapeEntryVersionsRequest {
    key: SeriesIdentityShapeEntryKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SeriesIdentityShapeEntryVersionsFilter | null;
}

export interface ListSeriesIdentityShapeEntryVersionsResponse {
    result: Result;
    versions: SeriesIdentityShapeEntry[];
    total: number;
}

export interface GetSeriesIdentityShapeEntryVersionRequest {
    key: SeriesIdentityShapeEntryVersionKey;
}

export interface GetSeriesIdentityShapeEntryVersionResponse {
    result: Result;
    version: SeriesIdentityShapeEntry | null;
}

export const subjects = {
    list_series_identity_shape_entries_request: 'marketdata.v1.series_identity_shape_entries.list',
    get_series_identity_shape_entry_request: 'marketdata.v1.series_identity_shape_entries.get',
    get_many_series_identity_shape_entries_request:
        'marketdata.v1.series_identity_shape_entries.get_many',
    put_series_identity_shape_entry_request: 'marketdata.v1.series_identity_shape_entries.put',
    put_many_series_identity_shape_entries_request:
        'marketdata.v1.series_identity_shape_entries.put_many',
    delete_series_identity_shape_entry_request:
        'marketdata.v1.series_identity_shape_entries.delete',
    delete_many_series_identity_shape_entries_request:
        'marketdata.v1.series_identity_shape_entries.delete_many',
    list_by_shape_code_series_identity_shape_entries_request:
        'marketdata.v1.series_identity_shape_entries.list_by_shape_code',
    list_series_identity_shape_entry_versions_request:
        'marketdata.v1.series_identity_shape_entries_versions.list',
    get_series_identity_shape_entry_version_request:
        'marketdata.v1.series_identity_shape_entries_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_series_identity_shape_entries_request: true,
    get_series_identity_shape_entry_request: true,
    get_many_series_identity_shape_entries_request: true,
    put_series_identity_shape_entry_request: true,
    put_many_series_identity_shape_entries_request: true,
    delete_series_identity_shape_entry_request: true,
    delete_many_series_identity_shape_entries_request: true,
    list_by_shape_code_series_identity_shape_entries_request: true,
    list_series_identity_shape_entry_versions_request: true,
    get_series_identity_shape_entry_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.events.series_identity_shape_entries.created',
    updated: 'marketdata.v1.events.series_identity_shape_entries.updated',
    deleted: 'marketdata.v1.events.series_identity_shape_entries.deleted',
} as const;
