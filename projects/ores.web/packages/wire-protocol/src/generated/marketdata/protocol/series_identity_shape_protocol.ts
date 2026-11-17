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
import type { SeriesIdentityShape } from '../domain/series_identity_shape.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SeriesIdentityShapeKey {
    asset_class: string;
    series_subclass: string;
    index_family: string;
    field: string;
}

export interface SeriesIdentityShapeWrite {
    asset_class: string;
    series_subclass: string;
    index_family: string;
    field: string;
    requirement: string;
    description: string;
}

export interface SeriesIdentityShapeChange {
    write: SeriesIdentityShapeWrite;
    precondition: Precondition;
}

export interface SeriesIdentityShapeRemoval {
    key: SeriesIdentityShapeKey;
    precondition: Precondition;
}

export interface SeriesIdentityShapeLookup {
    key: SeriesIdentityShapeKey;
    series_identity_shape: SeriesIdentityShape | null;
}

export interface SeriesIdentityShapeEvent {
    event_id: string;
    key: SeriesIdentityShapeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SeriesIdentityShapeVersionKey {
    series_identity_shape: SeriesIdentityShapeKey;
    version: number;
}

export interface SeriesIdentityShapeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSeriesIdentityShapesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListSeriesIdentityShapesResponse {
    result: Result;
    shapes: SeriesIdentityShape[];
    total: number;
}

export interface GetSeriesIdentityShapeRequest {
    key: SeriesIdentityShapeKey;
}

export interface GetSeriesIdentityShapeResponse {
    result: Result;
    series_identity_shape: SeriesIdentityShape | null;
}

export interface GetManySeriesIdentityShapesRequest {
    keys: SeriesIdentityShapeKey[];
}

export interface GetManySeriesIdentityShapesResponse {
    result: Result;
    entries: SeriesIdentityShapeLookup[];
}

export interface PutSeriesIdentityShapeRequest {
    change: SeriesIdentityShapeChange;
    intent: ChangeIntent;
}

export interface PutSeriesIdentityShapeResponse {
    result: Result;
    series_identity_shape: SeriesIdentityShape | null;
}

export interface PutManySeriesIdentityShapesRequest {
    changes: SeriesIdentityShapeChange[];
    intent: ChangeIntent;
}

export interface PutManySeriesIdentityShapesResponse {
    result: Result;
    shapes: SeriesIdentityShape[];
}

export interface DeleteSeriesIdentityShapeRequest {
    removal: SeriesIdentityShapeRemoval;
    intent: ChangeIntent;
}

export interface DeleteSeriesIdentityShapeResponse {
    result: Result;
}

export interface DeleteManySeriesIdentityShapesRequest {
    removals: SeriesIdentityShapeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySeriesIdentityShapesResponse {
    result: Result;
}

export interface ListSeriesIdentityShapeVersionsRequest {
    key: SeriesIdentityShapeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SeriesIdentityShapeVersionsFilter | null;
}

export interface ListSeriesIdentityShapeVersionsResponse {
    result: Result;
    versions: SeriesIdentityShape[];
    total: number;
}

export interface GetSeriesIdentityShapeVersionRequest {
    key: SeriesIdentityShapeVersionKey;
}

export interface GetSeriesIdentityShapeVersionResponse {
    result: Result;
    version: SeriesIdentityShape | null;
}

export const subjects = {
    list_series_identity_shapes_request: 'marketdata.v1.series_identity_shapes.list',
    get_series_identity_shape_request: 'marketdata.v1.series_identity_shapes.get',
    get_many_series_identity_shapes_request: 'marketdata.v1.series_identity_shapes.get_many',
    put_series_identity_shape_request: 'marketdata.v1.series_identity_shapes.put',
    put_many_series_identity_shapes_request: 'marketdata.v1.series_identity_shapes.put_many',
    delete_series_identity_shape_request: 'marketdata.v1.series_identity_shapes.delete',
    delete_many_series_identity_shapes_request: 'marketdata.v1.series_identity_shapes.delete_many',
    list_series_identity_shape_versions_request:
        'marketdata.v1.series_identity_shapes_versions.list',
    get_series_identity_shape_version_request: 'marketdata.v1.series_identity_shapes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_series_identity_shapes_request: true,
    get_series_identity_shape_request: true,
    get_many_series_identity_shapes_request: true,
    put_series_identity_shape_request: true,
    put_many_series_identity_shapes_request: true,
    delete_series_identity_shape_request: true,
    delete_many_series_identity_shapes_request: true,
    list_series_identity_shape_versions_request: true,
    get_series_identity_shape_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.events.series_identity_shapes.created',
    updated: 'marketdata.v1.events.series_identity_shapes.updated',
    deleted: 'marketdata.v1.events.series_identity_shapes.deleted',
} as const;
