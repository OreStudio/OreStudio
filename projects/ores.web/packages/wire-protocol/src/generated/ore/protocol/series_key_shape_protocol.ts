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
import type { SeriesKeyShape } from '../domain/series_key_shape.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SeriesKeyShapeKey {
    series_type: string;
}

export interface SeriesKeyShapeWrite {
    series_type: string;
    qualifier_depth: number;
    has_point_dimension: boolean;
    default_point: string;
    description: string;
}

export interface SeriesKeyShapeChange {
    write: SeriesKeyShapeWrite;
    precondition: Precondition;
}

export interface SeriesKeyShapeRemoval {
    key: SeriesKeyShapeKey;
    precondition: Precondition;
}

export interface SeriesKeyShapeLookup {
    key: SeriesKeyShapeKey;
    series_key_shape: SeriesKeyShape | null;
}

export interface SeriesKeyShapeEvent {
    event_id: string;
    key: SeriesKeyShapeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SeriesKeyShapeVersionKey {
    series_key_shape: SeriesKeyShapeKey;
    version: number;
}

export interface SeriesKeyShapeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSeriesKeyShapesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListSeriesKeyShapesResponse {
    result: Result;
    shapes: SeriesKeyShape[];
    total: number;
}

export interface GetSeriesKeyShapeRequest {
    key: SeriesKeyShapeKey;
}

export interface GetSeriesKeyShapeResponse {
    result: Result;
    series_key_shape: SeriesKeyShape | null;
}

export interface GetManySeriesKeyShapesRequest {
    keys: SeriesKeyShapeKey[];
}

export interface GetManySeriesKeyShapesResponse {
    result: Result;
    entries: SeriesKeyShapeLookup[];
}

export interface PutSeriesKeyShapeRequest {
    change: SeriesKeyShapeChange;
    intent: ChangeIntent;
}

export interface PutSeriesKeyShapeResponse {
    result: Result;
    series_key_shape: SeriesKeyShape;
}

export interface PutManySeriesKeyShapesRequest {
    changes: SeriesKeyShapeChange[];
    intent: ChangeIntent;
}

export interface PutManySeriesKeyShapesResponse {
    result: Result;
    shapes: SeriesKeyShape[];
}

export interface DeleteSeriesKeyShapeRequest {
    removal: SeriesKeyShapeRemoval;
    intent: ChangeIntent;
}

export interface DeleteSeriesKeyShapeResponse {
    result: Result;
}

export interface DeleteManySeriesKeyShapesRequest {
    removals: SeriesKeyShapeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySeriesKeyShapesResponse {
    result: Result;
}

export interface ListSeriesKeyShapeVersionsRequest {
    key: SeriesKeyShapeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SeriesKeyShapeVersionsFilter | null;
}

export interface ListSeriesKeyShapeVersionsResponse {
    result: Result;
    versions: SeriesKeyShape[];
    total: number;
}

export interface GetSeriesKeyShapeVersionRequest {
    key: SeriesKeyShapeVersionKey;
}

export interface GetSeriesKeyShapeVersionResponse {
    result: Result;
    version: SeriesKeyShape;
}

export const subjects = {
    list_series_key_shapes_request: "ore.v1.series_key_shapes.list",
    get_series_key_shape_request: "ore.v1.series_key_shapes.get",
    get_many_series_key_shapes_request: "ore.v1.series_key_shapes.get_many",
    put_series_key_shape_request: "ore.v1.series_key_shapes.put",
    put_many_series_key_shapes_request: "ore.v1.series_key_shapes.put_many",
    delete_series_key_shape_request: "ore.v1.series_key_shapes.delete",
    delete_many_series_key_shapes_request: "ore.v1.series_key_shapes.delete_many",
    list_series_key_shape_versions_request: "ore.v1.series_key_shapes_versions.list",
    get_series_key_shape_version_request: "ore.v1.series_key_shapes_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_series_key_shapes_request: true,
    get_series_key_shape_request: true,
    get_many_series_key_shapes_request: true,
    put_series_key_shape_request: true,
    put_many_series_key_shapes_request: true,
    delete_series_key_shape_request: true,
    delete_many_series_key_shapes_request: true,
    list_series_key_shape_versions_request: true,
    get_series_key_shape_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "ore.v1.series_key_shapes_events.created",
    updated: "ore.v1.series_key_shapes_events.updated",
    deleted: "ore.v1.series_key_shapes_events.deleted",
} as const;
