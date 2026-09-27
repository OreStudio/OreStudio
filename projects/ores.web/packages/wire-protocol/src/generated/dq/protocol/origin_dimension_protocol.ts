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
import type { OriginDimension } from '../domain/origin_dimension.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface OriginDimensionKey {
    code: string;
}

export interface OriginDimensionWrite {
    code: string;
    name: string;
    description: string;
}

export interface OriginDimensionChange {
    write: OriginDimensionWrite;
    precondition: Precondition;
}

export interface OriginDimensionRemoval {
    key: OriginDimensionKey;
    precondition: Precondition;
}

export interface OriginDimensionLookup {
    key: OriginDimensionKey;
    origin_dimension: OriginDimension | null;
}

export interface OriginDimensionEvent {
    event_id: string;
    key: OriginDimensionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface OriginDimensionVersionKey {
    origin_dimension: OriginDimensionKey;
    version: number;
}

export interface OriginDimensionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListOriginDimensionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListOriginDimensionsResponse {
    result: Result;
    dimensions: OriginDimension[];
    total: number;
}

export interface GetOriginDimensionRequest {
    key: OriginDimensionKey;
}

export interface GetOriginDimensionResponse {
    result: Result;
    origin_dimension: OriginDimension | null;
}

export interface GetManyOriginDimensionsRequest {
    keys: OriginDimensionKey[];
}

export interface GetManyOriginDimensionsResponse {
    result: Result;
    entries: OriginDimensionLookup[];
}

export interface PutOriginDimensionRequest {
    change: OriginDimensionChange;
    intent: ChangeIntent;
}

export interface PutOriginDimensionResponse {
    result: Result;
    origin_dimension: OriginDimension;
}

export interface PutManyOriginDimensionsRequest {
    changes: OriginDimensionChange[];
    intent: ChangeIntent;
}

export interface PutManyOriginDimensionsResponse {
    result: Result;
    dimensions: OriginDimension[];
}

export interface DeleteOriginDimensionRequest {
    removal: OriginDimensionRemoval;
    intent: ChangeIntent;
}

export interface DeleteOriginDimensionResponse {
    result: Result;
}

export interface DeleteManyOriginDimensionsRequest {
    removals: OriginDimensionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyOriginDimensionsResponse {
    result: Result;
}

export interface ListOriginDimensionVersionsRequest {
    key: OriginDimensionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: OriginDimensionVersionsFilter | null;
}

export interface ListOriginDimensionVersionsResponse {
    result: Result;
    versions: OriginDimension[];
    total: number;
}

export interface GetOriginDimensionVersionRequest {
    key: OriginDimensionVersionKey;
}

export interface GetOriginDimensionVersionResponse {
    result: Result;
    version: OriginDimension;
}

export const subjects = {
    list_origin_dimensions_request: 'dq.v1.origin_dimensions.list',
    get_origin_dimension_request: 'dq.v1.origin_dimensions.get',
    get_many_origin_dimensions_request: 'dq.v1.origin_dimensions.get_many',
    put_origin_dimension_request: 'dq.v1.origin_dimensions.put',
    put_many_origin_dimensions_request: 'dq.v1.origin_dimensions.put_many',
    delete_origin_dimension_request: 'dq.v1.origin_dimensions.delete',
    delete_many_origin_dimensions_request: 'dq.v1.origin_dimensions.delete_many',
    list_origin_dimension_versions_request: 'dq.v1.origin_dimensions_versions.list',
    get_origin_dimension_version_request: 'dq.v1.origin_dimensions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_origin_dimensions_request: true,
    get_origin_dimension_request: true,
    get_many_origin_dimensions_request: true,
    put_origin_dimension_request: true,
    put_many_origin_dimensions_request: true,
    delete_origin_dimension_request: true,
    delete_many_origin_dimensions_request: true,
    list_origin_dimension_versions_request: true,
    get_origin_dimension_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'dq.v1.origin_dimensions_events.created',
    updated: 'dq.v1.origin_dimensions_events.updated',
    deleted: 'dq.v1.origin_dimensions_events.deleted',
} as const;
