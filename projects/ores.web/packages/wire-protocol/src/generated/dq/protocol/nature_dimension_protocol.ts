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
import type { NatureDimension } from '../domain/nature_dimension.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface NatureDimensionKey {
    code: string;
}

export interface NatureDimensionWrite {
    code: string;
    name: string;
    description: string;
}

export interface NatureDimensionChange {
    write: NatureDimensionWrite;
    precondition: Precondition;
}

export interface NatureDimensionRemoval {
    key: NatureDimensionKey;
    precondition: Precondition;
}

export interface NatureDimensionLookup {
    key: NatureDimensionKey;
    nature_dimension: NatureDimension | null;
}

export interface NatureDimensionsFilter {
    code_one_of: string[] | null;
}

export interface NatureDimensionEvent {
    event_id: string;
    key: NatureDimensionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface NatureDimensionVersionKey {
    nature_dimension: NatureDimensionKey;
    version: number;
}

export interface NatureDimensionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListNatureDimensionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: NatureDimensionsFilter | null;
    as_of: string | null;
}

export interface ListNatureDimensionsResponse {
    result: Result;
    dimensions: NatureDimension[];
    total: number;
}

export interface GetNatureDimensionRequest {
    key: NatureDimensionKey;
}

export interface GetNatureDimensionResponse {
    result: Result;
    nature_dimension: NatureDimension | null;
}

export interface GetManyNatureDimensionsRequest {
    keys: NatureDimensionKey[];
}

export interface GetManyNatureDimensionsResponse {
    result: Result;
    entries: NatureDimensionLookup[];
}

export interface PutNatureDimensionRequest {
    change: NatureDimensionChange;
    intent: ChangeIntent;
}

export interface PutNatureDimensionResponse {
    result: Result;
    nature_dimension: NatureDimension | null;
}

export interface PutManyNatureDimensionsRequest {
    changes: NatureDimensionChange[];
    intent: ChangeIntent;
}

export interface PutManyNatureDimensionsResponse {
    result: Result;
    dimensions: NatureDimension[];
}

export interface DeleteNatureDimensionRequest {
    removal: NatureDimensionRemoval;
    intent: ChangeIntent;
}

export interface DeleteNatureDimensionResponse {
    result: Result;
}

export interface DeleteManyNatureDimensionsRequest {
    removals: NatureDimensionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyNatureDimensionsResponse {
    result: Result;
}

export interface ListNatureDimensionVersionsRequest {
    key: NatureDimensionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: NatureDimensionVersionsFilter | null;
}

export interface ListNatureDimensionVersionsResponse {
    result: Result;
    versions: NatureDimension[];
    total: number;
}

export interface GetNatureDimensionVersionRequest {
    key: NatureDimensionVersionKey;
}

export interface GetNatureDimensionVersionResponse {
    result: Result;
    version: NatureDimension | null;
}

export const subjects = {
    list_nature_dimensions_request: 'dq.v1.nature_dimensions.list',
    get_nature_dimension_request: 'dq.v1.nature_dimensions.get',
    get_many_nature_dimensions_request: 'dq.v1.nature_dimensions.get_many',
    put_nature_dimension_request: 'dq.v1.nature_dimensions.put',
    put_many_nature_dimensions_request: 'dq.v1.nature_dimensions.put_many',
    delete_nature_dimension_request: 'dq.v1.nature_dimensions.delete',
    delete_many_nature_dimensions_request: 'dq.v1.nature_dimensions.delete_many',
    list_nature_dimension_versions_request: 'dq.v1.nature_dimensions_versions.list',
    get_nature_dimension_version_request: 'dq.v1.nature_dimensions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_nature_dimensions_request: true,
    get_nature_dimension_request: true,
    get_many_nature_dimensions_request: true,
    put_nature_dimension_request: true,
    put_many_nature_dimensions_request: true,
    delete_nature_dimension_request: true,
    delete_many_nature_dimensions_request: true,
    list_nature_dimension_versions_request: true,
    get_nature_dimension_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'dq.v1.nature_dimensions_events.created',
    updated: 'dq.v1.nature_dimensions_events.updated',
    deleted: 'dq.v1.nature_dimensions_events.deleted',
} as const;
