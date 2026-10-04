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
import type { TreatmentDimension } from '../domain/treatment_dimension.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TreatmentDimensionKey {
    code: string;
}

export interface TreatmentDimensionWrite {
    code: string;
    name: string;
    description: string;
}

export interface TreatmentDimensionChange {
    write: TreatmentDimensionWrite;
    precondition: Precondition;
}

export interface TreatmentDimensionRemoval {
    key: TreatmentDimensionKey;
    precondition: Precondition;
}

export interface TreatmentDimensionLookup {
    key: TreatmentDimensionKey;
    treatment_dimension: TreatmentDimension | null;
}

export interface TreatmentDimensionsFilter {
    code_one_of: string[] | null;
}

export interface TreatmentDimensionEvent {
    event_id: string;
    key: TreatmentDimensionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TreatmentDimensionVersionKey {
    treatment_dimension: TreatmentDimensionKey;
    version: number;
}

export interface TreatmentDimensionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTreatmentDimensionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TreatmentDimensionsFilter | null;
}

export interface ListTreatmentDimensionsResponse {
    result: Result;
    dimensions: TreatmentDimension[];
    total: number;
}

export interface GetTreatmentDimensionRequest {
    key: TreatmentDimensionKey;
}

export interface GetTreatmentDimensionResponse {
    result: Result;
    treatment_dimension: TreatmentDimension | null;
}

export interface GetManyTreatmentDimensionsRequest {
    keys: TreatmentDimensionKey[];
}

export interface GetManyTreatmentDimensionsResponse {
    result: Result;
    entries: TreatmentDimensionLookup[];
}

export interface PutTreatmentDimensionRequest {
    change: TreatmentDimensionChange;
    intent: ChangeIntent;
}

export interface PutTreatmentDimensionResponse {
    result: Result;
    treatment_dimension: TreatmentDimension | null;
}

export interface PutManyTreatmentDimensionsRequest {
    changes: TreatmentDimensionChange[];
    intent: ChangeIntent;
}

export interface PutManyTreatmentDimensionsResponse {
    result: Result;
    dimensions: TreatmentDimension[];
}

export interface DeleteTreatmentDimensionRequest {
    removal: TreatmentDimensionRemoval;
    intent: ChangeIntent;
}

export interface DeleteTreatmentDimensionResponse {
    result: Result;
}

export interface DeleteManyTreatmentDimensionsRequest {
    removals: TreatmentDimensionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTreatmentDimensionsResponse {
    result: Result;
}

export interface ListTreatmentDimensionVersionsRequest {
    key: TreatmentDimensionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TreatmentDimensionVersionsFilter | null;
}

export interface ListTreatmentDimensionVersionsResponse {
    result: Result;
    versions: TreatmentDimension[];
    total: number;
}

export interface GetTreatmentDimensionVersionRequest {
    key: TreatmentDimensionVersionKey;
}

export interface GetTreatmentDimensionVersionResponse {
    result: Result;
    version: TreatmentDimension | null;
}

export const subjects = {
    list_treatment_dimensions_request: 'dq.v1.treatment_dimensions.list',
    get_treatment_dimension_request: 'dq.v1.treatment_dimensions.get',
    get_many_treatment_dimensions_request: 'dq.v1.treatment_dimensions.get_many',
    put_treatment_dimension_request: 'dq.v1.treatment_dimensions.put',
    put_many_treatment_dimensions_request: 'dq.v1.treatment_dimensions.put_many',
    delete_treatment_dimension_request: 'dq.v1.treatment_dimensions.delete',
    delete_many_treatment_dimensions_request: 'dq.v1.treatment_dimensions.delete_many',
    list_treatment_dimension_versions_request: 'dq.v1.treatment_dimensions_versions.list',
    get_treatment_dimension_version_request: 'dq.v1.treatment_dimensions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_treatment_dimensions_request: true,
    get_treatment_dimension_request: true,
    get_many_treatment_dimensions_request: true,
    put_treatment_dimension_request: true,
    put_many_treatment_dimensions_request: true,
    delete_treatment_dimension_request: true,
    delete_many_treatment_dimensions_request: true,
    list_treatment_dimension_versions_request: true,
    get_treatment_dimension_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'dq.v1.treatment_dimensions_events.created',
    updated: 'dq.v1.treatment_dimensions_events.updated',
    deleted: 'dq.v1.treatment_dimensions_events.deleted',
} as const;
