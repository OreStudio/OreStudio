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
import type { CurveSegmentConvention } from '../domain/curve_segment_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSegmentConventionKey {
    id: string;
}

export interface CurveSegmentConventionWrite {
    id: string;
    curve_segment_id: string;
    conventions: string;
}

export interface CurveSegmentConventionChange {
    write: CurveSegmentConventionWrite;
    precondition: Precondition;
}

export interface CurveSegmentConventionRemoval {
    key: CurveSegmentConventionKey;
    precondition: Precondition;
}

export interface CurveSegmentConventionLookup {
    key: CurveSegmentConventionKey;
    curve_segment_convention: CurveSegmentConvention | null;
}

export interface CurveSegmentConventionsFilter {
    id_one_of: string[] | null;
}

export interface CurveSegmentConventionEvent {
    event_id: string;
    key: CurveSegmentConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSegmentConventionVersionKey {
    curve_segment_convention: CurveSegmentConventionKey;
    version: number;
}

export interface CurveSegmentConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSegmentConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentConventionsFilter | null;
    as_of: string | null;
}

export interface ListCurveSegmentConventionsResponse {
    result: Result;
    segment_conventions: CurveSegmentConvention[];
    total: number;
}

export interface GetCurveSegmentConventionRequest {
    key: CurveSegmentConventionKey;
}

export interface GetCurveSegmentConventionResponse {
    result: Result;
    curve_segment_convention: CurveSegmentConvention | null;
}

export interface GetManyCurveSegmentConventionsRequest {
    keys: CurveSegmentConventionKey[];
}

export interface GetManyCurveSegmentConventionsResponse {
    result: Result;
    entries: CurveSegmentConventionLookup[];
}

export interface PutCurveSegmentConventionRequest {
    change: CurveSegmentConventionChange;
    intent: ChangeIntent;
}

export interface PutCurveSegmentConventionResponse {
    result: Result;
    curve_segment_convention: CurveSegmentConvention | null;
}

export interface PutManyCurveSegmentConventionsRequest {
    changes: CurveSegmentConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSegmentConventionsResponse {
    result: Result;
    segment_conventions: CurveSegmentConvention[];
}

export interface DeleteCurveSegmentConventionRequest {
    removal: CurveSegmentConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSegmentConventionResponse {
    result: Result;
}

export interface DeleteManyCurveSegmentConventionsRequest {
    removals: CurveSegmentConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSegmentConventionsResponse {
    result: Result;
}

export interface ListCurveSegmentConventionVersionsRequest {
    key: CurveSegmentConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentConventionVersionsFilter | null;
}

export interface ListCurveSegmentConventionVersionsResponse {
    result: Result;
    versions: CurveSegmentConvention[];
    total: number;
}

export interface GetCurveSegmentConventionVersionRequest {
    key: CurveSegmentConventionVersionKey;
}

export interface GetCurveSegmentConventionVersionResponse {
    result: Result;
    version: CurveSegmentConvention | null;
}

export const subjects = {
    list_curve_segment_conventions_request: 'refdata.v1.curve_segment_conventions.list',
    get_curve_segment_convention_request: 'refdata.v1.curve_segment_conventions.get',
    get_many_curve_segment_conventions_request: 'refdata.v1.curve_segment_conventions.get_many',
    put_curve_segment_convention_request: 'refdata.v1.curve_segment_conventions.put',
    put_many_curve_segment_conventions_request: 'refdata.v1.curve_segment_conventions.put_many',
    delete_curve_segment_convention_request: 'refdata.v1.curve_segment_conventions.delete',
    delete_many_curve_segment_conventions_request:
        'refdata.v1.curve_segment_conventions.delete_many',
    list_curve_segment_convention_versions_request:
        'refdata.v1.curve_segment_conventions_versions.list',
    get_curve_segment_convention_version_request:
        'refdata.v1.curve_segment_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_segment_conventions_request: true,
    get_curve_segment_convention_request: true,
    get_many_curve_segment_conventions_request: true,
    put_curve_segment_convention_request: true,
    put_many_curve_segment_conventions_request: true,
    delete_curve_segment_convention_request: true,
    delete_many_curve_segment_conventions_request: true,
    list_curve_segment_convention_versions_request: true,
    get_curve_segment_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.curve_segment_conventions.created',
    updated: 'refdata.v1.events.curve_segment_conventions.updated',
    deleted: 'refdata.v1.events.curve_segment_conventions.deleted',
} as const;
