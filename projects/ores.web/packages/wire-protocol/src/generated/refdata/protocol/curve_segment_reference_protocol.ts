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
import type { CurveSegmentReference } from '../domain/curve_segment_reference.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSegmentReferenceKey {
    id: string;
}

export interface CurveSegmentReferenceWrite {
    id: string;
    curve_segment_id: string;
    reference_curve: string;
}

export interface CurveSegmentReferenceChange {
    write: CurveSegmentReferenceWrite;
    precondition: Precondition;
}

export interface CurveSegmentReferenceRemoval {
    key: CurveSegmentReferenceKey;
    precondition: Precondition;
}

export interface CurveSegmentReferenceLookup {
    key: CurveSegmentReferenceKey;
    curve_segment_reference: CurveSegmentReference | null;
}

export interface CurveSegmentReferencesFilter {
    id_one_of: string[] | null;
}

export interface CurveSegmentReferenceEvent {
    event_id: string;
    key: CurveSegmentReferenceKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSegmentReferenceVersionKey {
    curve_segment_reference: CurveSegmentReferenceKey;
    version: number;
}

export interface CurveSegmentReferenceVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSegmentReferencesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentReferencesFilter | null;
    as_of: string | null;
}

export interface ListCurveSegmentReferencesResponse {
    result: Result;
    segment_references: CurveSegmentReference[];
    total: number;
}

export interface GetCurveSegmentReferenceRequest {
    key: CurveSegmentReferenceKey;
}

export interface GetCurveSegmentReferenceResponse {
    result: Result;
    curve_segment_reference: CurveSegmentReference | null;
}

export interface GetManyCurveSegmentReferencesRequest {
    keys: CurveSegmentReferenceKey[];
}

export interface GetManyCurveSegmentReferencesResponse {
    result: Result;
    entries: CurveSegmentReferenceLookup[];
}

export interface PutCurveSegmentReferenceRequest {
    change: CurveSegmentReferenceChange;
    intent: ChangeIntent;
}

export interface PutCurveSegmentReferenceResponse {
    result: Result;
    curve_segment_reference: CurveSegmentReference | null;
}

export interface PutManyCurveSegmentReferencesRequest {
    changes: CurveSegmentReferenceChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSegmentReferencesResponse {
    result: Result;
    segment_references: CurveSegmentReference[];
}

export interface DeleteCurveSegmentReferenceRequest {
    removal: CurveSegmentReferenceRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSegmentReferenceResponse {
    result: Result;
}

export interface DeleteManyCurveSegmentReferencesRequest {
    removals: CurveSegmentReferenceRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSegmentReferencesResponse {
    result: Result;
}

export interface ListCurveSegmentReferenceVersionsRequest {
    key: CurveSegmentReferenceKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentReferenceVersionsFilter | null;
}

export interface ListCurveSegmentReferenceVersionsResponse {
    result: Result;
    versions: CurveSegmentReference[];
    total: number;
}

export interface GetCurveSegmentReferenceVersionRequest {
    key: CurveSegmentReferenceVersionKey;
}

export interface GetCurveSegmentReferenceVersionResponse {
    result: Result;
    version: CurveSegmentReference | null;
}

export const subjects = {
    list_curve_segment_references_request: 'refdata.v1.curve_segment_references.list',
    get_curve_segment_reference_request: 'refdata.v1.curve_segment_references.get',
    get_many_curve_segment_references_request: 'refdata.v1.curve_segment_references.get_many',
    put_curve_segment_reference_request: 'refdata.v1.curve_segment_references.put',
    put_many_curve_segment_references_request: 'refdata.v1.curve_segment_references.put_many',
    delete_curve_segment_reference_request: 'refdata.v1.curve_segment_references.delete',
    delete_many_curve_segment_references_request: 'refdata.v1.curve_segment_references.delete_many',
    list_curve_segment_reference_versions_request:
        'refdata.v1.curve_segment_references_versions.list',
    get_curve_segment_reference_version_request: 'refdata.v1.curve_segment_references_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_segment_references_request: true,
    get_curve_segment_reference_request: true,
    get_many_curve_segment_references_request: true,
    put_curve_segment_reference_request: true,
    put_many_curve_segment_references_request: true,
    delete_curve_segment_reference_request: true,
    delete_many_curve_segment_references_request: true,
    list_curve_segment_reference_versions_request: true,
    get_curve_segment_reference_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_segment_references_events.created',
    updated: 'refdata.v1.curve_segment_references_events.updated',
    deleted: 'refdata.v1.curve_segment_references_events.deleted',
} as const;
