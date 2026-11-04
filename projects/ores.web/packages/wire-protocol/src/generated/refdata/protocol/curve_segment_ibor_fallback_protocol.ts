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
import type { CurveSegmentIborFallback } from '../domain/curve_segment_ibor_fallback.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSegmentIborFallbackKey {
    id: string;
}

export interface CurveSegmentIborFallbackWrite {
    id: string;
    curve_segment_id: string;
    ibor_index: string;
    rfr_curve: string;
    rfr_index: string | null;
    spread: string | null;
}

export interface CurveSegmentIborFallbackChange {
    write: CurveSegmentIborFallbackWrite;
    precondition: Precondition;
}

export interface CurveSegmentIborFallbackRemoval {
    key: CurveSegmentIborFallbackKey;
    precondition: Precondition;
}

export interface CurveSegmentIborFallbackLookup {
    key: CurveSegmentIborFallbackKey;
    curve_segment_ibor_fallback: CurveSegmentIborFallback | null;
}

export interface CurveSegmentIborFallbacksFilter {
    id_one_of: string[] | null;
}

export interface CurveSegmentIborFallbackEvent {
    event_id: string;
    key: CurveSegmentIborFallbackKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSegmentIborFallbackVersionKey {
    curve_segment_ibor_fallback: CurveSegmentIborFallbackKey;
    version: number;
}

export interface CurveSegmentIborFallbackVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSegmentIborFallbacksRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentIborFallbacksFilter | null;
    as_of: string | null;
}

export interface ListCurveSegmentIborFallbacksResponse {
    result: Result;
    segment_ibor_fallbacks: CurveSegmentIborFallback[];
    total: number;
}

export interface GetCurveSegmentIborFallbackRequest {
    key: CurveSegmentIborFallbackKey;
}

export interface GetCurveSegmentIborFallbackResponse {
    result: Result;
    curve_segment_ibor_fallback: CurveSegmentIborFallback | null;
}

export interface GetManyCurveSegmentIborFallbacksRequest {
    keys: CurveSegmentIborFallbackKey[];
}

export interface GetManyCurveSegmentIborFallbacksResponse {
    result: Result;
    entries: CurveSegmentIborFallbackLookup[];
}

export interface PutCurveSegmentIborFallbackRequest {
    change: CurveSegmentIborFallbackChange;
    intent: ChangeIntent;
}

export interface PutCurveSegmentIborFallbackResponse {
    result: Result;
    curve_segment_ibor_fallback: CurveSegmentIborFallback | null;
}

export interface PutManyCurveSegmentIborFallbacksRequest {
    changes: CurveSegmentIborFallbackChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSegmentIborFallbacksResponse {
    result: Result;
    segment_ibor_fallbacks: CurveSegmentIborFallback[];
}

export interface DeleteCurveSegmentIborFallbackRequest {
    removal: CurveSegmentIborFallbackRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSegmentIborFallbackResponse {
    result: Result;
}

export interface DeleteManyCurveSegmentIborFallbacksRequest {
    removals: CurveSegmentIborFallbackRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSegmentIborFallbacksResponse {
    result: Result;
}

export interface ListCurveSegmentIborFallbackVersionsRequest {
    key: CurveSegmentIborFallbackKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentIborFallbackVersionsFilter | null;
}

export interface ListCurveSegmentIborFallbackVersionsResponse {
    result: Result;
    versions: CurveSegmentIborFallback[];
    total: number;
}

export interface GetCurveSegmentIborFallbackVersionRequest {
    key: CurveSegmentIborFallbackVersionKey;
}

export interface GetCurveSegmentIborFallbackVersionResponse {
    result: Result;
    version: CurveSegmentIborFallback | null;
}

export const subjects = {
    list_curve_segment_ibor_fallbacks_request: 'refdata.v1.curve_segment_ibor_fallbacks.list',
    get_curve_segment_ibor_fallback_request: 'refdata.v1.curve_segment_ibor_fallbacks.get',
    get_many_curve_segment_ibor_fallbacks_request:
        'refdata.v1.curve_segment_ibor_fallbacks.get_many',
    put_curve_segment_ibor_fallback_request: 'refdata.v1.curve_segment_ibor_fallbacks.put',
    put_many_curve_segment_ibor_fallbacks_request:
        'refdata.v1.curve_segment_ibor_fallbacks.put_many',
    delete_curve_segment_ibor_fallback_request: 'refdata.v1.curve_segment_ibor_fallbacks.delete',
    delete_many_curve_segment_ibor_fallbacks_request:
        'refdata.v1.curve_segment_ibor_fallbacks.delete_many',
    list_curve_segment_ibor_fallback_versions_request:
        'refdata.v1.curve_segment_ibor_fallbacks_versions.list',
    get_curve_segment_ibor_fallback_version_request:
        'refdata.v1.curve_segment_ibor_fallbacks_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_segment_ibor_fallbacks_request: true,
    get_curve_segment_ibor_fallback_request: true,
    get_many_curve_segment_ibor_fallbacks_request: true,
    put_curve_segment_ibor_fallback_request: true,
    put_many_curve_segment_ibor_fallbacks_request: true,
    delete_curve_segment_ibor_fallback_request: true,
    delete_many_curve_segment_ibor_fallbacks_request: true,
    list_curve_segment_ibor_fallback_versions_request: true,
    get_curve_segment_ibor_fallback_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.curve_segment_ibor_fallbacks.created',
    updated: 'refdata.v1.events.curve_segment_ibor_fallbacks.updated',
    deleted: 'refdata.v1.events.curve_segment_ibor_fallbacks.deleted',
} as const;
