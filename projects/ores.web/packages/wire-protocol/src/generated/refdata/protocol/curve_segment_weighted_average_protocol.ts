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
import type { CurveSegmentWeightedAverage } from '../domain/curve_segment_weighted_average.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSegmentWeightedAverageKey {
    id: string;
}

export interface CurveSegmentWeightedAverageWrite {
    id: string;
    curve_segment_id: string;
    reference_curve_1: string;
    reference_curve_2: string;
    weight_1: number;
    weight_2: number;
}

export interface CurveSegmentWeightedAverageChange {
    write: CurveSegmentWeightedAverageWrite;
    precondition: Precondition;
}

export interface CurveSegmentWeightedAverageRemoval {
    key: CurveSegmentWeightedAverageKey;
    precondition: Precondition;
}

export interface CurveSegmentWeightedAverageLookup {
    key: CurveSegmentWeightedAverageKey;
    curve_segment_weighted_average: CurveSegmentWeightedAverage | null;
}

export interface CurveSegmentWeightedAveragesFilter {
    id_one_of: string[] | null;
}

export interface CurveSegmentWeightedAverageEvent {
    event_id: string;
    key: CurveSegmentWeightedAverageKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSegmentWeightedAverageVersionKey {
    curve_segment_weighted_average: CurveSegmentWeightedAverageKey;
    version: number;
}

export interface CurveSegmentWeightedAverageVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSegmentWeightedAveragesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentWeightedAveragesFilter | null;
    as_of: string | null;
}

export interface ListCurveSegmentWeightedAveragesResponse {
    result: Result;
    segment_weighted_averages: CurveSegmentWeightedAverage[];
    total: number;
}

export interface GetCurveSegmentWeightedAverageRequest {
    key: CurveSegmentWeightedAverageKey;
}

export interface GetCurveSegmentWeightedAverageResponse {
    result: Result;
    curve_segment_weighted_average: CurveSegmentWeightedAverage | null;
}

export interface GetManyCurveSegmentWeightedAveragesRequest {
    keys: CurveSegmentWeightedAverageKey[];
}

export interface GetManyCurveSegmentWeightedAveragesResponse {
    result: Result;
    entries: CurveSegmentWeightedAverageLookup[];
}

export interface PutCurveSegmentWeightedAverageRequest {
    change: CurveSegmentWeightedAverageChange;
    intent: ChangeIntent;
}

export interface PutCurveSegmentWeightedAverageResponse {
    result: Result;
    curve_segment_weighted_average: CurveSegmentWeightedAverage | null;
}

export interface PutManyCurveSegmentWeightedAveragesRequest {
    changes: CurveSegmentWeightedAverageChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSegmentWeightedAveragesResponse {
    result: Result;
    segment_weighted_averages: CurveSegmentWeightedAverage[];
}

export interface DeleteCurveSegmentWeightedAverageRequest {
    removal: CurveSegmentWeightedAverageRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSegmentWeightedAverageResponse {
    result: Result;
}

export interface DeleteManyCurveSegmentWeightedAveragesRequest {
    removals: CurveSegmentWeightedAverageRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSegmentWeightedAveragesResponse {
    result: Result;
}

export interface ListCurveSegmentWeightedAverageVersionsRequest {
    key: CurveSegmentWeightedAverageKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentWeightedAverageVersionsFilter | null;
}

export interface ListCurveSegmentWeightedAverageVersionsResponse {
    result: Result;
    versions: CurveSegmentWeightedAverage[];
    total: number;
}

export interface GetCurveSegmentWeightedAverageVersionRequest {
    key: CurveSegmentWeightedAverageVersionKey;
}

export interface GetCurveSegmentWeightedAverageVersionResponse {
    result: Result;
    version: CurveSegmentWeightedAverage | null;
}

export const subjects = {
    list_curve_segment_weighted_averages_request: 'refdata.v1.curve_segment_weighted_averages.list',
    get_curve_segment_weighted_average_request: 'refdata.v1.curve_segment_weighted_averages.get',
    get_many_curve_segment_weighted_averages_request:
        'refdata.v1.curve_segment_weighted_averages.get_many',
    put_curve_segment_weighted_average_request: 'refdata.v1.curve_segment_weighted_averages.put',
    put_many_curve_segment_weighted_averages_request:
        'refdata.v1.curve_segment_weighted_averages.put_many',
    delete_curve_segment_weighted_average_request:
        'refdata.v1.curve_segment_weighted_averages.delete',
    delete_many_curve_segment_weighted_averages_request:
        'refdata.v1.curve_segment_weighted_averages.delete_many',
    list_curve_segment_weighted_average_versions_request:
        'refdata.v1.curve_segment_weighted_averages_versions.list',
    get_curve_segment_weighted_average_version_request:
        'refdata.v1.curve_segment_weighted_averages_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_segment_weighted_averages_request: true,
    get_curve_segment_weighted_average_request: true,
    get_many_curve_segment_weighted_averages_request: true,
    put_curve_segment_weighted_average_request: true,
    put_many_curve_segment_weighted_averages_request: true,
    delete_curve_segment_weighted_average_request: true,
    delete_many_curve_segment_weighted_averages_request: true,
    list_curve_segment_weighted_average_versions_request: true,
    get_curve_segment_weighted_average_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_segment_weighted_averages_events.created',
    updated: 'refdata.v1.curve_segment_weighted_averages_events.updated',
    deleted: 'refdata.v1.curve_segment_weighted_averages_events.deleted',
} as const;
