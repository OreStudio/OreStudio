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
import type { CurveSegmentDiscountRatio } from '../domain/curve_segment_discount_ratio.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSegmentDiscountRatioKey {
    id: string;
}

export interface CurveSegmentDiscountRatioWrite {
    id: string;
    curve_segment_id: string;
    base_curve: string;
    base_curve_currency: string;
    numerator_curve: string;
    numerator_curve_currency: string;
    denominator_curve: string;
    denominator_curve_currency: string;
}

export interface CurveSegmentDiscountRatioChange {
    write: CurveSegmentDiscountRatioWrite;
    precondition: Precondition;
}

export interface CurveSegmentDiscountRatioRemoval {
    key: CurveSegmentDiscountRatioKey;
    precondition: Precondition;
}

export interface CurveSegmentDiscountRatioLookup {
    key: CurveSegmentDiscountRatioKey;
    curve_segment_discount_ratio: CurveSegmentDiscountRatio | null;
}

export interface CurveSegmentDiscountRatiosFilter {
    id_one_of: string[] | null;
}

export interface CurveSegmentDiscountRatioEvent {
    event_id: string;
    key: CurveSegmentDiscountRatioKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSegmentDiscountRatioVersionKey {
    curve_segment_discount_ratio: CurveSegmentDiscountRatioKey;
    version: number;
}

export interface CurveSegmentDiscountRatioVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSegmentDiscountRatiosRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentDiscountRatiosFilter | null;
    as_of: string | null;
}

export interface ListCurveSegmentDiscountRatiosResponse {
    result: Result;
    segment_discount_ratios: CurveSegmentDiscountRatio[];
    total: number;
}

export interface GetCurveSegmentDiscountRatioRequest {
    key: CurveSegmentDiscountRatioKey;
}

export interface GetCurveSegmentDiscountRatioResponse {
    result: Result;
    curve_segment_discount_ratio: CurveSegmentDiscountRatio | null;
}

export interface GetManyCurveSegmentDiscountRatiosRequest {
    keys: CurveSegmentDiscountRatioKey[];
}

export interface GetManyCurveSegmentDiscountRatiosResponse {
    result: Result;
    entries: CurveSegmentDiscountRatioLookup[];
}

export interface PutCurveSegmentDiscountRatioRequest {
    change: CurveSegmentDiscountRatioChange;
    intent: ChangeIntent;
}

export interface PutCurveSegmentDiscountRatioResponse {
    result: Result;
    curve_segment_discount_ratio: CurveSegmentDiscountRatio | null;
}

export interface PutManyCurveSegmentDiscountRatiosRequest {
    changes: CurveSegmentDiscountRatioChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSegmentDiscountRatiosResponse {
    result: Result;
    segment_discount_ratios: CurveSegmentDiscountRatio[];
}

export interface DeleteCurveSegmentDiscountRatioRequest {
    removal: CurveSegmentDiscountRatioRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSegmentDiscountRatioResponse {
    result: Result;
}

export interface DeleteManyCurveSegmentDiscountRatiosRequest {
    removals: CurveSegmentDiscountRatioRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSegmentDiscountRatiosResponse {
    result: Result;
}

export interface ListCurveSegmentDiscountRatioVersionsRequest {
    key: CurveSegmentDiscountRatioKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentDiscountRatioVersionsFilter | null;
}

export interface ListCurveSegmentDiscountRatioVersionsResponse {
    result: Result;
    versions: CurveSegmentDiscountRatio[];
    total: number;
}

export interface GetCurveSegmentDiscountRatioVersionRequest {
    key: CurveSegmentDiscountRatioVersionKey;
}

export interface GetCurveSegmentDiscountRatioVersionResponse {
    result: Result;
    version: CurveSegmentDiscountRatio | null;
}

export const subjects = {
    list_curve_segment_discount_ratios_request: 'refdata.v1.curve_segment_discount_ratios.list',
    get_curve_segment_discount_ratio_request: 'refdata.v1.curve_segment_discount_ratios.get',
    get_many_curve_segment_discount_ratios_request:
        'refdata.v1.curve_segment_discount_ratios.get_many',
    put_curve_segment_discount_ratio_request: 'refdata.v1.curve_segment_discount_ratios.put',
    put_many_curve_segment_discount_ratios_request:
        'refdata.v1.curve_segment_discount_ratios.put_many',
    delete_curve_segment_discount_ratio_request: 'refdata.v1.curve_segment_discount_ratios.delete',
    delete_many_curve_segment_discount_ratios_request:
        'refdata.v1.curve_segment_discount_ratios.delete_many',
    list_curve_segment_discount_ratio_versions_request:
        'refdata.v1.curve_segment_discount_ratios_versions.list',
    get_curve_segment_discount_ratio_version_request:
        'refdata.v1.curve_segment_discount_ratios_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_segment_discount_ratios_request: true,
    get_curve_segment_discount_ratio_request: true,
    get_many_curve_segment_discount_ratios_request: true,
    put_curve_segment_discount_ratio_request: true,
    put_many_curve_segment_discount_ratios_request: true,
    delete_curve_segment_discount_ratio_request: true,
    delete_many_curve_segment_discount_ratios_request: true,
    list_curve_segment_discount_ratio_versions_request: true,
    get_curve_segment_discount_ratio_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.curve_segment_discount_ratios.created',
    updated: 'refdata.v1.events.curve_segment_discount_ratios.updated',
    deleted: 'refdata.v1.events.curve_segment_discount_ratios.deleted',
} as const;
