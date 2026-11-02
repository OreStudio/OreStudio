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
import type { CurveSegmentCrossCurrencyTerms } from '../domain/curve_segment_cross_currency_terms.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSegmentCrossCurrencyTermsKey {
    id: string;
}

export interface CurveSegmentCrossCurrencyTermsWrite {
    id: string;
    curve_segment_id: string;
    discount_curve: string;
    spot_rate: string;
    projection_curve_domestic: string | null;
    projection_curve_foreign: string | null;
}

export interface CurveSegmentCrossCurrencyTermsChange {
    write: CurveSegmentCrossCurrencyTermsWrite;
    precondition: Precondition;
}

export interface CurveSegmentCrossCurrencyTermsRemoval {
    key: CurveSegmentCrossCurrencyTermsKey;
    precondition: Precondition;
}

export interface CurveSegmentCrossCurrencyTermsLookup {
    key: CurveSegmentCrossCurrencyTermsKey;
    curve_segment_cross_currency_terms: CurveSegmentCrossCurrencyTerms | null;
}

export interface CurveSegmentCrossCurrencyTermsFilter {
    id_one_of: string[] | null;
}

export interface CurveSegmentCrossCurrencyTermsEvent {
    event_id: string;
    key: CurveSegmentCrossCurrencyTermsKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSegmentCrossCurrencyTermsVersionKey {
    curve_segment_cross_currency_terms: CurveSegmentCrossCurrencyTermsKey;
    version: number;
}

export interface CurveSegmentCrossCurrencyTermsVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSegmentCrossCurrencyTermsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentCrossCurrencyTermsFilter | null;
    as_of: string | null;
}

export interface ListCurveSegmentCrossCurrencyTermsResponse {
    result: Result;
    segment_cross_currency_terms: CurveSegmentCrossCurrencyTerms[];
    total: number;
}

export interface GetCurveSegmentCrossCurrencyTermsRequest {
    key: CurveSegmentCrossCurrencyTermsKey;
}

export interface GetCurveSegmentCrossCurrencyTermsResponse {
    result: Result;
    curve_segment_cross_currency_terms: CurveSegmentCrossCurrencyTerms | null;
}

export interface GetManyCurveSegmentCrossCurrencyTermsRequest {
    keys: CurveSegmentCrossCurrencyTermsKey[];
}

export interface GetManyCurveSegmentCrossCurrencyTermsResponse {
    result: Result;
    entries: CurveSegmentCrossCurrencyTermsLookup[];
}

export interface PutCurveSegmentCrossCurrencyTermsRequest {
    change: CurveSegmentCrossCurrencyTermsChange;
    intent: ChangeIntent;
}

export interface PutCurveSegmentCrossCurrencyTermsResponse {
    result: Result;
    curve_segment_cross_currency_terms: CurveSegmentCrossCurrencyTerms | null;
}

export interface PutManyCurveSegmentCrossCurrencyTermsRequest {
    changes: CurveSegmentCrossCurrencyTermsChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSegmentCrossCurrencyTermsResponse {
    result: Result;
    segment_cross_currency_terms: CurveSegmentCrossCurrencyTerms[];
}

export interface DeleteCurveSegmentCrossCurrencyTermsRequest {
    removal: CurveSegmentCrossCurrencyTermsRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSegmentCrossCurrencyTermsResponse {
    result: Result;
}

export interface DeleteManyCurveSegmentCrossCurrencyTermsRequest {
    removals: CurveSegmentCrossCurrencyTermsRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSegmentCrossCurrencyTermsResponse {
    result: Result;
}

export interface ListCurveSegmentCrossCurrencyTermsVersionsRequest {
    key: CurveSegmentCrossCurrencyTermsKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentCrossCurrencyTermsVersionsFilter | null;
}

export interface ListCurveSegmentCrossCurrencyTermsVersionsResponse {
    result: Result;
    versions: CurveSegmentCrossCurrencyTerms[];
    total: number;
}

export interface GetCurveSegmentCrossCurrencyTermsVersionRequest {
    key: CurveSegmentCrossCurrencyTermsVersionKey;
}

export interface GetCurveSegmentCrossCurrencyTermsVersionResponse {
    result: Result;
    version: CurveSegmentCrossCurrencyTerms | null;
}

export const subjects = {
    list_curve_segment_cross_currency_terms_request:
        'refdata.v1.curve_segment_cross_currency_terms.list',
    get_curve_segment_cross_currency_terms_request:
        'refdata.v1.curve_segment_cross_currency_terms.get',
    get_many_curve_segment_cross_currency_terms_request:
        'refdata.v1.curve_segment_cross_currency_terms.get_many',
    put_curve_segment_cross_currency_terms_request:
        'refdata.v1.curve_segment_cross_currency_terms.put',
    put_many_curve_segment_cross_currency_terms_request:
        'refdata.v1.curve_segment_cross_currency_terms.put_many',
    delete_curve_segment_cross_currency_terms_request:
        'refdata.v1.curve_segment_cross_currency_terms.delete',
    delete_many_curve_segment_cross_currency_terms_request:
        'refdata.v1.curve_segment_cross_currency_terms.delete_many',
    list_curve_segment_cross_currency_terms_versions_request:
        'refdata.v1.curve_segment_cross_currency_terms_versions.list',
    get_curve_segment_cross_currency_terms_version_request:
        'refdata.v1.curve_segment_cross_currency_terms_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_segment_cross_currency_terms_request: true,
    get_curve_segment_cross_currency_terms_request: true,
    get_many_curve_segment_cross_currency_terms_request: true,
    put_curve_segment_cross_currency_terms_request: true,
    put_many_curve_segment_cross_currency_terms_request: true,
    delete_curve_segment_cross_currency_terms_request: true,
    delete_many_curve_segment_cross_currency_terms_request: true,
    list_curve_segment_cross_currency_terms_versions_request: true,
    get_curve_segment_cross_currency_terms_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_segment_cross_currency_terms_events.created',
    updated: 'refdata.v1.curve_segment_cross_currency_terms_events.updated',
    deleted: 'refdata.v1.curve_segment_cross_currency_terms_events.deleted',
} as const;
