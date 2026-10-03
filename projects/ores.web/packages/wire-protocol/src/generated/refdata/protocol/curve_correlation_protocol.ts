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
import type { CurveCorrelation } from '../domain/curve_correlation.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveCorrelationKey {
    id: string;
}

export interface CurveCorrelationWrite {
    id: string;
    curve_definition_id: string;
    correlation_type: string;
    index_1: string | null;
    index_2: string | null;
    conventions: string | null;
    swaption_volatility: string | null;
    discount_curve: string | null;
    currency: string | null;
    dimension: string | null;
    quote_type: string | null;
    extrapolation: string | null;
    day_counter: string | null;
    calendar: string | null;
    business_day_convention: string | null;
    option_tenors: string | null;
}

export interface CurveCorrelationChange {
    write: CurveCorrelationWrite;
    precondition: Precondition;
}

export interface CurveCorrelationRemoval {
    key: CurveCorrelationKey;
    precondition: Precondition;
}

export interface CurveCorrelationLookup {
    key: CurveCorrelationKey;
    curve_correlation: CurveCorrelation | null;
}

export interface CurveCorrelationEvent {
    event_id: string;
    key: CurveCorrelationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveCorrelationVersionKey {
    curve_correlation: CurveCorrelationKey;
    version: number;
}

export interface CurveCorrelationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveCorrelationsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurveCorrelationsResponse {
    result: Result;
    correlations: CurveCorrelation[];
    total: number;
}

export interface GetCurveCorrelationRequest {
    key: CurveCorrelationKey;
}

export interface GetCurveCorrelationResponse {
    result: Result;
    curve_correlation: CurveCorrelation | null;
}

export interface GetManyCurveCorrelationsRequest {
    keys: CurveCorrelationKey[];
}

export interface GetManyCurveCorrelationsResponse {
    result: Result;
    entries: CurveCorrelationLookup[];
}

export interface PutCurveCorrelationRequest {
    change: CurveCorrelationChange;
    intent: ChangeIntent;
}

export interface PutCurveCorrelationResponse {
    result: Result;
    curve_correlation: CurveCorrelation | null;
}

export interface PutManyCurveCorrelationsRequest {
    changes: CurveCorrelationChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveCorrelationsResponse {
    result: Result;
    correlations: CurveCorrelation[];
}

export interface DeleteCurveCorrelationRequest {
    removal: CurveCorrelationRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveCorrelationResponse {
    result: Result;
}

export interface DeleteManyCurveCorrelationsRequest {
    removals: CurveCorrelationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveCorrelationsResponse {
    result: Result;
}

export interface ListCurveCorrelationVersionsRequest {
    key: CurveCorrelationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveCorrelationVersionsFilter | null;
}

export interface ListCurveCorrelationVersionsResponse {
    result: Result;
    versions: CurveCorrelation[];
    total: number;
}

export interface GetCurveCorrelationVersionRequest {
    key: CurveCorrelationVersionKey;
}

export interface GetCurveCorrelationVersionResponse {
    result: Result;
    version: CurveCorrelation | null;
}

export const subjects = {
    list_curve_correlations_request: 'refdata.v1.curve_correlations.list',
    get_curve_correlation_request: 'refdata.v1.curve_correlations.get',
    get_many_curve_correlations_request: 'refdata.v1.curve_correlations.get_many',
    put_curve_correlation_request: 'refdata.v1.curve_correlations.put',
    put_many_curve_correlations_request: 'refdata.v1.curve_correlations.put_many',
    delete_curve_correlation_request: 'refdata.v1.curve_correlations.delete',
    delete_many_curve_correlations_request: 'refdata.v1.curve_correlations.delete_many',
    list_curve_correlation_versions_request: 'refdata.v1.curve_correlations_versions.list',
    get_curve_correlation_version_request: 'refdata.v1.curve_correlations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_correlations_request: true,
    get_curve_correlation_request: true,
    get_many_curve_correlations_request: true,
    put_curve_correlation_request: true,
    put_many_curve_correlations_request: true,
    delete_curve_correlation_request: true,
    delete_many_curve_correlations_request: true,
    list_curve_correlation_versions_request: true,
    get_curve_correlation_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_correlations_events.created',
    updated: 'refdata.v1.curve_correlations_events.updated',
    deleted: 'refdata.v1.curve_correlations_events.deleted',
} as const;
