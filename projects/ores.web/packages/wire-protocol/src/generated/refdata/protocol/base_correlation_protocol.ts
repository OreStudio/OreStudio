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
import type { BaseCorrelation } from '../domain/base_correlation.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BaseCorrelationKey {
    id: string;
}

export interface BaseCorrelationWrite {
    id: string;
    curve_definition_id: string;
    terms: string;
    detachment_points: string;
    settlement_days: number;
    calendar: string;
    business_day_convention: string;
    day_counter: string;
    extrapolate: string | null;
    quote_name: string | null;
    start_date: string | null;
    rule: string | null;
    adjust_for_losses: string | null;
    index_term: string | null;
    index_spread: string | null;
    currency: string | null;
    calibrate_constituents_to_index_spread: string | null;
    use_assumed_recovery: string | null;
}

export interface BaseCorrelationChange {
    write: BaseCorrelationWrite;
    precondition: Precondition;
}

export interface BaseCorrelationRemoval {
    key: BaseCorrelationKey;
    precondition: Precondition;
}

export interface BaseCorrelationLookup {
    key: BaseCorrelationKey;
    base_correlation: BaseCorrelation | null;
}

export interface BaseCorrelationEvent {
    event_id: string;
    key: BaseCorrelationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BaseCorrelationVersionKey {
    base_correlation: BaseCorrelationKey;
    version: number;
}

export interface BaseCorrelationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBaseCorrelationsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBaseCorrelationsResponse {
    result: Result;
    base_correlations: BaseCorrelation[];
    total: number;
}

export interface GetBaseCorrelationRequest {
    key: BaseCorrelationKey;
}

export interface GetBaseCorrelationResponse {
    result: Result;
    base_correlation: BaseCorrelation | null;
}

export interface GetManyBaseCorrelationsRequest {
    keys: BaseCorrelationKey[];
}

export interface GetManyBaseCorrelationsResponse {
    result: Result;
    entries: BaseCorrelationLookup[];
}

export interface PutBaseCorrelationRequest {
    change: BaseCorrelationChange;
    intent: ChangeIntent;
}

export interface PutBaseCorrelationResponse {
    result: Result;
    base_correlation: BaseCorrelation | null;
}

export interface PutManyBaseCorrelationsRequest {
    changes: BaseCorrelationChange[];
    intent: ChangeIntent;
}

export interface PutManyBaseCorrelationsResponse {
    result: Result;
    base_correlations: BaseCorrelation[];
}

export interface DeleteBaseCorrelationRequest {
    removal: BaseCorrelationRemoval;
    intent: ChangeIntent;
}

export interface DeleteBaseCorrelationResponse {
    result: Result;
}

export interface DeleteManyBaseCorrelationsRequest {
    removals: BaseCorrelationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBaseCorrelationsResponse {
    result: Result;
}

export interface ListBaseCorrelationVersionsRequest {
    key: BaseCorrelationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BaseCorrelationVersionsFilter | null;
}

export interface ListBaseCorrelationVersionsResponse {
    result: Result;
    versions: BaseCorrelation[];
    total: number;
}

export interface GetBaseCorrelationVersionRequest {
    key: BaseCorrelationVersionKey;
}

export interface GetBaseCorrelationVersionResponse {
    result: Result;
    version: BaseCorrelation | null;
}

export const subjects = {
    list_base_correlations_request: 'refdata.v1.base_correlations.list',
    get_base_correlation_request: 'refdata.v1.base_correlations.get',
    get_many_base_correlations_request: 'refdata.v1.base_correlations.get_many',
    put_base_correlation_request: 'refdata.v1.base_correlations.put',
    put_many_base_correlations_request: 'refdata.v1.base_correlations.put_many',
    delete_base_correlation_request: 'refdata.v1.base_correlations.delete',
    delete_many_base_correlations_request: 'refdata.v1.base_correlations.delete_many',
    list_base_correlation_versions_request: 'refdata.v1.base_correlations_versions.list',
    get_base_correlation_version_request: 'refdata.v1.base_correlations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_base_correlations_request: true,
    get_base_correlation_request: true,
    get_many_base_correlations_request: true,
    put_base_correlation_request: true,
    put_many_base_correlations_request: true,
    delete_base_correlation_request: true,
    delete_many_base_correlations_request: true,
    list_base_correlation_versions_request: true,
    get_base_correlation_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.base_correlations_events.created',
    updated: 'refdata.v1.base_correlations_events.updated',
    deleted: 'refdata.v1.base_correlations_events.deleted',
} as const;
