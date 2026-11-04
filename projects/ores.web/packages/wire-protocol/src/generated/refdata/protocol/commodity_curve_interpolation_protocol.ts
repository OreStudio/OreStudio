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
import type { CommodityCurveInterpolation } from '../domain/commodity_curve_interpolation.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CommodityCurveInterpolationKey {
    id: string;
}

export interface CommodityCurveInterpolationWrite {
    id: string;
    curve_definition_id: string;
    day_counter: string | null;
    interpolation_method: string | null;
}

export interface CommodityCurveInterpolationChange {
    write: CommodityCurveInterpolationWrite;
    precondition: Precondition;
}

export interface CommodityCurveInterpolationRemoval {
    key: CommodityCurveInterpolationKey;
    precondition: Precondition;
}

export interface CommodityCurveInterpolationLookup {
    key: CommodityCurveInterpolationKey;
    commodity_curve_interpolation: CommodityCurveInterpolation | null;
}

export interface CommodityCurveInterpolationsFilter {
    id_one_of: string[] | null;
}

export interface CommodityCurveInterpolationEvent {
    event_id: string;
    key: CommodityCurveInterpolationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityCurveInterpolationVersionKey {
    commodity_curve_interpolation: CommodityCurveInterpolationKey;
    version: number;
}

export interface CommodityCurveInterpolationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityCurveInterpolationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityCurveInterpolationsFilter | null;
    as_of: string | null;
}

export interface ListCommodityCurveInterpolationsResponse {
    result: Result;
    interpolations: CommodityCurveInterpolation[];
    total: number;
}

export interface GetCommodityCurveInterpolationRequest {
    key: CommodityCurveInterpolationKey;
}

export interface GetCommodityCurveInterpolationResponse {
    result: Result;
    commodity_curve_interpolation: CommodityCurveInterpolation | null;
}

export interface GetManyCommodityCurveInterpolationsRequest {
    keys: CommodityCurveInterpolationKey[];
}

export interface GetManyCommodityCurveInterpolationsResponse {
    result: Result;
    entries: CommodityCurveInterpolationLookup[];
}

export interface PutCommodityCurveInterpolationRequest {
    change: CommodityCurveInterpolationChange;
    intent: ChangeIntent;
}

export interface PutCommodityCurveInterpolationResponse {
    result: Result;
    commodity_curve_interpolation: CommodityCurveInterpolation | null;
}

export interface PutManyCommodityCurveInterpolationsRequest {
    changes: CommodityCurveInterpolationChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityCurveInterpolationsResponse {
    result: Result;
    interpolations: CommodityCurveInterpolation[];
}

export interface DeleteCommodityCurveInterpolationRequest {
    removal: CommodityCurveInterpolationRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityCurveInterpolationResponse {
    result: Result;
}

export interface DeleteManyCommodityCurveInterpolationsRequest {
    removals: CommodityCurveInterpolationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityCurveInterpolationsResponse {
    result: Result;
}

export interface ListCommodityCurveInterpolationVersionsRequest {
    key: CommodityCurveInterpolationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityCurveInterpolationVersionsFilter | null;
}

export interface ListCommodityCurveInterpolationVersionsResponse {
    result: Result;
    versions: CommodityCurveInterpolation[];
    total: number;
}

export interface GetCommodityCurveInterpolationVersionRequest {
    key: CommodityCurveInterpolationVersionKey;
}

export interface GetCommodityCurveInterpolationVersionResponse {
    result: Result;
    version: CommodityCurveInterpolation | null;
}

export const subjects = {
    list_commodity_curve_interpolations_request: 'refdata.v1.commodity_curve_interpolations.list',
    get_commodity_curve_interpolation_request: 'refdata.v1.commodity_curve_interpolations.get',
    get_many_commodity_curve_interpolations_request:
        'refdata.v1.commodity_curve_interpolations.get_many',
    put_commodity_curve_interpolation_request: 'refdata.v1.commodity_curve_interpolations.put',
    put_many_commodity_curve_interpolations_request:
        'refdata.v1.commodity_curve_interpolations.put_many',
    delete_commodity_curve_interpolation_request:
        'refdata.v1.commodity_curve_interpolations.delete',
    delete_many_commodity_curve_interpolations_request:
        'refdata.v1.commodity_curve_interpolations.delete_many',
    list_commodity_curve_interpolation_versions_request:
        'refdata.v1.commodity_curve_interpolations_versions.list',
    get_commodity_curve_interpolation_version_request:
        'refdata.v1.commodity_curve_interpolations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_curve_interpolations_request: true,
    get_commodity_curve_interpolation_request: true,
    get_many_commodity_curve_interpolations_request: true,
    put_commodity_curve_interpolation_request: true,
    put_many_commodity_curve_interpolations_request: true,
    delete_commodity_curve_interpolation_request: true,
    delete_many_commodity_curve_interpolations_request: true,
    list_commodity_curve_interpolation_versions_request: true,
    get_commodity_curve_interpolation_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.commodity_curve_interpolations.created',
    updated: 'refdata.v1.events.commodity_curve_interpolations.updated',
    deleted: 'refdata.v1.events.commodity_curve_interpolations.deleted',
} as const;
