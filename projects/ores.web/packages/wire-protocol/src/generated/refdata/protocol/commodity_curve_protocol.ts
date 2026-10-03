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
import type { CommodityCurve } from '../domain/commodity_curve.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CommodityCurveKey {
    id: string;
}

export interface CommodityCurveWrite {
    id: string;
    curve_definition_id: string;
    currency: string;
    base_price_curve: string | null;
    base_yield_curve: string | null;
    yield_curve: string | null;
    spot_quote: string | null;
    has_quotes: boolean;
    day_counter: string | null;
    interpolation_method: string | null;
    conventions: string | null;
    extrapolation: string | null;
    has_basis_configuration: boolean;
    basis_base_price_curve: string | null;
    basis_base_price_conventions: string | null;
    basis_conventions: string | null;
    basis_day_counter: string | null;
    basis_interpolation_method: string | null;
    basis_add_basis: string | null;
    basis_month_offset: number | null;
    basis_average_base: string | null;
    basis_price_as_historical_fixing: string | null;
    has_price_segments: boolean;
}

export interface CommodityCurveChange {
    write: CommodityCurveWrite;
    precondition: Precondition;
}

export interface CommodityCurveRemoval {
    key: CommodityCurveKey;
    precondition: Precondition;
}

export interface CommodityCurveLookup {
    key: CommodityCurveKey;
    commodity_curve: CommodityCurve | null;
}

export interface CommodityCurveEvent {
    event_id: string;
    key: CommodityCurveKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityCurveVersionKey {
    commodity_curve: CommodityCurveKey;
    version: number;
}

export interface CommodityCurveVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityCurvesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCommodityCurvesResponse {
    result: Result;
    commodity_curves: CommodityCurve[];
    total: number;
}

export interface GetCommodityCurveRequest {
    key: CommodityCurveKey;
}

export interface GetCommodityCurveResponse {
    result: Result;
    commodity_curve: CommodityCurve | null;
}

export interface GetManyCommodityCurvesRequest {
    keys: CommodityCurveKey[];
}

export interface GetManyCommodityCurvesResponse {
    result: Result;
    entries: CommodityCurveLookup[];
}

export interface PutCommodityCurveRequest {
    change: CommodityCurveChange;
    intent: ChangeIntent;
}

export interface PutCommodityCurveResponse {
    result: Result;
    commodity_curve: CommodityCurve | null;
}

export interface PutManyCommodityCurvesRequest {
    changes: CommodityCurveChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityCurvesResponse {
    result: Result;
    commodity_curves: CommodityCurve[];
}

export interface DeleteCommodityCurveRequest {
    removal: CommodityCurveRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityCurveResponse {
    result: Result;
}

export interface DeleteManyCommodityCurvesRequest {
    removals: CommodityCurveRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityCurvesResponse {
    result: Result;
}

export interface ListCommodityCurveVersionsRequest {
    key: CommodityCurveKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityCurveVersionsFilter | null;
}

export interface ListCommodityCurveVersionsResponse {
    result: Result;
    versions: CommodityCurve[];
    total: number;
}

export interface GetCommodityCurveVersionRequest {
    key: CommodityCurveVersionKey;
}

export interface GetCommodityCurveVersionResponse {
    result: Result;
    version: CommodityCurve | null;
}

export const subjects = {
    list_commodity_curves_request: 'refdata.v1.commodity_curves.list',
    get_commodity_curve_request: 'refdata.v1.commodity_curves.get',
    get_many_commodity_curves_request: 'refdata.v1.commodity_curves.get_many',
    put_commodity_curve_request: 'refdata.v1.commodity_curves.put',
    put_many_commodity_curves_request: 'refdata.v1.commodity_curves.put_many',
    delete_commodity_curve_request: 'refdata.v1.commodity_curves.delete',
    delete_many_commodity_curves_request: 'refdata.v1.commodity_curves.delete_many',
    list_commodity_curve_versions_request: 'refdata.v1.commodity_curves_versions.list',
    get_commodity_curve_version_request: 'refdata.v1.commodity_curves_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_curves_request: true,
    get_commodity_curve_request: true,
    get_many_commodity_curves_request: true,
    put_commodity_curve_request: true,
    put_many_commodity_curves_request: true,
    delete_commodity_curve_request: true,
    delete_many_commodity_curves_request: true,
    list_commodity_curve_versions_request: true,
    get_commodity_curve_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.commodity_curves_events.created',
    updated: 'refdata.v1.commodity_curves_events.updated',
    deleted: 'refdata.v1.commodity_curves_events.deleted',
} as const;
