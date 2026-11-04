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
import type { CommodityCurveBasis } from '../domain/commodity_curve_basis.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CommodityCurveBasisKey {
    id: string;
}

export interface CommodityCurveBasisWrite {
    id: string;
    curve_definition_id: string;
    basis_base_price_curve: string | null;
    basis_base_price_conventions: string | null;
    basis_conventions: string | null;
    basis_day_counter: string | null;
    basis_interpolation_method: string | null;
    basis_add_basis: string | null;
    basis_month_offset: number | null;
    basis_average_base: string | null;
    basis_price_as_historical_fixing: string | null;
}

export interface CommodityCurveBasisChange {
    write: CommodityCurveBasisWrite;
    precondition: Precondition;
}

export interface CommodityCurveBasisRemoval {
    key: CommodityCurveBasisKey;
    precondition: Precondition;
}

export interface CommodityCurveBasisLookup {
    key: CommodityCurveBasisKey;
    commodity_curve_basis: CommodityCurveBasis | null;
}

export interface CommodityCurveBasesFilter {
    id_one_of: string[] | null;
}

export interface CommodityCurveBasisEvent {
    event_id: string;
    key: CommodityCurveBasisKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityCurveBasisVersionKey {
    commodity_curve_basis: CommodityCurveBasisKey;
    version: number;
}

export interface CommodityCurveBasisVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityCurveBasesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityCurveBasesFilter | null;
    as_of: string | null;
}

export interface ListCommodityCurveBasesResponse {
    result: Result;
    bases: CommodityCurveBasis[];
    total: number;
}

export interface GetCommodityCurveBasisRequest {
    key: CommodityCurveBasisKey;
}

export interface GetCommodityCurveBasisResponse {
    result: Result;
    commodity_curve_basis: CommodityCurveBasis | null;
}

export interface GetManyCommodityCurveBasesRequest {
    keys: CommodityCurveBasisKey[];
}

export interface GetManyCommodityCurveBasesResponse {
    result: Result;
    entries: CommodityCurveBasisLookup[];
}

export interface PutCommodityCurveBasisRequest {
    change: CommodityCurveBasisChange;
    intent: ChangeIntent;
}

export interface PutCommodityCurveBasisResponse {
    result: Result;
    commodity_curve_basis: CommodityCurveBasis | null;
}

export interface PutManyCommodityCurveBasesRequest {
    changes: CommodityCurveBasisChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityCurveBasesResponse {
    result: Result;
    bases: CommodityCurveBasis[];
}

export interface DeleteCommodityCurveBasisRequest {
    removal: CommodityCurveBasisRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityCurveBasisResponse {
    result: Result;
}

export interface DeleteManyCommodityCurveBasesRequest {
    removals: CommodityCurveBasisRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityCurveBasesResponse {
    result: Result;
}

export interface ListCommodityCurveBasisVersionsRequest {
    key: CommodityCurveBasisKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityCurveBasisVersionsFilter | null;
}

export interface ListCommodityCurveBasisVersionsResponse {
    result: Result;
    versions: CommodityCurveBasis[];
    total: number;
}

export interface GetCommodityCurveBasisVersionRequest {
    key: CommodityCurveBasisVersionKey;
}

export interface GetCommodityCurveBasisVersionResponse {
    result: Result;
    version: CommodityCurveBasis | null;
}

export const subjects = {
    list_commodity_curve_bases_request: 'refdata.v1.commodity_curve_bases.list',
    get_commodity_curve_basis_request: 'refdata.v1.commodity_curve_bases.get',
    get_many_commodity_curve_bases_request: 'refdata.v1.commodity_curve_bases.get_many',
    put_commodity_curve_basis_request: 'refdata.v1.commodity_curve_bases.put',
    put_many_commodity_curve_bases_request: 'refdata.v1.commodity_curve_bases.put_many',
    delete_commodity_curve_basis_request: 'refdata.v1.commodity_curve_bases.delete',
    delete_many_commodity_curve_bases_request: 'refdata.v1.commodity_curve_bases.delete_many',
    list_commodity_curve_basis_versions_request: 'refdata.v1.commodity_curve_bases_versions.list',
    get_commodity_curve_basis_version_request: 'refdata.v1.commodity_curve_bases_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_curve_bases_request: true,
    get_commodity_curve_basis_request: true,
    get_many_commodity_curve_bases_request: true,
    put_commodity_curve_basis_request: true,
    put_many_commodity_curve_bases_request: true,
    delete_commodity_curve_basis_request: true,
    delete_many_commodity_curve_bases_request: true,
    list_commodity_curve_basis_versions_request: true,
    get_commodity_curve_basis_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.commodity_curve_bases.created',
    updated: 'refdata.v1.events.commodity_curve_bases.updated',
    deleted: 'refdata.v1.events.commodity_curve_bases.deleted',
} as const;
