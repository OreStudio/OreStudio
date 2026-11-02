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
import type { CommodityCurveDirect } from '../domain/commodity_curve_direct.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CommodityCurveDirectKey {
    id: string;
}

export interface CommodityCurveDirectWrite {
    id: string;
    curve_definition_id: string;
    spot_quote: string | null;
    conventions: string | null;
    has_quotes: boolean;
}

export interface CommodityCurveDirectChange {
    write: CommodityCurveDirectWrite;
    precondition: Precondition;
}

export interface CommodityCurveDirectRemoval {
    key: CommodityCurveDirectKey;
    precondition: Precondition;
}

export interface CommodityCurveDirectLookup {
    key: CommodityCurveDirectKey;
    commodity_curve_direct: CommodityCurveDirect | null;
}

export interface CommodityCurveDirectsFilter {
    id_one_of: string[] | null;
}

export interface CommodityCurveDirectEvent {
    event_id: string;
    key: CommodityCurveDirectKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityCurveDirectVersionKey {
    commodity_curve_direct: CommodityCurveDirectKey;
    version: number;
}

export interface CommodityCurveDirectVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityCurveDirectsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityCurveDirectsFilter | null;
    as_of: string | null;
}

export interface ListCommodityCurveDirectsResponse {
    result: Result;
    directs: CommodityCurveDirect[];
    total: number;
}

export interface GetCommodityCurveDirectRequest {
    key: CommodityCurveDirectKey;
}

export interface GetCommodityCurveDirectResponse {
    result: Result;
    commodity_curve_direct: CommodityCurveDirect | null;
}

export interface GetManyCommodityCurveDirectsRequest {
    keys: CommodityCurveDirectKey[];
}

export interface GetManyCommodityCurveDirectsResponse {
    result: Result;
    entries: CommodityCurveDirectLookup[];
}

export interface PutCommodityCurveDirectRequest {
    change: CommodityCurveDirectChange;
    intent: ChangeIntent;
}

export interface PutCommodityCurveDirectResponse {
    result: Result;
    commodity_curve_direct: CommodityCurveDirect | null;
}

export interface PutManyCommodityCurveDirectsRequest {
    changes: CommodityCurveDirectChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityCurveDirectsResponse {
    result: Result;
    directs: CommodityCurveDirect[];
}

export interface DeleteCommodityCurveDirectRequest {
    removal: CommodityCurveDirectRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityCurveDirectResponse {
    result: Result;
}

export interface DeleteManyCommodityCurveDirectsRequest {
    removals: CommodityCurveDirectRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityCurveDirectsResponse {
    result: Result;
}

export interface ListCommodityCurveDirectVersionsRequest {
    key: CommodityCurveDirectKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityCurveDirectVersionsFilter | null;
}

export interface ListCommodityCurveDirectVersionsResponse {
    result: Result;
    versions: CommodityCurveDirect[];
    total: number;
}

export interface GetCommodityCurveDirectVersionRequest {
    key: CommodityCurveDirectVersionKey;
}

export interface GetCommodityCurveDirectVersionResponse {
    result: Result;
    version: CommodityCurveDirect | null;
}

export const subjects = {
    list_commodity_curve_directs_request: 'refdata.v1.commodity_curve_directs.list',
    get_commodity_curve_direct_request: 'refdata.v1.commodity_curve_directs.get',
    get_many_commodity_curve_directs_request: 'refdata.v1.commodity_curve_directs.get_many',
    put_commodity_curve_direct_request: 'refdata.v1.commodity_curve_directs.put',
    put_many_commodity_curve_directs_request: 'refdata.v1.commodity_curve_directs.put_many',
    delete_commodity_curve_direct_request: 'refdata.v1.commodity_curve_directs.delete',
    delete_many_commodity_curve_directs_request: 'refdata.v1.commodity_curve_directs.delete_many',
    list_commodity_curve_direct_versions_request:
        'refdata.v1.commodity_curve_directs_versions.list',
    get_commodity_curve_direct_version_request: 'refdata.v1.commodity_curve_directs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_curve_directs_request: true,
    get_commodity_curve_direct_request: true,
    get_many_commodity_curve_directs_request: true,
    put_commodity_curve_direct_request: true,
    put_many_commodity_curve_directs_request: true,
    delete_commodity_curve_direct_request: true,
    delete_many_commodity_curve_directs_request: true,
    list_commodity_curve_direct_versions_request: true,
    get_commodity_curve_direct_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.commodity_curve_directs_events.created',
    updated: 'refdata.v1.commodity_curve_directs_events.updated',
    deleted: 'refdata.v1.commodity_curve_directs_events.deleted',
} as const;
