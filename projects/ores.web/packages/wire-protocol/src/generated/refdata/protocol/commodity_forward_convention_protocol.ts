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
import type { CommodityForwardConvention } from '../domain/commodity_forward_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CommodityForwardConventionKey {
    id: string;
}

export interface CommodityForwardConventionWrite {
    id: string;
    spot_days: number | null;
    points_factor: number | null;
    advance_calendar: string | null;
    spot_relative: boolean | null;
    delivery_location: string | null;
    business_day_convention: string | null;
    outright: boolean | null;
}

export interface CommodityForwardConventionChange {
    write: CommodityForwardConventionWrite;
    precondition: Precondition;
}

export interface CommodityForwardConventionRemoval {
    key: CommodityForwardConventionKey;
    precondition: Precondition;
}

export interface CommodityForwardConventionLookup {
    key: CommodityForwardConventionKey;
    commodity_forward_convention: CommodityForwardConvention | null;
}

export interface CommodityForwardConventionsFilter {
    id_one_of: string[] | null;
}

export interface CommodityForwardConventionEvent {
    event_id: string;
    key: CommodityForwardConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityForwardConventionVersionKey {
    commodity_forward_convention: CommodityForwardConventionKey;
    version: number;
}

export interface CommodityForwardConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityForwardConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityForwardConventionsFilter | null;
    as_of: string | null;
}

export interface ListCommodityForwardConventionsResponse {
    result: Result;
    commodity_forward_conventions: CommodityForwardConvention[];
    total: number;
}

export interface GetCommodityForwardConventionRequest {
    key: CommodityForwardConventionKey;
}

export interface GetCommodityForwardConventionResponse {
    result: Result;
    commodity_forward_convention: CommodityForwardConvention | null;
}

export interface GetManyCommodityForwardConventionsRequest {
    keys: CommodityForwardConventionKey[];
}

export interface GetManyCommodityForwardConventionsResponse {
    result: Result;
    entries: CommodityForwardConventionLookup[];
}

export interface PutCommodityForwardConventionRequest {
    change: CommodityForwardConventionChange;
    intent: ChangeIntent;
}

export interface PutCommodityForwardConventionResponse {
    result: Result;
    commodity_forward_convention: CommodityForwardConvention | null;
}

export interface PutManyCommodityForwardConventionsRequest {
    changes: CommodityForwardConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityForwardConventionsResponse {
    result: Result;
    commodity_forward_conventions: CommodityForwardConvention[];
}

export interface DeleteCommodityForwardConventionRequest {
    removal: CommodityForwardConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityForwardConventionResponse {
    result: Result;
}

export interface DeleteManyCommodityForwardConventionsRequest {
    removals: CommodityForwardConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityForwardConventionsResponse {
    result: Result;
}

export interface ListCommodityForwardConventionVersionsRequest {
    key: CommodityForwardConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityForwardConventionVersionsFilter | null;
}

export interface ListCommodityForwardConventionVersionsResponse {
    result: Result;
    versions: CommodityForwardConvention[];
    total: number;
}

export interface GetCommodityForwardConventionVersionRequest {
    key: CommodityForwardConventionVersionKey;
}

export interface GetCommodityForwardConventionVersionResponse {
    result: Result;
    version: CommodityForwardConvention | null;
}

export const subjects = {
    list_commodity_forward_conventions_request: 'refdata.v1.commodity_forward_conventions.list',
    get_commodity_forward_convention_request: 'refdata.v1.commodity_forward_conventions.get',
    get_many_commodity_forward_conventions_request:
        'refdata.v1.commodity_forward_conventions.get_many',
    put_commodity_forward_convention_request: 'refdata.v1.commodity_forward_conventions.put',
    put_many_commodity_forward_conventions_request:
        'refdata.v1.commodity_forward_conventions.put_many',
    delete_commodity_forward_convention_request: 'refdata.v1.commodity_forward_conventions.delete',
    delete_many_commodity_forward_conventions_request:
        'refdata.v1.commodity_forward_conventions.delete_many',
    list_commodity_forward_convention_versions_request:
        'refdata.v1.commodity_forward_conventions_versions.list',
    get_commodity_forward_convention_version_request:
        'refdata.v1.commodity_forward_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_forward_conventions_request: true,
    get_commodity_forward_convention_request: true,
    get_many_commodity_forward_conventions_request: true,
    put_commodity_forward_convention_request: true,
    put_many_commodity_forward_conventions_request: true,
    delete_commodity_forward_convention_request: true,
    delete_many_commodity_forward_conventions_request: true,
    list_commodity_forward_convention_versions_request: true,
    get_commodity_forward_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.commodity_forward_conventions_events.created',
    updated: 'refdata.v1.commodity_forward_conventions_events.updated',
    deleted: 'refdata.v1.commodity_forward_conventions_events.deleted',
} as const;
