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
import type { CommodityFutureExpiryAnchor } from '../domain/commodity_future_expiry_anchor.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CommodityFutureExpiryAnchorKey {
    id: string;
}

export interface CommodityFutureExpiryAnchorWrite {
    id: string;
    convention_id: string;
    anchor_day_of_month: number | null;
    anchor_calendar_days_before: number | null;
    anchor_business_days_after: number | null;
    anchor_nth_nth: number | null;
    anchor_nth_weekday: string | null;
    anchor_last_weekday: string | null;
    anchor_weekly_day_of_the_week: string | null;
}

export interface CommodityFutureExpiryAnchorChange {
    write: CommodityFutureExpiryAnchorWrite;
    precondition: Precondition;
}

export interface CommodityFutureExpiryAnchorRemoval {
    key: CommodityFutureExpiryAnchorKey;
    precondition: Precondition;
}

export interface CommodityFutureExpiryAnchorLookup {
    key: CommodityFutureExpiryAnchorKey;
    commodity_future_expiry_anchor: CommodityFutureExpiryAnchor | null;
}

export interface CommodityFutureExpiryAnchorsFilter {
    convention_id: string | null;
    id_one_of: string[] | null;
    convention_id_one_of: string[] | null;
}

export interface CommodityFutureExpiryAnchorEvent {
    event_id: string;
    key: CommodityFutureExpiryAnchorKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityFutureExpiryAnchorVersionKey {
    commodity_future_expiry_anchor: CommodityFutureExpiryAnchorKey;
    version: number;
}

export interface CommodityFutureExpiryAnchorVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityFutureExpiryAnchorsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureExpiryAnchorsFilter | null;
    as_of: string | null;
}

export interface ListCommodityFutureExpiryAnchorsResponse {
    result: Result;
    commodity_future_expiry_anchors: CommodityFutureExpiryAnchor[];
    total: number;
}

export interface GetCommodityFutureExpiryAnchorRequest {
    key: CommodityFutureExpiryAnchorKey;
}

export interface GetCommodityFutureExpiryAnchorResponse {
    result: Result;
    commodity_future_expiry_anchor: CommodityFutureExpiryAnchor | null;
}

export interface GetManyCommodityFutureExpiryAnchorsRequest {
    keys: CommodityFutureExpiryAnchorKey[];
}

export interface GetManyCommodityFutureExpiryAnchorsResponse {
    result: Result;
    entries: CommodityFutureExpiryAnchorLookup[];
}

export interface PutCommodityFutureExpiryAnchorRequest {
    change: CommodityFutureExpiryAnchorChange;
    intent: ChangeIntent;
}

export interface PutCommodityFutureExpiryAnchorResponse {
    result: Result;
    commodity_future_expiry_anchor: CommodityFutureExpiryAnchor | null;
}

export interface PutManyCommodityFutureExpiryAnchorsRequest {
    changes: CommodityFutureExpiryAnchorChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityFutureExpiryAnchorsResponse {
    result: Result;
    commodity_future_expiry_anchors: CommodityFutureExpiryAnchor[];
}

export interface DeleteCommodityFutureExpiryAnchorRequest {
    removal: CommodityFutureExpiryAnchorRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityFutureExpiryAnchorResponse {
    result: Result;
}

export interface DeleteManyCommodityFutureExpiryAnchorsRequest {
    removals: CommodityFutureExpiryAnchorRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityFutureExpiryAnchorsResponse {
    result: Result;
}

export interface ListByConventionIdCommodityFutureExpiryAnchorsRequest {
    convention_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureExpiryAnchorsFilter | null;
}

export interface ListByConventionIdCommodityFutureExpiryAnchorsResponse {
    result: Result;
    commodity_future_expiry_anchors: CommodityFutureExpiryAnchor[];
    total: number;
}

export interface ListCommodityFutureExpiryAnchorVersionsRequest {
    key: CommodityFutureExpiryAnchorKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureExpiryAnchorVersionsFilter | null;
}

export interface ListCommodityFutureExpiryAnchorVersionsResponse {
    result: Result;
    versions: CommodityFutureExpiryAnchor[];
    total: number;
}

export interface GetCommodityFutureExpiryAnchorVersionRequest {
    key: CommodityFutureExpiryAnchorVersionKey;
}

export interface GetCommodityFutureExpiryAnchorVersionResponse {
    result: Result;
    version: CommodityFutureExpiryAnchor | null;
}

export const subjects = {
    list_commodity_future_expiry_anchors_request: 'refdata.v1.commodity_future_expiry_anchors.list',
    get_commodity_future_expiry_anchor_request: 'refdata.v1.commodity_future_expiry_anchors.get',
    get_many_commodity_future_expiry_anchors_request:
        'refdata.v1.commodity_future_expiry_anchors.get_many',
    put_commodity_future_expiry_anchor_request: 'refdata.v1.commodity_future_expiry_anchors.put',
    put_many_commodity_future_expiry_anchors_request:
        'refdata.v1.commodity_future_expiry_anchors.put_many',
    delete_commodity_future_expiry_anchor_request:
        'refdata.v1.commodity_future_expiry_anchors.delete',
    delete_many_commodity_future_expiry_anchors_request:
        'refdata.v1.commodity_future_expiry_anchors.delete_many',
    list_by_convention_id_commodity_future_expiry_anchors_request:
        'refdata.v1.commodity_future_expiry_anchors.list_by_convention_id',
    list_commodity_future_expiry_anchor_versions_request:
        'refdata.v1.commodity_future_expiry_anchors_versions.list',
    get_commodity_future_expiry_anchor_version_request:
        'refdata.v1.commodity_future_expiry_anchors_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_future_expiry_anchors_request: true,
    get_commodity_future_expiry_anchor_request: true,
    get_many_commodity_future_expiry_anchors_request: true,
    put_commodity_future_expiry_anchor_request: true,
    put_many_commodity_future_expiry_anchors_request: true,
    delete_commodity_future_expiry_anchor_request: true,
    delete_many_commodity_future_expiry_anchors_request: true,
    list_by_convention_id_commodity_future_expiry_anchors_request: true,
    list_commodity_future_expiry_anchor_versions_request: true,
    get_commodity_future_expiry_anchor_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.commodity_future_expiry_anchors_events.created',
    updated: 'refdata.v1.commodity_future_expiry_anchors_events.updated',
    deleted: 'refdata.v1.commodity_future_expiry_anchors_events.deleted',
} as const;
