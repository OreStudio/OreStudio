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
import type { CommodityPriceSegment } from '../domain/commodity_price_segment.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CommodityPriceSegmentKey {
    id: string;
}

export interface CommodityPriceSegmentWrite {
    id: string;
    curve_definition_id: string;
    segment_type: string;
    priority: number | null;
    conventions: string | null;
    has_quotes: boolean;
    peak_price_curve_id: string | null;
    peak_price_calendar: string | null;
    has_off_peak_daily: boolean;
    position: number;
}

export interface CommodityPriceSegmentChange {
    write: CommodityPriceSegmentWrite;
    precondition: Precondition;
}

export interface CommodityPriceSegmentRemoval {
    key: CommodityPriceSegmentKey;
    precondition: Precondition;
}

export interface CommodityPriceSegmentLookup {
    key: CommodityPriceSegmentKey;
    commodity_price_segment: CommodityPriceSegment | null;
}

export interface CommodityPriceSegmentEvent {
    event_id: string;
    key: CommodityPriceSegmentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityPriceSegmentVersionKey {
    commodity_price_segment: CommodityPriceSegmentKey;
    version: number;
}

export interface CommodityPriceSegmentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityPriceSegmentsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCommodityPriceSegmentsResponse {
    result: Result;
    price_segments: CommodityPriceSegment[];
    total: number;
}

export interface GetCommodityPriceSegmentRequest {
    key: CommodityPriceSegmentKey;
}

export interface GetCommodityPriceSegmentResponse {
    result: Result;
    commodity_price_segment: CommodityPriceSegment | null;
}

export interface GetManyCommodityPriceSegmentsRequest {
    keys: CommodityPriceSegmentKey[];
}

export interface GetManyCommodityPriceSegmentsResponse {
    result: Result;
    entries: CommodityPriceSegmentLookup[];
}

export interface PutCommodityPriceSegmentRequest {
    change: CommodityPriceSegmentChange;
    intent: ChangeIntent;
}

export interface PutCommodityPriceSegmentResponse {
    result: Result;
    commodity_price_segment: CommodityPriceSegment | null;
}

export interface PutManyCommodityPriceSegmentsRequest {
    changes: CommodityPriceSegmentChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityPriceSegmentsResponse {
    result: Result;
    price_segments: CommodityPriceSegment[];
}

export interface DeleteCommodityPriceSegmentRequest {
    removal: CommodityPriceSegmentRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityPriceSegmentResponse {
    result: Result;
}

export interface DeleteManyCommodityPriceSegmentsRequest {
    removals: CommodityPriceSegmentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityPriceSegmentsResponse {
    result: Result;
}

export interface ListCommodityPriceSegmentVersionsRequest {
    key: CommodityPriceSegmentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityPriceSegmentVersionsFilter | null;
}

export interface ListCommodityPriceSegmentVersionsResponse {
    result: Result;
    versions: CommodityPriceSegment[];
    total: number;
}

export interface GetCommodityPriceSegmentVersionRequest {
    key: CommodityPriceSegmentVersionKey;
}

export interface GetCommodityPriceSegmentVersionResponse {
    result: Result;
    version: CommodityPriceSegment | null;
}

export const subjects = {
    list_commodity_price_segments_request: 'refdata.v1.commodity_price_segments.list',
    get_commodity_price_segment_request: 'refdata.v1.commodity_price_segments.get',
    get_many_commodity_price_segments_request: 'refdata.v1.commodity_price_segments.get_many',
    put_commodity_price_segment_request: 'refdata.v1.commodity_price_segments.put',
    put_many_commodity_price_segments_request: 'refdata.v1.commodity_price_segments.put_many',
    delete_commodity_price_segment_request: 'refdata.v1.commodity_price_segments.delete',
    delete_many_commodity_price_segments_request: 'refdata.v1.commodity_price_segments.delete_many',
    list_commodity_price_segment_versions_request:
        'refdata.v1.commodity_price_segments_versions.list',
    get_commodity_price_segment_version_request: 'refdata.v1.commodity_price_segments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_price_segments_request: true,
    get_commodity_price_segment_request: true,
    get_many_commodity_price_segments_request: true,
    put_commodity_price_segment_request: true,
    put_many_commodity_price_segments_request: true,
    delete_commodity_price_segment_request: true,
    delete_many_commodity_price_segments_request: true,
    list_commodity_price_segment_versions_request: true,
    get_commodity_price_segment_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.commodity_price_segments_events.created',
    updated: 'refdata.v1.commodity_price_segments_events.updated',
    deleted: 'refdata.v1.commodity_price_segments_events.deleted',
} as const;
