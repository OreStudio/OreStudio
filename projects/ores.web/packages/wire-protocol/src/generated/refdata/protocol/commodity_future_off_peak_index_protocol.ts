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
import type { CommodityFutureOffPeakIndex } from '../domain/commodity_future_off_peak_index.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CommodityFutureOffPeakIndexKey {
    id: string;
}

export interface CommodityFutureOffPeakIndexWrite {
    id: string;
    convention_id: string;
    off_peak_index: string;
    peak_index: string;
    off_peak_hours: number;
    peak_calendar: string;
}

export interface CommodityFutureOffPeakIndexChange {
    write: CommodityFutureOffPeakIndexWrite;
    precondition: Precondition;
}

export interface CommodityFutureOffPeakIndexRemoval {
    key: CommodityFutureOffPeakIndexKey;
    precondition: Precondition;
}

export interface CommodityFutureOffPeakIndexLookup {
    key: CommodityFutureOffPeakIndexKey;
    commodity_future_off_peak_index: CommodityFutureOffPeakIndex | null;
}

export interface CommodityFutureOffPeakIndexesFilter {
    convention_id: string | null;
    id_one_of: string[] | null;
    convention_id_one_of: string[] | null;
}

export interface CommodityFutureOffPeakIndexEvent {
    event_id: string;
    key: CommodityFutureOffPeakIndexKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityFutureOffPeakIndexVersionKey {
    commodity_future_off_peak_index: CommodityFutureOffPeakIndexKey;
    version: number;
}

export interface CommodityFutureOffPeakIndexVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityFutureOffPeakIndexesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureOffPeakIndexesFilter | null;
    as_of: string | null;
}

export interface ListCommodityFutureOffPeakIndexesResponse {
    result: Result;
    commodity_future_off_peak_indexes: CommodityFutureOffPeakIndex[];
    total: number;
}

export interface GetCommodityFutureOffPeakIndexRequest {
    key: CommodityFutureOffPeakIndexKey;
}

export interface GetCommodityFutureOffPeakIndexResponse {
    result: Result;
    commodity_future_off_peak_index: CommodityFutureOffPeakIndex | null;
}

export interface GetManyCommodityFutureOffPeakIndexesRequest {
    keys: CommodityFutureOffPeakIndexKey[];
}

export interface GetManyCommodityFutureOffPeakIndexesResponse {
    result: Result;
    entries: CommodityFutureOffPeakIndexLookup[];
}

export interface PutCommodityFutureOffPeakIndexRequest {
    change: CommodityFutureOffPeakIndexChange;
    intent: ChangeIntent;
}

export interface PutCommodityFutureOffPeakIndexResponse {
    result: Result;
    commodity_future_off_peak_index: CommodityFutureOffPeakIndex | null;
}

export interface PutManyCommodityFutureOffPeakIndexesRequest {
    changes: CommodityFutureOffPeakIndexChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityFutureOffPeakIndexesResponse {
    result: Result;
    commodity_future_off_peak_indexes: CommodityFutureOffPeakIndex[];
}

export interface DeleteCommodityFutureOffPeakIndexRequest {
    removal: CommodityFutureOffPeakIndexRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityFutureOffPeakIndexResponse {
    result: Result;
}

export interface DeleteManyCommodityFutureOffPeakIndexesRequest {
    removals: CommodityFutureOffPeakIndexRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityFutureOffPeakIndexesResponse {
    result: Result;
}

export interface ListByConventionIdCommodityFutureOffPeakIndexesRequest {
    convention_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureOffPeakIndexesFilter | null;
}

export interface ListByConventionIdCommodityFutureOffPeakIndexesResponse {
    result: Result;
    commodity_future_off_peak_indexes: CommodityFutureOffPeakIndex[];
    total: number;
}

export interface ListCommodityFutureOffPeakIndexVersionsRequest {
    key: CommodityFutureOffPeakIndexKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureOffPeakIndexVersionsFilter | null;
}

export interface ListCommodityFutureOffPeakIndexVersionsResponse {
    result: Result;
    versions: CommodityFutureOffPeakIndex[];
    total: number;
}

export interface GetCommodityFutureOffPeakIndexVersionRequest {
    key: CommodityFutureOffPeakIndexVersionKey;
}

export interface GetCommodityFutureOffPeakIndexVersionResponse {
    result: Result;
    version: CommodityFutureOffPeakIndex | null;
}

export const subjects = {
    list_commodity_future_off_peak_indexes_request:
        'refdata.v1.commodity_future_off_peak_indexes.list',
    get_commodity_future_off_peak_index_request: 'refdata.v1.commodity_future_off_peak_indexes.get',
    get_many_commodity_future_off_peak_indexes_request:
        'refdata.v1.commodity_future_off_peak_indexes.get_many',
    put_commodity_future_off_peak_index_request: 'refdata.v1.commodity_future_off_peak_indexes.put',
    put_many_commodity_future_off_peak_indexes_request:
        'refdata.v1.commodity_future_off_peak_indexes.put_many',
    delete_commodity_future_off_peak_index_request:
        'refdata.v1.commodity_future_off_peak_indexes.delete',
    delete_many_commodity_future_off_peak_indexes_request:
        'refdata.v1.commodity_future_off_peak_indexes.delete_many',
    list_by_convention_id_commodity_future_off_peak_indexes_request:
        'refdata.v1.commodity_future_off_peak_indexes.list_by_convention_id',
    list_commodity_future_off_peak_index_versions_request:
        'refdata.v1.commodity_future_off_peak_indexes_versions.list',
    get_commodity_future_off_peak_index_version_request:
        'refdata.v1.commodity_future_off_peak_indexes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_future_off_peak_indexes_request: true,
    get_commodity_future_off_peak_index_request: true,
    get_many_commodity_future_off_peak_indexes_request: true,
    put_commodity_future_off_peak_index_request: true,
    put_many_commodity_future_off_peak_indexes_request: true,
    delete_commodity_future_off_peak_index_request: true,
    delete_many_commodity_future_off_peak_indexes_request: true,
    list_by_convention_id_commodity_future_off_peak_indexes_request: true,
    list_commodity_future_off_peak_index_versions_request: true,
    get_commodity_future_off_peak_index_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.commodity_future_off_peak_indexes_events.created',
    updated: 'refdata.v1.commodity_future_off_peak_indexes_events.updated',
    deleted: 'refdata.v1.commodity_future_off_peak_indexes_events.deleted',
} as const;
