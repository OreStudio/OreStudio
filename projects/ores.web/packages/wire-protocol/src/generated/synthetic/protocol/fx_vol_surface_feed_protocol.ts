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
import type { FxVolSurfaceFeed } from '../domain/fx_vol_surface_feed.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxVolSurfaceFeedKey {
    id: string;
}

export interface FxVolSurfaceFeedWrite {
    id: string;
    config_id: string;
    base_currency_code: string;
    quote_currency_code: string;
    source_name: string;
    ticks_per_hour: number;
    enabled: boolean;
    auto_start: boolean;
    folder_id: string | null;
    asset_class_code: string;
    arrival_mode: string;
    branching_ratio: number;
    decay: number;
    sensitivity: number;
    skew_reversion: number;
    skew_volatility: number;
    curvature_reversion: number;
    curvature_volatility: number;
    spot_variance_correlation: number;
    spot_skew_correlation: number;
    segment_correlation: number;
}

export interface FxVolSurfaceFeedChange {
    write: FxVolSurfaceFeedWrite;
    precondition: Precondition;
}

export interface FxVolSurfaceFeedRemoval {
    key: FxVolSurfaceFeedKey;
    precondition: Precondition;
}

export interface FxVolSurfaceFeedLookup {
    key: FxVolSurfaceFeedKey;
    fx_vol_surface_feed: FxVolSurfaceFeed | null;
}

export interface FxVolSurfaceFeedsFilter {
    id_one_of: string[] | null;
}

export interface FxVolSurfaceFeedEvent {
    event_id: string;
    key: FxVolSurfaceFeedKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxVolSurfaceFeedVersionKey {
    fx_vol_surface_feed: FxVolSurfaceFeedKey;
    version: number;
}

export interface FxVolSurfaceFeedVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxVolSurfaceFeedsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfaceFeedsFilter | null;
    as_of: string | null;
}

export interface ListFxVolSurfaceFeedsResponse {
    result: Result;
    fx_vol_surface_feeds: FxVolSurfaceFeed[];
    total: number;
}

export interface GetFxVolSurfaceFeedRequest {
    key: FxVolSurfaceFeedKey;
}

export interface GetFxVolSurfaceFeedResponse {
    result: Result;
    fx_vol_surface_feed: FxVolSurfaceFeed | null;
}

export interface GetManyFxVolSurfaceFeedsRequest {
    keys: FxVolSurfaceFeedKey[];
}

export interface GetManyFxVolSurfaceFeedsResponse {
    result: Result;
    entries: FxVolSurfaceFeedLookup[];
}

export interface PutFxVolSurfaceFeedRequest {
    change: FxVolSurfaceFeedChange;
    intent: ChangeIntent;
}

export interface PutFxVolSurfaceFeedResponse {
    result: Result;
    fx_vol_surface_feed: FxVolSurfaceFeed | null;
}

export interface PutManyFxVolSurfaceFeedsRequest {
    changes: FxVolSurfaceFeedChange[];
    intent: ChangeIntent;
}

export interface PutManyFxVolSurfaceFeedsResponse {
    result: Result;
    fx_vol_surface_feeds: FxVolSurfaceFeed[];
}

export interface DeleteFxVolSurfaceFeedRequest {
    removal: FxVolSurfaceFeedRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxVolSurfaceFeedResponse {
    result: Result;
}

export interface DeleteManyFxVolSurfaceFeedsRequest {
    removals: FxVolSurfaceFeedRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxVolSurfaceFeedsResponse {
    result: Result;
}

export interface ListFxVolSurfaceFeedVersionsRequest {
    key: FxVolSurfaceFeedKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfaceFeedVersionsFilter | null;
}

export interface ListFxVolSurfaceFeedVersionsResponse {
    result: Result;
    versions: FxVolSurfaceFeed[];
    total: number;
}

export interface GetFxVolSurfaceFeedVersionRequest {
    key: FxVolSurfaceFeedVersionKey;
}

export interface GetFxVolSurfaceFeedVersionResponse {
    result: Result;
    version: FxVolSurfaceFeed | null;
}

export const subjects = {
    list_fx_vol_surface_feeds_request: 'synthetic.v1.fx_vol_surface_feeds.list',
    get_fx_vol_surface_feed_request: 'synthetic.v1.fx_vol_surface_feeds.get',
    get_many_fx_vol_surface_feeds_request: 'synthetic.v1.fx_vol_surface_feeds.get_many',
    put_fx_vol_surface_feed_request: 'synthetic.v1.fx_vol_surface_feeds.put',
    put_many_fx_vol_surface_feeds_request: 'synthetic.v1.fx_vol_surface_feeds.put_many',
    delete_fx_vol_surface_feed_request: 'synthetic.v1.fx_vol_surface_feeds.delete',
    delete_many_fx_vol_surface_feeds_request: 'synthetic.v1.fx_vol_surface_feeds.delete_many',
    list_fx_vol_surface_feed_versions_request: 'synthetic.v1.fx_vol_surface_feeds_versions.list',
    get_fx_vol_surface_feed_version_request: 'synthetic.v1.fx_vol_surface_feeds_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_vol_surface_feeds_request: true,
    get_fx_vol_surface_feed_request: true,
    get_many_fx_vol_surface_feeds_request: true,
    put_fx_vol_surface_feed_request: true,
    put_many_fx_vol_surface_feeds_request: true,
    delete_fx_vol_surface_feed_request: true,
    delete_many_fx_vol_surface_feeds_request: true,
    list_fx_vol_surface_feed_versions_request: true,
    get_fx_vol_surface_feed_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.events.fx_vol_surface_feeds.created',
    updated: 'synthetic.v1.events.fx_vol_surface_feeds.updated',
    deleted: 'synthetic.v1.events.fx_vol_surface_feeds.deleted',
} as const;
