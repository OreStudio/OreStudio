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
import type { FxVolSurfaceFeedExpiry } from '../domain/fx_vol_surface_feed_expiry.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxVolSurfaceFeedExpiryKey {
    id: string;
}

export interface FxVolSurfaceFeedExpiryWrite {
    id: string;
    feed_id: string;
    tenor: string;
    atm_vol: number;
    variance_reversion: number;
    variance_volatility: number;
}

export interface FxVolSurfaceFeedExpiryChange {
    write: FxVolSurfaceFeedExpiryWrite;
    precondition: Precondition;
}

export interface FxVolSurfaceFeedExpiryRemoval {
    key: FxVolSurfaceFeedExpiryKey;
    precondition: Precondition;
}

export interface FxVolSurfaceFeedExpiryLookup {
    key: FxVolSurfaceFeedExpiryKey;
    fx_vol_surface_feed_expiry: FxVolSurfaceFeedExpiry | null;
}

export interface FxVolSurfaceFeedExpiriesFilter {
    feed_id: string | null;
    id_one_of: string[] | null;
    feed_id_one_of: string[] | null;
}

export interface FxVolSurfaceFeedExpiryEvent {
    event_id: string;
    key: FxVolSurfaceFeedExpiryKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxVolSurfaceFeedExpiryVersionKey {
    fx_vol_surface_feed_expiry: FxVolSurfaceFeedExpiryKey;
    version: number;
}

export interface FxVolSurfaceFeedExpiryVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxVolSurfaceFeedExpiriesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfaceFeedExpiriesFilter | null;
    as_of: string | null;
}

export interface ListFxVolSurfaceFeedExpiriesResponse {
    result: Result;
    fx_vol_surface_feed_expiries: FxVolSurfaceFeedExpiry[];
    total: number;
}

export interface GetFxVolSurfaceFeedExpiryRequest {
    key: FxVolSurfaceFeedExpiryKey;
}

export interface GetFxVolSurfaceFeedExpiryResponse {
    result: Result;
    fx_vol_surface_feed_expiry: FxVolSurfaceFeedExpiry | null;
}

export interface GetManyFxVolSurfaceFeedExpiriesRequest {
    keys: FxVolSurfaceFeedExpiryKey[];
}

export interface GetManyFxVolSurfaceFeedExpiriesResponse {
    result: Result;
    entries: FxVolSurfaceFeedExpiryLookup[];
}

export interface PutFxVolSurfaceFeedExpiryRequest {
    change: FxVolSurfaceFeedExpiryChange;
    intent: ChangeIntent;
}

export interface PutFxVolSurfaceFeedExpiryResponse {
    result: Result;
    fx_vol_surface_feed_expiry: FxVolSurfaceFeedExpiry | null;
}

export interface PutManyFxVolSurfaceFeedExpiriesRequest {
    changes: FxVolSurfaceFeedExpiryChange[];
    intent: ChangeIntent;
}

export interface PutManyFxVolSurfaceFeedExpiriesResponse {
    result: Result;
    fx_vol_surface_feed_expiries: FxVolSurfaceFeedExpiry[];
}

export interface DeleteFxVolSurfaceFeedExpiryRequest {
    removal: FxVolSurfaceFeedExpiryRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxVolSurfaceFeedExpiryResponse {
    result: Result;
}

export interface DeleteManyFxVolSurfaceFeedExpiriesRequest {
    removals: FxVolSurfaceFeedExpiryRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxVolSurfaceFeedExpiriesResponse {
    result: Result;
}

export interface ListFxVolSurfaceFeedExpiryVersionsRequest {
    key: FxVolSurfaceFeedExpiryKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfaceFeedExpiryVersionsFilter | null;
}

export interface ListFxVolSurfaceFeedExpiryVersionsResponse {
    result: Result;
    versions: FxVolSurfaceFeedExpiry[];
    total: number;
}

export interface GetFxVolSurfaceFeedExpiryVersionRequest {
    key: FxVolSurfaceFeedExpiryVersionKey;
}

export interface GetFxVolSurfaceFeedExpiryVersionResponse {
    result: Result;
    version: FxVolSurfaceFeedExpiry | null;
}

export const subjects = {
    list_fx_vol_surface_feed_expiries_request: 'synthetic.v1.fx_vol_surface_feed_expiries.list',
    get_fx_vol_surface_feed_expiry_request: 'synthetic.v1.fx_vol_surface_feed_expiries.get',
    get_many_fx_vol_surface_feed_expiries_request:
        'synthetic.v1.fx_vol_surface_feed_expiries.get_many',
    put_fx_vol_surface_feed_expiry_request: 'synthetic.v1.fx_vol_surface_feed_expiries.put',
    put_many_fx_vol_surface_feed_expiries_request:
        'synthetic.v1.fx_vol_surface_feed_expiries.put_many',
    delete_fx_vol_surface_feed_expiry_request: 'synthetic.v1.fx_vol_surface_feed_expiries.delete',
    delete_many_fx_vol_surface_feed_expiries_request:
        'synthetic.v1.fx_vol_surface_feed_expiries.delete_many',
    list_fx_vol_surface_feed_expiry_versions_request:
        'synthetic.v1.fx_vol_surface_feed_expiries_versions.list',
    get_fx_vol_surface_feed_expiry_version_request:
        'synthetic.v1.fx_vol_surface_feed_expiries_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_vol_surface_feed_expiries_request: true,
    get_fx_vol_surface_feed_expiry_request: true,
    get_many_fx_vol_surface_feed_expiries_request: true,
    put_fx_vol_surface_feed_expiry_request: true,
    put_many_fx_vol_surface_feed_expiries_request: true,
    delete_fx_vol_surface_feed_expiry_request: true,
    delete_many_fx_vol_surface_feed_expiries_request: true,
    list_fx_vol_surface_feed_expiry_versions_request: true,
    get_fx_vol_surface_feed_expiry_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.events.fx_vol_surface_feed_expiries.created',
    updated: 'synthetic.v1.events.fx_vol_surface_feed_expiries.updated',
    deleted: 'synthetic.v1.events.fx_vol_surface_feed_expiries.deleted',
} as const;
