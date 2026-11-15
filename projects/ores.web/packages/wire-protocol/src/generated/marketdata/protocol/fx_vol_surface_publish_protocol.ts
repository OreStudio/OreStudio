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
import type { FxVolSurfacePublish } from '../domain/fx_vol_surface_publish.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface FxVolSurfacePublishKey {
    id: string;
}

export interface FxVolSurfacePublishLookup {
    key: FxVolSurfacePublishKey;
    fx_vol_surface_publish: FxVolSurfacePublish | null;
}

export interface FxVolSurfacePublishesFilter {
    request_id: string | null;
    party_id: string | null;
    blueprint_id: string | null;
    id_one_of: string[] | null;
    request_id_one_of: string[] | null;
    party_id_one_of: string[] | null;
    blueprint_id_one_of: string[] | null;
}

export interface FxVolSurfacePublishEvent {
    event_id: string;
    key: FxVolSurfacePublishKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxVolSurfacePublishVersionKey {
    fx_vol_surface_publish: FxVolSurfacePublishKey;
    version: number;
}

export interface FxVolSurfacePublishVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxVolSurfacePublishesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfacePublishesFilter | null;
    as_of: string | null;
}

export interface ListFxVolSurfacePublishesResponse {
    result: Result;
    publishes: FxVolSurfacePublish[];
    total: number;
}

export interface GetFxVolSurfacePublishRequest {
    key: FxVolSurfacePublishKey;
}

export interface GetFxVolSurfacePublishResponse {
    result: Result;
    fx_vol_surface_publish: FxVolSurfacePublish | null;
}

export interface GetManyFxVolSurfacePublishesRequest {
    keys: FxVolSurfacePublishKey[];
}

export interface GetManyFxVolSurfacePublishesResponse {
    result: Result;
    entries: FxVolSurfacePublishLookup[];
}

export interface ListByRequestIdFxVolSurfacePublishesRequest {
    request_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfacePublishesFilter | null;
}

export interface ListByRequestIdFxVolSurfacePublishesResponse {
    result: Result;
    publishes: FxVolSurfacePublish[];
    total: number;
}

export interface ListFxVolSurfacePublishVersionsRequest {
    key: FxVolSurfacePublishKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfacePublishVersionsFilter | null;
}

export interface ListFxVolSurfacePublishVersionsResponse {
    result: Result;
    versions: FxVolSurfacePublish[];
    total: number;
}

export interface GetFxVolSurfacePublishVersionRequest {
    key: FxVolSurfacePublishVersionKey;
}

export interface GetFxVolSurfacePublishVersionResponse {
    result: Result;
    version: FxVolSurfacePublish | null;
}

export const subjects = {
    list_fx_vol_surface_publishes_request: 'marketdata.v1.fx_vol_surface_publishes.list',
    get_fx_vol_surface_publish_request: 'marketdata.v1.fx_vol_surface_publishes.get',
    get_many_fx_vol_surface_publishes_request: 'marketdata.v1.fx_vol_surface_publishes.get_many',
    list_by_request_id_fx_vol_surface_publishes_request:
        'marketdata.v1.fx_vol_surface_publishes.list_by_request_id',
    list_fx_vol_surface_publish_versions_request:
        'marketdata.v1.fx_vol_surface_publishes_versions.list',
    get_fx_vol_surface_publish_version_request:
        'marketdata.v1.fx_vol_surface_publishes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_vol_surface_publishes_request: true,
    get_fx_vol_surface_publish_request: true,
    get_many_fx_vol_surface_publishes_request: true,
    list_by_request_id_fx_vol_surface_publishes_request: true,
    list_fx_vol_surface_publish_versions_request: true,
    get_fx_vol_surface_publish_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.events.fx_vol_surface_publishes.created',
    updated: 'marketdata.v1.events.fx_vol_surface_publishes.updated',
    deleted: 'marketdata.v1.events.fx_vol_surface_publishes.deleted',
} as const;
