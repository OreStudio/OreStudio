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
import type { FxForwardPublishSpot } from '../domain/fx_forward_publish_spot.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxForwardPublishSpotKey {
    id: string;
}

export interface FxForwardPublishSpotLookup {
    key: FxForwardPublishSpotKey;
    fx_forward_publish_spot: FxForwardPublishSpot | null;
}

export interface FxForwardPublishSpotsFilter {
    publish_id: string | null;
    id_one_of: string[] | null;
    publish_id_one_of: string[] | null;
}

export interface FxForwardPublishSpotEvent {
    event_id: string;
    key: FxForwardPublishSpotKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxForwardPublishSpotVersionKey {
    fx_forward_publish_spot: FxForwardPublishSpotKey;
    version: number;
}

export interface FxForwardPublishSpotVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxForwardPublishSpotsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxForwardPublishSpotsFilter | null;
    as_of: string | null;
}

export interface ListFxForwardPublishSpotsResponse {
    result: Result;
    spots: FxForwardPublishSpot[];
    total: number;
}

export interface GetFxForwardPublishSpotRequest {
    key: FxForwardPublishSpotKey;
}

export interface GetFxForwardPublishSpotResponse {
    result: Result;
    fx_forward_publish_spot: FxForwardPublishSpot | null;
}

export interface GetManyFxForwardPublishSpotsRequest {
    keys: FxForwardPublishSpotKey[];
}

export interface GetManyFxForwardPublishSpotsResponse {
    result: Result;
    entries: FxForwardPublishSpotLookup[];
}

export interface ListFxForwardPublishSpotVersionsRequest {
    key: FxForwardPublishSpotKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxForwardPublishSpotVersionsFilter | null;
}

export interface ListFxForwardPublishSpotVersionsResponse {
    result: Result;
    versions: FxForwardPublishSpot[];
    total: number;
}

export interface GetFxForwardPublishSpotVersionRequest {
    key: FxForwardPublishSpotVersionKey;
}

export interface GetFxForwardPublishSpotVersionResponse {
    result: Result;
    version: FxForwardPublishSpot | null;
}

export const subjects = {
    list_fx_forward_publish_spots_request: 'marketdata.v1.fx_forward_publish_spots.list',
    get_fx_forward_publish_spot_request: 'marketdata.v1.fx_forward_publish_spots.get',
    get_many_fx_forward_publish_spots_request: 'marketdata.v1.fx_forward_publish_spots.get_many',
    list_fx_forward_publish_spot_versions_request:
        'marketdata.v1.fx_forward_publish_spots_versions.list',
    get_fx_forward_publish_spot_version_request:
        'marketdata.v1.fx_forward_publish_spots_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_forward_publish_spots_request: true,
    get_fx_forward_publish_spot_request: true,
    get_many_fx_forward_publish_spots_request: true,
    list_fx_forward_publish_spot_versions_request: true,
    get_fx_forward_publish_spot_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.events.fx_forward_publish_spots.created',
    updated: 'marketdata.v1.events.fx_forward_publish_spots.updated',
    deleted: 'marketdata.v1.events.fx_forward_publish_spots.deleted',
} as const;
