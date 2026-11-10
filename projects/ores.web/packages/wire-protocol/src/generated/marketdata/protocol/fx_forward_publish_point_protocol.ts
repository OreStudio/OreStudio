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
import type { FxForwardPublishPoint } from '../domain/fx_forward_publish_point.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxForwardPublishPointKey {
    id: string;
}

export interface FxForwardPublishPointLookup {
    key: FxForwardPublishPointKey;
    fx_forward_publish_point: FxForwardPublishPoint | null;
}

export interface FxForwardPublishPointsFilter {
    publish_id: string | null;
    id_one_of: string[] | null;
    publish_id_one_of: string[] | null;
}

export interface FxForwardPublishPointEvent {
    event_id: string;
    key: FxForwardPublishPointKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxForwardPublishPointVersionKey {
    fx_forward_publish_point: FxForwardPublishPointKey;
    version: number;
}

export interface FxForwardPublishPointVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxForwardPublishPointsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxForwardPublishPointsFilter | null;
    as_of: string | null;
}

export interface ListFxForwardPublishPointsResponse {
    result: Result;
    points: FxForwardPublishPoint[];
    total: number;
}

export interface GetFxForwardPublishPointRequest {
    key: FxForwardPublishPointKey;
}

export interface GetFxForwardPublishPointResponse {
    result: Result;
    fx_forward_publish_point: FxForwardPublishPoint | null;
}

export interface GetManyFxForwardPublishPointsRequest {
    keys: FxForwardPublishPointKey[];
}

export interface GetManyFxForwardPublishPointsResponse {
    result: Result;
    entries: FxForwardPublishPointLookup[];
}

export interface ListFxForwardPublishPointVersionsRequest {
    key: FxForwardPublishPointKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxForwardPublishPointVersionsFilter | null;
}

export interface ListFxForwardPublishPointVersionsResponse {
    result: Result;
    versions: FxForwardPublishPoint[];
    total: number;
}

export interface GetFxForwardPublishPointVersionRequest {
    key: FxForwardPublishPointVersionKey;
}

export interface GetFxForwardPublishPointVersionResponse {
    result: Result;
    version: FxForwardPublishPoint | null;
}

export const subjects = {
    list_fx_forward_publish_points_request: 'marketdata.v1.fx_forward_publish_points.list',
    get_fx_forward_publish_point_request: 'marketdata.v1.fx_forward_publish_points.get',
    get_many_fx_forward_publish_points_request: 'marketdata.v1.fx_forward_publish_points.get_many',
    list_fx_forward_publish_point_versions_request:
        'marketdata.v1.fx_forward_publish_points_versions.list',
    get_fx_forward_publish_point_version_request:
        'marketdata.v1.fx_forward_publish_points_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_forward_publish_points_request: true,
    get_fx_forward_publish_point_request: true,
    get_many_fx_forward_publish_points_request: true,
    list_fx_forward_publish_point_versions_request: true,
    get_fx_forward_publish_point_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.events.fx_forward_publish_points.created',
    updated: 'marketdata.v1.events.fx_forward_publish_points.updated',
    deleted: 'marketdata.v1.events.fx_forward_publish_points.deleted',
} as const;
