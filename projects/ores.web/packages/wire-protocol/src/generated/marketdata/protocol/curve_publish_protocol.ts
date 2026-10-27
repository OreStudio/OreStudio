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
import type { CurvePublish } from '../domain/curve_publish.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CurvePublishKey {
    id: string;
}

export interface CurvePublishLookup {
    key: CurvePublishKey;
    curve_publish: CurvePublish | null;
}

export interface CurvePublishesFilter {
    request_id: string | null;
    party_id: string | null;
    bootstrap_config_id: string | null;
    id_one_of: string[] | null;
    request_id_one_of: string[] | null;
    party_id_one_of: string[] | null;
    bootstrap_config_id_one_of: string[] | null;
}

export interface CurvePublishEvent {
    event_id: string;
    key: CurvePublishKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurvePublishVersionKey {
    curve_publish: CurvePublishKey;
    version: number;
}

export interface CurvePublishVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurvePublishesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurvePublishesFilter | null;
    as_of: string | null;
}

export interface ListCurvePublishesResponse {
    result: Result;
    curve_publishes: CurvePublish[];
    total: number;
}

export interface GetCurvePublishRequest {
    key: CurvePublishKey;
}

export interface GetCurvePublishResponse {
    result: Result;
    curve_publish: CurvePublish | null;
}

export interface GetManyCurvePublishesRequest {
    keys: CurvePublishKey[];
}

export interface GetManyCurvePublishesResponse {
    result: Result;
    entries: CurvePublishLookup[];
}

export interface ListByRequestIdCurvePublishesRequest {
    request_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CurvePublishesFilter | null;
}

export interface ListByRequestIdCurvePublishesResponse {
    result: Result;
    curve_publishes: CurvePublish[];
    total: number;
}

export interface ListCurvePublishVersionsRequest {
    key: CurvePublishKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurvePublishVersionsFilter | null;
}

export interface ListCurvePublishVersionsResponse {
    result: Result;
    versions: CurvePublish[];
    total: number;
}

export interface GetCurvePublishVersionRequest {
    key: CurvePublishVersionKey;
}

export interface GetCurvePublishVersionResponse {
    result: Result;
    version: CurvePublish | null;
}

export const subjects = {
    list_curve_publishes_request: 'marketdata.v1.curve_publishes.list',
    get_curve_publish_request: 'marketdata.v1.curve_publishes.get',
    get_many_curve_publishes_request: 'marketdata.v1.curve_publishes.get_many',
    list_by_request_id_curve_publishes_request: 'marketdata.v1.curve_publishes.list_by_request_id',
    list_curve_publish_versions_request: 'marketdata.v1.curve_publishes_versions.list',
    get_curve_publish_version_request: 'marketdata.v1.curve_publishes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_publishes_request: true,
    get_curve_publish_request: true,
    get_many_curve_publishes_request: true,
    list_by_request_id_curve_publishes_request: true,
    list_curve_publish_versions_request: true,
    get_curve_publish_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.curve_publishes_events.created',
    updated: 'marketdata.v1.curve_publishes_events.updated',
    deleted: 'marketdata.v1.curve_publishes_events.deleted',
} as const;
