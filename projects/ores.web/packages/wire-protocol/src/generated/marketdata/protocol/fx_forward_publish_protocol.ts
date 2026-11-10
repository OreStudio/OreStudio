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
import type { FxForwardPublish } from '../domain/fx_forward_publish.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface FxForwardPublishKey {
    id: string;
}

export interface FxForwardPublishLookup {
    key: FxForwardPublishKey;
    fx_forward_publish: FxForwardPublish | null;
}

export interface FxForwardPublishesFilter {
    request_id: string | null;
    party_id: string | null;
    pair_code: string | null;
    id_one_of: string[] | null;
    request_id_one_of: string[] | null;
    party_id_one_of: string[] | null;
    pair_code_one_of: string[] | null;
}

export interface FxForwardPublishEvent {
    event_id: string;
    key: FxForwardPublishKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxForwardPublishVersionKey {
    fx_forward_publish: FxForwardPublishKey;
    version: number;
}

export interface FxForwardPublishVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxForwardPublishesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxForwardPublishesFilter | null;
    as_of: string | null;
}

export interface ListFxForwardPublishesResponse {
    result: Result;
    publishes: FxForwardPublish[];
    total: number;
}

export interface GetFxForwardPublishRequest {
    key: FxForwardPublishKey;
}

export interface GetFxForwardPublishResponse {
    result: Result;
    fx_forward_publish: FxForwardPublish | null;
}

export interface GetManyFxForwardPublishesRequest {
    keys: FxForwardPublishKey[];
}

export interface GetManyFxForwardPublishesResponse {
    result: Result;
    entries: FxForwardPublishLookup[];
}

export interface ListByRequestIdFxForwardPublishesRequest {
    request_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: FxForwardPublishesFilter | null;
}

export interface ListByRequestIdFxForwardPublishesResponse {
    result: Result;
    publishes: FxForwardPublish[];
    total: number;
}

export interface ListFxForwardPublishVersionsRequest {
    key: FxForwardPublishKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxForwardPublishVersionsFilter | null;
}

export interface ListFxForwardPublishVersionsResponse {
    result: Result;
    versions: FxForwardPublish[];
    total: number;
}

export interface GetFxForwardPublishVersionRequest {
    key: FxForwardPublishVersionKey;
}

export interface GetFxForwardPublishVersionResponse {
    result: Result;
    version: FxForwardPublish | null;
}

export const subjects = {
    list_fx_forward_publishes_request: 'marketdata.v1.fx_forward_publishes.list',
    get_fx_forward_publish_request: 'marketdata.v1.fx_forward_publishes.get',
    get_many_fx_forward_publishes_request: 'marketdata.v1.fx_forward_publishes.get_many',
    list_by_request_id_fx_forward_publishes_request:
        'marketdata.v1.fx_forward_publishes.list_by_request_id',
    list_fx_forward_publish_versions_request: 'marketdata.v1.fx_forward_publishes_versions.list',
    get_fx_forward_publish_version_request: 'marketdata.v1.fx_forward_publishes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_forward_publishes_request: true,
    get_fx_forward_publish_request: true,
    get_many_fx_forward_publishes_request: true,
    list_by_request_id_fx_forward_publishes_request: true,
    list_fx_forward_publish_versions_request: true,
    get_fx_forward_publish_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.events.fx_forward_publishes.created',
    updated: 'marketdata.v1.events.fx_forward_publishes.updated',
    deleted: 'marketdata.v1.events.fx_forward_publishes.deleted',
} as const;
