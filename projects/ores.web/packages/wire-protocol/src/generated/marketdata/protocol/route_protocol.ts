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
import type { Route } from '../domain/route.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RouteKey {
    publisher_code: string;
}

export interface RouteWrite {
    id: string;
    party_id: string;
    publisher_code: string;
    asset_class: string;
    feed_id: string;
}

export interface RouteChange {
    write: RouteWrite;
    precondition: Precondition;
}

export interface RouteRemoval {
    key: RouteKey;
    precondition: Precondition;
}

export interface RouteLookup {
    key: RouteKey;
    route: Route | null;
}

export interface RoutesFilter {
    party_id: string | null;
    publisher_code: string | null;
    asset_class: string | null;
    feed_id: string | null;
    id_one_of: string[] | null;
    party_id_one_of: string[] | null;
    publisher_code_one_of: string[] | null;
    asset_class_one_of: string[] | null;
    feed_id_one_of: string[] | null;
}

export interface RouteEvent {
    event_id: string;
    key: RouteKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface RouteVersionKey {
    route: RouteKey;
    version: number;
}

export interface RouteVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRoutesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RoutesFilter | null;
    as_of: string | null;
}

export interface ListRoutesResponse {
    result: Result;
    routes: Route[];
    total: number;
}

export interface GetRouteRequest {
    key: RouteKey;
}

export interface GetRouteResponse {
    result: Result;
    route: Route | null;
}

export interface GetManyRoutesRequest {
    keys: RouteKey[];
}

export interface GetManyRoutesResponse {
    result: Result;
    entries: RouteLookup[];
}

export interface PutRouteRequest {
    change: RouteChange;
    intent: ChangeIntent;
}

export interface PutRouteResponse {
    result: Result;
    route: Route | null;
}

export interface PutManyRoutesRequest {
    changes: RouteChange[];
    intent: ChangeIntent;
}

export interface PutManyRoutesResponse {
    result: Result;
    routes: Route[];
}

export interface DeleteRouteRequest {
    removal: RouteRemoval;
    intent: ChangeIntent;
}

export interface DeleteRouteResponse {
    result: Result;
}

export interface DeleteManyRoutesRequest {
    removals: RouteRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRoutesResponse {
    result: Result;
}

export interface ListRouteVersionsRequest {
    key: RouteKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RouteVersionsFilter | null;
}

export interface ListRouteVersionsResponse {
    result: Result;
    versions: Route[];
    total: number;
}

export interface GetRouteVersionRequest {
    key: RouteVersionKey;
}

export interface GetRouteVersionResponse {
    result: Result;
    version: Route | null;
}

export const subjects = {
    list_routes_request: 'marketdata.v1.routes.list',
    get_route_request: 'marketdata.v1.routes.get',
    get_many_routes_request: 'marketdata.v1.routes.get_many',
    put_route_request: 'marketdata.v1.routes.put',
    put_many_routes_request: 'marketdata.v1.routes.put_many',
    delete_route_request: 'marketdata.v1.routes.delete',
    delete_many_routes_request: 'marketdata.v1.routes.delete_many',
    list_route_versions_request: 'marketdata.v1.routes_versions.list',
    get_route_version_request: 'marketdata.v1.routes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_routes_request: true,
    get_route_request: true,
    get_many_routes_request: true,
    put_route_request: true,
    put_many_routes_request: true,
    delete_route_request: true,
    delete_many_routes_request: true,
    list_route_versions_request: true,
    get_route_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.routes_events.created',
    updated: 'marketdata.v1.routes_events.updated',
    deleted: 'marketdata.v1.routes_events.deleted',
} as const;
