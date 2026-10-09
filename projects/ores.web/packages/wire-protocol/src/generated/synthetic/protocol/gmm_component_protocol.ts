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
import type { GmmComponent } from '../domain/gmm_component.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface GmmComponentKey {
    id: string;
}

export interface GmmComponentWrite {
    id: string;
    party_id: string;
    fx_spot_config_id: string;
    component_index: number;
    description: string;
    mean: number;
    stdev: number;
    weight: number;
}

export interface GmmComponentChange {
    write: GmmComponentWrite;
    precondition: Precondition;
}

export interface GmmComponentRemoval {
    key: GmmComponentKey;
    precondition: Precondition;
}

export interface GmmComponentLookup {
    key: GmmComponentKey;
    gmm_component: GmmComponent | null;
}

export interface GmmComponentsFilter {
    id_one_of: string[] | null;
}

export interface GmmComponentEvent {
    event_id: string;
    key: GmmComponentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface GmmComponentVersionKey {
    gmm_component: GmmComponentKey;
    version: number;
}

export interface GmmComponentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListGmmComponentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: GmmComponentsFilter | null;
    as_of: string | null;
}

export interface ListGmmComponentsResponse {
    result: Result;
    gmm_components: GmmComponent[];
    total: number;
}

export interface GetGmmComponentRequest {
    key: GmmComponentKey;
}

export interface GetGmmComponentResponse {
    result: Result;
    gmm_component: GmmComponent | null;
}

export interface GetManyGmmComponentsRequest {
    keys: GmmComponentKey[];
}

export interface GetManyGmmComponentsResponse {
    result: Result;
    entries: GmmComponentLookup[];
}

export interface PutGmmComponentRequest {
    change: GmmComponentChange;
    intent: ChangeIntent;
}

export interface PutGmmComponentResponse {
    result: Result;
    gmm_component: GmmComponent | null;
}

export interface PutManyGmmComponentsRequest {
    changes: GmmComponentChange[];
    intent: ChangeIntent;
}

export interface PutManyGmmComponentsResponse {
    result: Result;
    gmm_components: GmmComponent[];
}

export interface DeleteGmmComponentRequest {
    removal: GmmComponentRemoval;
    intent: ChangeIntent;
}

export interface DeleteGmmComponentResponse {
    result: Result;
}

export interface DeleteManyGmmComponentsRequest {
    removals: GmmComponentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyGmmComponentsResponse {
    result: Result;
}

export interface ListGmmComponentVersionsRequest {
    key: GmmComponentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: GmmComponentVersionsFilter | null;
}

export interface ListGmmComponentVersionsResponse {
    result: Result;
    versions: GmmComponent[];
    total: number;
}

export interface GetGmmComponentVersionRequest {
    key: GmmComponentVersionKey;
}

export interface GetGmmComponentVersionResponse {
    result: Result;
    version: GmmComponent | null;
}

export const subjects = {
    list_gmm_components_request: 'synthetic.v1.gmm_components.list',
    get_gmm_component_request: 'synthetic.v1.gmm_components.get',
    get_many_gmm_components_request: 'synthetic.v1.gmm_components.get_many',
    put_gmm_component_request: 'synthetic.v1.gmm_components.put',
    put_many_gmm_components_request: 'synthetic.v1.gmm_components.put_many',
    delete_gmm_component_request: 'synthetic.v1.gmm_components.delete',
    delete_many_gmm_components_request: 'synthetic.v1.gmm_components.delete_many',
    list_gmm_component_versions_request: 'synthetic.v1.gmm_components_versions.list',
    get_gmm_component_version_request: 'synthetic.v1.gmm_components_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_gmm_components_request: true,
    get_gmm_component_request: true,
    get_many_gmm_components_request: true,
    put_gmm_component_request: true,
    put_many_gmm_components_request: true,
    delete_gmm_component_request: true,
    delete_many_gmm_components_request: true,
    list_gmm_component_versions_request: true,
    get_gmm_component_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.gmm_components_events.created',
    updated: 'synthetic.v1.gmm_components_events.updated',
    deleted: 'synthetic.v1.gmm_components_events.deleted',
} as const;
