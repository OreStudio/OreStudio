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
import type { Composite } from '../domain/composite.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CompositeKey {
    name: string;
}

export interface CompositeWrite {
    id: string;
    name: string;
    provider_code: string;
    oresmd_uri: string;
    rule_code: string;
    description: string;
}

export interface CompositeChange {
    write: CompositeWrite;
    precondition: Precondition;
}

export interface CompositeRemoval {
    key: CompositeKey;
    precondition: Precondition;
}

export interface CompositeLookup {
    key: CompositeKey;
    composite: Composite | null;
}

export interface CompositesFilter {
    provider_code: string | null;
    id_one_of: string[] | null;
    provider_code_one_of: string[] | null;
    search: string | null;
}

export interface CompositeEvent {
    event_id: string;
    key: CompositeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CompositeVersionKey {
    composite: CompositeKey;
    version: number;
}

export interface CompositeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCompositesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CompositesFilter | null;
    as_of: string | null;
}

export interface ListCompositesResponse {
    result: Result;
    composites: Composite[];
    total: number;
}

export interface GetCompositeRequest {
    key: CompositeKey;
}

export interface GetCompositeResponse {
    result: Result;
    composite: Composite | null;
}

export interface GetManyCompositesRequest {
    keys: CompositeKey[];
}

export interface GetManyCompositesResponse {
    result: Result;
    entries: CompositeLookup[];
}

export interface PutCompositeRequest {
    change: CompositeChange;
    intent: ChangeIntent;
}

export interface PutCompositeResponse {
    result: Result;
    composite: Composite | null;
}

export interface PutManyCompositesRequest {
    changes: CompositeChange[];
    intent: ChangeIntent;
}

export interface PutManyCompositesResponse {
    result: Result;
    composites: Composite[];
}

export interface DeleteCompositeRequest {
    removal: CompositeRemoval;
    intent: ChangeIntent;
}

export interface DeleteCompositeResponse {
    result: Result;
}

export interface DeleteManyCompositesRequest {
    removals: CompositeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCompositesResponse {
    result: Result;
}

export interface ListCompositeVersionsRequest {
    key: CompositeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CompositeVersionsFilter | null;
}

export interface ListCompositeVersionsResponse {
    result: Result;
    versions: Composite[];
    total: number;
}

export interface GetCompositeVersionRequest {
    key: CompositeVersionKey;
}

export interface GetCompositeVersionResponse {
    result: Result;
    version: Composite | null;
}

export const subjects = {
    list_composites_request: 'marketdata.v1.composites.list',
    get_composite_request: 'marketdata.v1.composites.get',
    get_many_composites_request: 'marketdata.v1.composites.get_many',
    put_composite_request: 'marketdata.v1.composites.put',
    put_many_composites_request: 'marketdata.v1.composites.put_many',
    delete_composite_request: 'marketdata.v1.composites.delete',
    delete_many_composites_request: 'marketdata.v1.composites.delete_many',
    list_composite_versions_request: 'marketdata.v1.composites_versions.list',
    get_composite_version_request: 'marketdata.v1.composites_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_composites_request: true,
    get_composite_request: true,
    get_many_composites_request: true,
    put_composite_request: true,
    put_many_composites_request: true,
    delete_composite_request: true,
    delete_many_composites_request: true,
    list_composite_versions_request: true,
    get_composite_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.composites_events.created',
    updated: 'marketdata.v1.composites_events.updated',
    deleted: 'marketdata.v1.composites_events.deleted',
} as const;
