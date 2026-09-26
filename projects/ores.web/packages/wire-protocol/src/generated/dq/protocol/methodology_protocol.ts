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
import type { Methodology } from '../domain/methodology.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface MethodologyKey {
    name: string;
}

export interface MethodologyWrite {
    id: string;
    name: string;
    description: string;
    logic_reference: string;
    implementation_details: string;
}

export interface MethodologyChange {
    write: MethodologyWrite;
    precondition: Precondition;
}

export interface MethodologyRemoval {
    key: MethodologyKey;
    precondition: Precondition;
}

export interface MethodologyLookup {
    key: MethodologyKey;
    methodology: Methodology | null;
}

export interface MethodologyEvent {
    event_id: string;
    key: MethodologyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface MethodologyVersionKey {
    methodology: MethodologyKey;
    version: number;
}

export interface MethodologyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListMethodologiesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListMethodologiesResponse {
    result: Result;
    methodologies: Methodology[];
    total: number;
}

export interface GetMethodologyRequest {
    key: MethodologyKey;
}

export interface GetMethodologyResponse {
    result: Result;
    methodology: Methodology | null;
}

export interface GetManyMethodologiesRequest {
    keys: MethodologyKey[];
}

export interface GetManyMethodologiesResponse {
    result: Result;
    entries: MethodologyLookup[];
}

export interface PutMethodologyRequest {
    change: MethodologyChange;
    intent: ChangeIntent;
}

export interface PutMethodologyResponse {
    result: Result;
    methodology: Methodology;
}

export interface PutManyMethodologiesRequest {
    changes: MethodologyChange[];
    intent: ChangeIntent;
}

export interface PutManyMethodologiesResponse {
    result: Result;
    methodologies: Methodology[];
}

export interface DeleteMethodologyRequest {
    removal: MethodologyRemoval;
    intent: ChangeIntent;
}

export interface DeleteMethodologyResponse {
    result: Result;
}

export interface DeleteManyMethodologiesRequest {
    removals: MethodologyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyMethodologiesResponse {
    result: Result;
}

export interface ListMethodologyVersionsRequest {
    key: MethodologyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: MethodologyVersionsFilter | null;
}

export interface ListMethodologyVersionsResponse {
    result: Result;
    versions: Methodology[];
    total: number;
}

export interface GetMethodologyVersionRequest {
    key: MethodologyVersionKey;
}

export interface GetMethodologyVersionResponse {
    result: Result;
    version: Methodology;
}

export const subjects = {
    list_methodologies_request: "dq.v1.methodologies.list",
    get_methodology_request: "dq.v1.methodologies.get",
    get_many_methodologies_request: "dq.v1.methodologies.get_many",
    put_methodology_request: "dq.v1.methodologies.put",
    put_many_methodologies_request: "dq.v1.methodologies.put_many",
    delete_methodology_request: "dq.v1.methodologies.delete",
    delete_many_methodologies_request: "dq.v1.methodologies.delete_many",
    list_methodology_versions_request: "dq.v1.methodologies_versions.list",
    get_methodology_version_request: "dq.v1.methodologies_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_methodologies_request: true,
    get_methodology_request: true,
    get_many_methodologies_request: true,
    put_methodology_request: true,
    put_many_methodologies_request: true,
    delete_methodology_request: true,
    delete_many_methodologies_request: true,
    list_methodology_versions_request: true,
    get_methodology_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.methodologies_events.created",
    updated: "dq.v1.methodologies_events.updated",
    deleted: "dq.v1.methodologies_events.deleted",
} as const;
