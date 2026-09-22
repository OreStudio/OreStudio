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
import type { FloatingIndexType } from '../domain/floating_index_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FloatingIndexTypeKey {
    code: string;
}

export interface FloatingIndexTypeWrite {
    code: string;
    description: string;
}

export interface FloatingIndexTypeChange {
    write: FloatingIndexTypeWrite;
    precondition: Precondition;
}

export interface FloatingIndexTypeRemoval {
    key: FloatingIndexTypeKey;
    precondition: Precondition;
}

export interface FloatingIndexTypeLookup {
    key: FloatingIndexTypeKey;
    floating_index_type: FloatingIndexType | null;
}

export interface FloatingIndexTypeEvent {
    event_id: string;
    key: FloatingIndexTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FloatingIndexTypeVersionKey {
    floating_index_type: FloatingIndexTypeKey;
    version: number;
}

export interface FloatingIndexTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFloatingIndexTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListFloatingIndexTypesResponse {
    result: Result;
    types: FloatingIndexType[];
    total: number;
}

export interface GetFloatingIndexTypeRequest {
    key: FloatingIndexTypeKey;
}

export interface GetFloatingIndexTypeResponse {
    result: Result;
    floating_index_type: FloatingIndexType | null;
}

export interface GetManyFloatingIndexTypesRequest {
    keys: FloatingIndexTypeKey[];
}

export interface GetManyFloatingIndexTypesResponse {
    result: Result;
    entries: FloatingIndexTypeLookup[];
}

export interface PutFloatingIndexTypeRequest {
    change: FloatingIndexTypeChange;
    intent: ChangeIntent;
}

export interface PutFloatingIndexTypeResponse {
    result: Result;
    floating_index_type: FloatingIndexType;
}

export interface PutManyFloatingIndexTypesRequest {
    changes: FloatingIndexTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyFloatingIndexTypesResponse {
    result: Result;
    types: FloatingIndexType[];
}

export interface DeleteFloatingIndexTypeRequest {
    removal: FloatingIndexTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteFloatingIndexTypeResponse {
    result: Result;
}

export interface DeleteManyFloatingIndexTypesRequest {
    removals: FloatingIndexTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFloatingIndexTypesResponse {
    result: Result;
}

export interface ListFloatingIndexTypeVersionsRequest {
    key: FloatingIndexTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FloatingIndexTypeVersionsFilter | null;
}

export interface ListFloatingIndexTypeVersionsResponse {
    result: Result;
    versions: FloatingIndexType[];
    total: number;
}

export interface GetFloatingIndexTypeVersionRequest {
    key: FloatingIndexTypeVersionKey;
}

export interface GetFloatingIndexTypeVersionResponse {
    result: Result;
    version: FloatingIndexType;
}

export const subjects = {
    list_floating_index_types_request: "refdata.v1.floating_index_types.list",
    get_floating_index_type_request: "refdata.v1.floating_index_types.get",
    get_many_floating_index_types_request: "refdata.v1.floating_index_types.get_many",
    put_floating_index_type_request: "refdata.v1.floating_index_types.put",
    put_many_floating_index_types_request: "refdata.v1.floating_index_types.put_many",
    delete_floating_index_type_request: "refdata.v1.floating_index_types.delete",
    delete_many_floating_index_types_request: "refdata.v1.floating_index_types.delete_many",
    list_floating_index_type_versions_request: "refdata.v1.floating_index_types_versions.list",
    get_floating_index_type_version_request: "refdata.v1.floating_index_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_floating_index_types_request: true,
    get_floating_index_type_request: true,
    get_many_floating_index_types_request: true,
    put_floating_index_type_request: true,
    put_many_floating_index_types_request: true,
    delete_floating_index_type_request: true,
    delete_many_floating_index_types_request: true,
    list_floating_index_type_versions_request: true,
    get_floating_index_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.floating_index_types_events.created",
    updated: "refdata.v1.floating_index_types_events.updated",
    deleted: "refdata.v1.floating_index_types_events.deleted",
} as const;
