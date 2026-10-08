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
import type { StructureKind } from '../domain/structure_kind.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface StructureKindKey {
    code: string;
}

export interface StructureKindWrite {
    code: string;
    description: string;
    confirms_as_whole: boolean;
}

export interface StructureKindChange {
    write: StructureKindWrite;
    precondition: Precondition;
}

export interface StructureKindRemoval {
    key: StructureKindKey;
    precondition: Precondition;
}

export interface StructureKindLookup {
    key: StructureKindKey;
    structure_kind: StructureKind | null;
}

export interface StructureKindsFilter {
    code_one_of: string[] | null;
}

export interface StructureKindEvent {
    event_id: string;
    key: StructureKindKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface StructureKindVersionKey {
    structure_kind: StructureKindKey;
    version: number;
}

export interface StructureKindVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListStructureKindsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: StructureKindsFilter | null;
    as_of: string | null;
}

export interface ListStructureKindsResponse {
    result: Result;
    structure_kinds: StructureKind[];
    total: number;
}

export interface GetStructureKindRequest {
    key: StructureKindKey;
}

export interface GetStructureKindResponse {
    result: Result;
    structure_kind: StructureKind | null;
}

export interface GetManyStructureKindsRequest {
    keys: StructureKindKey[];
}

export interface GetManyStructureKindsResponse {
    result: Result;
    entries: StructureKindLookup[];
}

export interface PutStructureKindRequest {
    change: StructureKindChange;
    intent: ChangeIntent;
}

export interface PutStructureKindResponse {
    result: Result;
    structure_kind: StructureKind | null;
}

export interface PutManyStructureKindsRequest {
    changes: StructureKindChange[];
    intent: ChangeIntent;
}

export interface PutManyStructureKindsResponse {
    result: Result;
    structure_kinds: StructureKind[];
}

export interface DeleteStructureKindRequest {
    removal: StructureKindRemoval;
    intent: ChangeIntent;
}

export interface DeleteStructureKindResponse {
    result: Result;
}

export interface DeleteManyStructureKindsRequest {
    removals: StructureKindRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyStructureKindsResponse {
    result: Result;
}

export interface ListStructureKindVersionsRequest {
    key: StructureKindKey;
    offset: number;
    limit: number;
    order: Order;
    filter: StructureKindVersionsFilter | null;
}

export interface ListStructureKindVersionsResponse {
    result: Result;
    versions: StructureKind[];
    total: number;
}

export interface GetStructureKindVersionRequest {
    key: StructureKindVersionKey;
}

export interface GetStructureKindVersionResponse {
    result: Result;
    version: StructureKind | null;
}

export const subjects = {
    list_structure_kinds_request: 'trading.v1.structure_kinds.list',
    get_structure_kind_request: 'trading.v1.structure_kinds.get',
    get_many_structure_kinds_request: 'trading.v1.structure_kinds.get_many',
    put_structure_kind_request: 'trading.v1.structure_kinds.put',
    put_many_structure_kinds_request: 'trading.v1.structure_kinds.put_many',
    delete_structure_kind_request: 'trading.v1.structure_kinds.delete',
    delete_many_structure_kinds_request: 'trading.v1.structure_kinds.delete_many',
    list_structure_kind_versions_request: 'trading.v1.structure_kinds_versions.list',
    get_structure_kind_version_request: 'trading.v1.structure_kinds_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_structure_kinds_request: true,
    get_structure_kind_request: true,
    get_many_structure_kinds_request: true,
    put_structure_kind_request: true,
    put_many_structure_kinds_request: true,
    delete_structure_kind_request: true,
    delete_many_structure_kinds_request: true,
    list_structure_kind_versions_request: true,
    get_structure_kind_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.structure_kinds_events.created',
    updated: 'trading.v1.structure_kinds_events.updated',
    deleted: 'trading.v1.structure_kinds_events.deleted',
} as const;
