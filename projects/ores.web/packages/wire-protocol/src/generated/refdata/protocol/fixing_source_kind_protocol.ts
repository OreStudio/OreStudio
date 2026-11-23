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
import type { FixingSourceKind } from '../domain/fixing_source_kind.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FixingSourceKindKey {
    code: string;
}

export interface FixingSourceKindWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface FixingSourceKindChange {
    write: FixingSourceKindWrite;
    precondition: Precondition;
}

export interface FixingSourceKindRemoval {
    key: FixingSourceKindKey;
    precondition: Precondition;
}

export interface FixingSourceKindLookup {
    key: FixingSourceKindKey;
    fixing_source_kind: FixingSourceKind | null;
}

export interface FixingSourceKindsFilter {
    code_one_of: string[] | null;
}

export interface FixingSourceKindEvent {
    event_id: string;
    key: FixingSourceKindKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FixingSourceKindVersionKey {
    fixing_source_kind: FixingSourceKindKey;
    version: number;
}

export interface FixingSourceKindVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFixingSourceKindsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FixingSourceKindsFilter | null;
    as_of: string | null;
}

export interface ListFixingSourceKindsResponse {
    result: Result;
    kinds: FixingSourceKind[];
    total: number;
}

export interface GetFixingSourceKindRequest {
    key: FixingSourceKindKey;
}

export interface GetFixingSourceKindResponse {
    result: Result;
    fixing_source_kind: FixingSourceKind | null;
}

export interface GetManyFixingSourceKindsRequest {
    keys: FixingSourceKindKey[];
}

export interface GetManyFixingSourceKindsResponse {
    result: Result;
    entries: FixingSourceKindLookup[];
}

export interface PutFixingSourceKindRequest {
    change: FixingSourceKindChange;
    intent: ChangeIntent;
}

export interface PutFixingSourceKindResponse {
    result: Result;
    fixing_source_kind: FixingSourceKind | null;
}

export interface PutManyFixingSourceKindsRequest {
    changes: FixingSourceKindChange[];
    intent: ChangeIntent;
}

export interface PutManyFixingSourceKindsResponse {
    result: Result;
    kinds: FixingSourceKind[];
}

export interface DeleteFixingSourceKindRequest {
    removal: FixingSourceKindRemoval;
    intent: ChangeIntent;
}

export interface DeleteFixingSourceKindResponse {
    result: Result;
}

export interface DeleteManyFixingSourceKindsRequest {
    removals: FixingSourceKindRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFixingSourceKindsResponse {
    result: Result;
}

export interface ListFixingSourceKindVersionsRequest {
    key: FixingSourceKindKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FixingSourceKindVersionsFilter | null;
}

export interface ListFixingSourceKindVersionsResponse {
    result: Result;
    versions: FixingSourceKind[];
    total: number;
}

export interface GetFixingSourceKindVersionRequest {
    key: FixingSourceKindVersionKey;
}

export interface GetFixingSourceKindVersionResponse {
    result: Result;
    version: FixingSourceKind | null;
}

export const subjects = {
    list_fixing_source_kinds_request: 'refdata.v1.fixing_source_kinds.list',
    get_fixing_source_kind_request: 'refdata.v1.fixing_source_kinds.get',
    get_many_fixing_source_kinds_request: 'refdata.v1.fixing_source_kinds.get_many',
    put_fixing_source_kind_request: 'refdata.v1.fixing_source_kinds.put',
    put_many_fixing_source_kinds_request: 'refdata.v1.fixing_source_kinds.put_many',
    delete_fixing_source_kind_request: 'refdata.v1.fixing_source_kinds.delete',
    delete_many_fixing_source_kinds_request: 'refdata.v1.fixing_source_kinds.delete_many',
    list_fixing_source_kind_versions_request: 'refdata.v1.fixing_source_kinds_versions.list',
    get_fixing_source_kind_version_request: 'refdata.v1.fixing_source_kinds_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fixing_source_kinds_request: true,
    get_fixing_source_kind_request: true,
    get_many_fixing_source_kinds_request: true,
    put_fixing_source_kind_request: true,
    put_many_fixing_source_kinds_request: true,
    delete_fixing_source_kind_request: true,
    delete_many_fixing_source_kinds_request: true,
    list_fixing_source_kind_versions_request: true,
    get_fixing_source_kind_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.fixing_source_kinds.created',
    updated: 'refdata.v1.events.fixing_source_kinds.updated',
    deleted: 'refdata.v1.events.fixing_source_kinds.deleted',
} as const;
