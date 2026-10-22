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
import type { SourceKind } from '../domain/source_kind.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SourceKindKey {
    code: string;
}

export interface SourceKindWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface SourceKindChange {
    write: SourceKindWrite;
    precondition: Precondition;
}

export interface SourceKindRemoval {
    key: SourceKindKey;
    precondition: Precondition;
}

export interface SourceKindLookup {
    key: SourceKindKey;
    source_kind: SourceKind | null;
}

export interface SourceKindsFilter {
    code_one_of: string[] | null;
}

export interface SourceKindEvent {
    event_id: string;
    key: SourceKindKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SourceKindVersionKey {
    source_kind: SourceKindKey;
    version: number;
}

export interface SourceKindVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSourceKindsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SourceKindsFilter | null;
    as_of: string | null;
}

export interface ListSourceKindsResponse {
    result: Result;
    kinds: SourceKind[];
    total: number;
}

export interface GetSourceKindRequest {
    key: SourceKindKey;
}

export interface GetSourceKindResponse {
    result: Result;
    source_kind: SourceKind | null;
}

export interface GetManySourceKindsRequest {
    keys: SourceKindKey[];
}

export interface GetManySourceKindsResponse {
    result: Result;
    entries: SourceKindLookup[];
}

export interface PutSourceKindRequest {
    change: SourceKindChange;
    intent: ChangeIntent;
}

export interface PutSourceKindResponse {
    result: Result;
    source_kind: SourceKind | null;
}

export interface PutManySourceKindsRequest {
    changes: SourceKindChange[];
    intent: ChangeIntent;
}

export interface PutManySourceKindsResponse {
    result: Result;
    kinds: SourceKind[];
}

export interface DeleteSourceKindRequest {
    removal: SourceKindRemoval;
    intent: ChangeIntent;
}

export interface DeleteSourceKindResponse {
    result: Result;
}

export interface DeleteManySourceKindsRequest {
    removals: SourceKindRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySourceKindsResponse {
    result: Result;
}

export interface ListSourceKindVersionsRequest {
    key: SourceKindKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SourceKindVersionsFilter | null;
}

export interface ListSourceKindVersionsResponse {
    result: Result;
    versions: SourceKind[];
    total: number;
}

export interface GetSourceKindVersionRequest {
    key: SourceKindVersionKey;
}

export interface GetSourceKindVersionResponse {
    result: Result;
    version: SourceKind | null;
}

export const subjects = {
    list_source_kinds_request: 'refdata.v1.source_kinds.list',
    get_source_kind_request: 'refdata.v1.source_kinds.get',
    get_many_source_kinds_request: 'refdata.v1.source_kinds.get_many',
    put_source_kind_request: 'refdata.v1.source_kinds.put',
    put_many_source_kinds_request: 'refdata.v1.source_kinds.put_many',
    delete_source_kind_request: 'refdata.v1.source_kinds.delete',
    delete_many_source_kinds_request: 'refdata.v1.source_kinds.delete_many',
    list_source_kind_versions_request: 'refdata.v1.source_kinds_versions.list',
    get_source_kind_version_request: 'refdata.v1.source_kinds_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_source_kinds_request: true,
    get_source_kind_request: true,
    get_many_source_kinds_request: true,
    put_source_kind_request: true,
    put_many_source_kinds_request: true,
    delete_source_kind_request: true,
    delete_many_source_kinds_request: true,
    list_source_kind_versions_request: true,
    get_source_kind_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.source_kinds_events.created',
    updated: 'refdata.v1.source_kinds_events.updated',
    deleted: 'refdata.v1.source_kinds_events.deleted',
} as const;
