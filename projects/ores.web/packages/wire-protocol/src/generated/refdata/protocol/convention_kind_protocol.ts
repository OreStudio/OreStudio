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
import type { ConventionKind } from '../domain/convention_kind.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ConventionKindKey {
    code: string;
}

export interface ConventionKindWrite {
    code: string;
    name: string;
    description: string;
    asset_class: string;
    ore_element: string | null;
    display_order: number;
}

export interface ConventionKindChange {
    write: ConventionKindWrite;
    precondition: Precondition;
}

export interface ConventionKindRemoval {
    key: ConventionKindKey;
    precondition: Precondition;
}

export interface ConventionKindLookup {
    key: ConventionKindKey;
    convention_kind: ConventionKind | null;
}

export interface ConventionKindsFilter {
    code_one_of: string[] | null;
}

export interface ConventionKindEvent {
    event_id: string;
    key: ConventionKindKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ConventionKindVersionKey {
    convention_kind: ConventionKindKey;
    version: number;
}

export interface ConventionKindVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListConventionKindsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ConventionKindsFilter | null;
    as_of: string | null;
}

export interface ListConventionKindsResponse {
    result: Result;
    kinds: ConventionKind[];
    total: number;
}

export interface GetConventionKindRequest {
    key: ConventionKindKey;
}

export interface GetConventionKindResponse {
    result: Result;
    convention_kind: ConventionKind | null;
}

export interface GetManyConventionKindsRequest {
    keys: ConventionKindKey[];
}

export interface GetManyConventionKindsResponse {
    result: Result;
    entries: ConventionKindLookup[];
}

export interface PutConventionKindRequest {
    change: ConventionKindChange;
    intent: ChangeIntent;
}

export interface PutConventionKindResponse {
    result: Result;
    convention_kind: ConventionKind | null;
}

export interface PutManyConventionKindsRequest {
    changes: ConventionKindChange[];
    intent: ChangeIntent;
}

export interface PutManyConventionKindsResponse {
    result: Result;
    kinds: ConventionKind[];
}

export interface DeleteConventionKindRequest {
    removal: ConventionKindRemoval;
    intent: ChangeIntent;
}

export interface DeleteConventionKindResponse {
    result: Result;
}

export interface DeleteManyConventionKindsRequest {
    removals: ConventionKindRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyConventionKindsResponse {
    result: Result;
}

export interface ListConventionKindVersionsRequest {
    key: ConventionKindKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ConventionKindVersionsFilter | null;
}

export interface ListConventionKindVersionsResponse {
    result: Result;
    versions: ConventionKind[];
    total: number;
}

export interface GetConventionKindVersionRequest {
    key: ConventionKindVersionKey;
}

export interface GetConventionKindVersionResponse {
    result: Result;
    version: ConventionKind | null;
}

export const subjects = {
    list_convention_kinds_request: 'refdata.v1.convention_kinds.list',
    get_convention_kind_request: 'refdata.v1.convention_kinds.get',
    get_many_convention_kinds_request: 'refdata.v1.convention_kinds.get_many',
    put_convention_kind_request: 'refdata.v1.convention_kinds.put',
    put_many_convention_kinds_request: 'refdata.v1.convention_kinds.put_many',
    delete_convention_kind_request: 'refdata.v1.convention_kinds.delete',
    delete_many_convention_kinds_request: 'refdata.v1.convention_kinds.delete_many',
    list_convention_kind_versions_request: 'refdata.v1.convention_kinds_versions.list',
    get_convention_kind_version_request: 'refdata.v1.convention_kinds_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_convention_kinds_request: true,
    get_convention_kind_request: true,
    get_many_convention_kinds_request: true,
    put_convention_kind_request: true,
    put_many_convention_kinds_request: true,
    delete_convention_kind_request: true,
    delete_many_convention_kinds_request: true,
    list_convention_kind_versions_request: true,
    get_convention_kind_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.convention_kinds_events.created',
    updated: 'refdata.v1.convention_kinds_events.updated',
    deleted: 'refdata.v1.convention_kinds_events.deleted',
} as const;
