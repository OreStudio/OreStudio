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
import type { TenorKind } from '../domain/tenor_kind.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TenorKindKey {
    code: string;
}

export interface TenorKindWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface TenorKindChange {
    write: TenorKindWrite;
    precondition: Precondition;
}

export interface TenorKindRemoval {
    key: TenorKindKey;
    precondition: Precondition;
}

export interface TenorKindLookup {
    key: TenorKindKey;
    tenor_kind: TenorKind | null;
}

export interface TenorKindEvent {
    event_id: string;
    key: TenorKindKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TenorKindVersionKey {
    tenor_kind: TenorKindKey;
    version: number;
}

export interface TenorKindVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTenorKindsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTenorKindsResponse {
    result: Result;
    kinds: TenorKind[];
    total: number;
}

export interface GetTenorKindRequest {
    key: TenorKindKey;
}

export interface GetTenorKindResponse {
    result: Result;
    tenor_kind: TenorKind | null;
}

export interface GetManyTenorKindsRequest {
    keys: TenorKindKey[];
}

export interface GetManyTenorKindsResponse {
    result: Result;
    entries: TenorKindLookup[];
}

export interface PutTenorKindRequest {
    change: TenorKindChange;
    intent: ChangeIntent;
}

export interface PutTenorKindResponse {
    result: Result;
    tenor_kind: TenorKind;
}

export interface PutManyTenorKindsRequest {
    changes: TenorKindChange[];
    intent: ChangeIntent;
}

export interface PutManyTenorKindsResponse {
    result: Result;
    kinds: TenorKind[];
}

export interface DeleteTenorKindRequest {
    removal: TenorKindRemoval;
    intent: ChangeIntent;
}

export interface DeleteTenorKindResponse {
    result: Result;
}

export interface DeleteManyTenorKindsRequest {
    removals: TenorKindRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTenorKindsResponse {
    result: Result;
}

export interface ListTenorKindVersionsRequest {
    key: TenorKindKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorKindVersionsFilter | null;
}

export interface ListTenorKindVersionsResponse {
    result: Result;
    versions: TenorKind[];
    total: number;
}

export interface GetTenorKindVersionRequest {
    key: TenorKindVersionKey;
}

export interface GetTenorKindVersionResponse {
    result: Result;
    version: TenorKind;
}

export const subjects = {
    list_tenor_kinds_request: "refdata.v1.tenor_kinds.list",
    get_tenor_kind_request: "refdata.v1.tenor_kinds.get",
    get_many_tenor_kinds_request: "refdata.v1.tenor_kinds.get_many",
    put_tenor_kind_request: "refdata.v1.tenor_kinds.put",
    put_many_tenor_kinds_request: "refdata.v1.tenor_kinds.put_many",
    delete_tenor_kind_request: "refdata.v1.tenor_kinds.delete",
    delete_many_tenor_kinds_request: "refdata.v1.tenor_kinds.delete_many",
    list_tenor_kind_versions_request: "refdata.v1.tenor_kinds_versions.list",
    get_tenor_kind_version_request: "refdata.v1.tenor_kinds_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_tenor_kinds_request: true,
    get_tenor_kind_request: true,
    get_many_tenor_kinds_request: true,
    put_tenor_kind_request: true,
    put_many_tenor_kinds_request: true,
    delete_tenor_kind_request: true,
    delete_many_tenor_kinds_request: true,
    list_tenor_kind_versions_request: true,
    get_tenor_kind_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.tenor_kinds_events.created",
    updated: "refdata.v1.tenor_kinds_events.updated",
    deleted: "refdata.v1.tenor_kinds_events.deleted",
} as const;
