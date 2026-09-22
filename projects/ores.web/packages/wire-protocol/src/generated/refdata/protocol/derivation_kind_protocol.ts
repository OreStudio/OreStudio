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
import type { DerivationKind } from '../domain/derivation_kind.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DerivationKindKey {
    code: string;
}

export interface DerivationKindWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface DerivationKindChange {
    write: DerivationKindWrite;
    precondition: Precondition;
}

export interface DerivationKindRemoval {
    key: DerivationKindKey;
    precondition: Precondition;
}

export interface DerivationKindLookup {
    key: DerivationKindKey;
    derivation_kind: DerivationKind | null;
}

export interface DerivationKindEvent {
    event_id: string;
    key: DerivationKindKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DerivationKindVersionKey {
    derivation_kind: DerivationKindKey;
    version: number;
}

export interface DerivationKindVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDerivationKindsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListDerivationKindsResponse {
    result: Result;
    kinds: DerivationKind[];
    total: number;
}

export interface GetDerivationKindRequest {
    key: DerivationKindKey;
}

export interface GetDerivationKindResponse {
    result: Result;
    derivation_kind: DerivationKind | null;
}

export interface GetManyDerivationKindsRequest {
    keys: DerivationKindKey[];
}

export interface GetManyDerivationKindsResponse {
    result: Result;
    entries: DerivationKindLookup[];
}

export interface PutDerivationKindRequest {
    change: DerivationKindChange;
    intent: ChangeIntent;
}

export interface PutDerivationKindResponse {
    result: Result;
    derivation_kind: DerivationKind;
}

export interface PutManyDerivationKindsRequest {
    changes: DerivationKindChange[];
    intent: ChangeIntent;
}

export interface PutManyDerivationKindsResponse {
    result: Result;
    kinds: DerivationKind[];
}

export interface DeleteDerivationKindRequest {
    removal: DerivationKindRemoval;
    intent: ChangeIntent;
}

export interface DeleteDerivationKindResponse {
    result: Result;
}

export interface DeleteManyDerivationKindsRequest {
    removals: DerivationKindRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDerivationKindsResponse {
    result: Result;
}

export interface ListDerivationKindVersionsRequest {
    key: DerivationKindKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DerivationKindVersionsFilter | null;
}

export interface ListDerivationKindVersionsResponse {
    result: Result;
    versions: DerivationKind[];
    total: number;
}

export interface GetDerivationKindVersionRequest {
    key: DerivationKindVersionKey;
}

export interface GetDerivationKindVersionResponse {
    result: Result;
    version: DerivationKind;
}

export const subjects = {
    list_derivation_kinds_request: "refdata.v1.derivation_kinds.list",
    get_derivation_kind_request: "refdata.v1.derivation_kinds.get",
    get_many_derivation_kinds_request: "refdata.v1.derivation_kinds.get_many",
    put_derivation_kind_request: "refdata.v1.derivation_kinds.put",
    put_many_derivation_kinds_request: "refdata.v1.derivation_kinds.put_many",
    delete_derivation_kind_request: "refdata.v1.derivation_kinds.delete",
    delete_many_derivation_kinds_request: "refdata.v1.derivation_kinds.delete_many",
    list_derivation_kind_versions_request: "refdata.v1.derivation_kinds_versions.list",
    get_derivation_kind_version_request: "refdata.v1.derivation_kinds_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_derivation_kinds_request: true,
    get_derivation_kind_request: true,
    get_many_derivation_kinds_request: true,
    put_derivation_kind_request: true,
    put_many_derivation_kinds_request: true,
    delete_derivation_kind_request: true,
    delete_many_derivation_kinds_request: true,
    list_derivation_kind_versions_request: true,
    get_derivation_kind_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.derivation_kinds_events.created",
    updated: "refdata.v1.derivation_kinds_events.updated",
    deleted: "refdata.v1.derivation_kinds_events.deleted",
} as const;
