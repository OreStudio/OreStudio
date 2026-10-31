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
import type { CutKind } from '../domain/cut_kind.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CutKindKey {
    code: string;
}

export interface CutKindWrite {
    code: string;
    name: string;
    description: string;
    family_code: string;
    approval_kind_code: string | null;
    output_is_indicative: boolean;
    resolution_rule_code: string;
    retention_months: number;
    retention_destination_code: string;
    legal_hold_default: boolean;
    display_order: number;
}

export interface CutKindChange {
    write: CutKindWrite;
    precondition: Precondition;
}

export interface CutKindRemoval {
    key: CutKindKey;
    precondition: Precondition;
}

export interface CutKindLookup {
    key: CutKindKey;
    cut_kind: CutKind | null;
}

export interface CutKindsFilter {
    code_one_of: string[] | null;
}

export interface CutKindEvent {
    event_id: string;
    key: CutKindKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CutKindVersionKey {
    cut_kind: CutKindKey;
    version: number;
}

export interface CutKindVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCutKindsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CutKindsFilter | null;
    as_of: string | null;
}

export interface ListCutKindsResponse {
    result: Result;
    kinds: CutKind[];
    total: number;
}

export interface GetCutKindRequest {
    key: CutKindKey;
}

export interface GetCutKindResponse {
    result: Result;
    cut_kind: CutKind | null;
}

export interface GetManyCutKindsRequest {
    keys: CutKindKey[];
}

export interface GetManyCutKindsResponse {
    result: Result;
    entries: CutKindLookup[];
}

export interface PutCutKindRequest {
    change: CutKindChange;
    intent: ChangeIntent;
}

export interface PutCutKindResponse {
    result: Result;
    cut_kind: CutKind | null;
}

export interface PutManyCutKindsRequest {
    changes: CutKindChange[];
    intent: ChangeIntent;
}

export interface PutManyCutKindsResponse {
    result: Result;
    kinds: CutKind[];
}

export interface DeleteCutKindRequest {
    removal: CutKindRemoval;
    intent: ChangeIntent;
}

export interface DeleteCutKindResponse {
    result: Result;
}

export interface DeleteManyCutKindsRequest {
    removals: CutKindRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCutKindsResponse {
    result: Result;
}

export interface ListCutKindVersionsRequest {
    key: CutKindKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CutKindVersionsFilter | null;
}

export interface ListCutKindVersionsResponse {
    result: Result;
    versions: CutKind[];
    total: number;
}

export interface GetCutKindVersionRequest {
    key: CutKindVersionKey;
}

export interface GetCutKindVersionResponse {
    result: Result;
    version: CutKind | null;
}

export const subjects = {
    list_cut_kinds_request: 'reporting.v1.cut_kinds.list',
    get_cut_kind_request: 'reporting.v1.cut_kinds.get',
    get_many_cut_kinds_request: 'reporting.v1.cut_kinds.get_many',
    put_cut_kind_request: 'reporting.v1.cut_kinds.put',
    put_many_cut_kinds_request: 'reporting.v1.cut_kinds.put_many',
    delete_cut_kind_request: 'reporting.v1.cut_kinds.delete',
    delete_many_cut_kinds_request: 'reporting.v1.cut_kinds.delete_many',
    list_cut_kind_versions_request: 'reporting.v1.cut_kinds_versions.list',
    get_cut_kind_version_request: 'reporting.v1.cut_kinds_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cut_kinds_request: true,
    get_cut_kind_request: true,
    get_many_cut_kinds_request: true,
    put_cut_kind_request: true,
    put_many_cut_kinds_request: true,
    delete_cut_kind_request: true,
    delete_many_cut_kinds_request: true,
    list_cut_kind_versions_request: true,
    get_cut_kind_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.cut_kinds_events.created',
    updated: 'reporting.v1.cut_kinds_events.updated',
    deleted: 'reporting.v1.cut_kinds_events.deleted',
} as const;
