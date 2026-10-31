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
import type { CutFamily } from '../domain/cut_family.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CutFamilyKey {
    code: string;
}

export interface CutFamilyWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface CutFamilyChange {
    write: CutFamilyWrite;
    precondition: Precondition;
}

export interface CutFamilyRemoval {
    key: CutFamilyKey;
    precondition: Precondition;
}

export interface CutFamilyLookup {
    key: CutFamilyKey;
    cut_family: CutFamily | null;
}

export interface CutFamiliesFilter {
    code_one_of: string[] | null;
}

export interface CutFamilyEvent {
    event_id: string;
    key: CutFamilyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CutFamilyVersionKey {
    cut_family: CutFamilyKey;
    version: number;
}

export interface CutFamilyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCutFamiliesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CutFamiliesFilter | null;
    as_of: string | null;
}

export interface ListCutFamiliesResponse {
    result: Result;
    families: CutFamily[];
    total: number;
}

export interface GetCutFamilyRequest {
    key: CutFamilyKey;
}

export interface GetCutFamilyResponse {
    result: Result;
    cut_family: CutFamily | null;
}

export interface GetManyCutFamiliesRequest {
    keys: CutFamilyKey[];
}

export interface GetManyCutFamiliesResponse {
    result: Result;
    entries: CutFamilyLookup[];
}

export interface PutCutFamilyRequest {
    change: CutFamilyChange;
    intent: ChangeIntent;
}

export interface PutCutFamilyResponse {
    result: Result;
    cut_family: CutFamily | null;
}

export interface PutManyCutFamiliesRequest {
    changes: CutFamilyChange[];
    intent: ChangeIntent;
}

export interface PutManyCutFamiliesResponse {
    result: Result;
    families: CutFamily[];
}

export interface DeleteCutFamilyRequest {
    removal: CutFamilyRemoval;
    intent: ChangeIntent;
}

export interface DeleteCutFamilyResponse {
    result: Result;
}

export interface DeleteManyCutFamiliesRequest {
    removals: CutFamilyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCutFamiliesResponse {
    result: Result;
}

export interface ListCutFamilyVersionsRequest {
    key: CutFamilyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CutFamilyVersionsFilter | null;
}

export interface ListCutFamilyVersionsResponse {
    result: Result;
    versions: CutFamily[];
    total: number;
}

export interface GetCutFamilyVersionRequest {
    key: CutFamilyVersionKey;
}

export interface GetCutFamilyVersionResponse {
    result: Result;
    version: CutFamily | null;
}

export const subjects = {
    list_cut_families_request: 'reporting.v1.cut_families.list',
    get_cut_family_request: 'reporting.v1.cut_families.get',
    get_many_cut_families_request: 'reporting.v1.cut_families.get_many',
    put_cut_family_request: 'reporting.v1.cut_families.put',
    put_many_cut_families_request: 'reporting.v1.cut_families.put_many',
    delete_cut_family_request: 'reporting.v1.cut_families.delete',
    delete_many_cut_families_request: 'reporting.v1.cut_families.delete_many',
    list_cut_family_versions_request: 'reporting.v1.cut_families_versions.list',
    get_cut_family_version_request: 'reporting.v1.cut_families_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cut_families_request: true,
    get_cut_family_request: true,
    get_many_cut_families_request: true,
    put_cut_family_request: true,
    put_many_cut_families_request: true,
    delete_cut_family_request: true,
    delete_many_cut_families_request: true,
    list_cut_family_versions_request: true,
    get_cut_family_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.cut_families_events.created',
    updated: 'reporting.v1.cut_families_events.updated',
    deleted: 'reporting.v1.cut_families_events.deleted',
} as const;
