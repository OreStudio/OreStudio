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
import type { StressShiftFamily } from '../domain/stress_shift_family.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface StressShiftFamilyKey {
    code: string;
}

export interface StressShiftFamilyWrite {
    code: string;
    description: string;
}

export interface StressShiftFamilyChange {
    write: StressShiftFamilyWrite;
    precondition: Precondition;
}

export interface StressShiftFamilyRemoval {
    key: StressShiftFamilyKey;
    precondition: Precondition;
}

export interface StressShiftFamilyLookup {
    key: StressShiftFamilyKey;
    stress_shift_family: StressShiftFamily | null;
}

export interface StressShiftFamiliesFilter {
    code_one_of: string[] | null;
}

export interface StressShiftFamilyEvent {
    event_id: string;
    key: StressShiftFamilyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface StressShiftFamilyVersionKey {
    stress_shift_family: StressShiftFamilyKey;
    version: number;
}

export interface StressShiftFamilyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListStressShiftFamiliesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: StressShiftFamiliesFilter | null;
    as_of: string | null;
}

export interface ListStressShiftFamiliesResponse {
    result: Result;
    families: StressShiftFamily[];
    total: number;
}

export interface GetStressShiftFamilyRequest {
    key: StressShiftFamilyKey;
}

export interface GetStressShiftFamilyResponse {
    result: Result;
    stress_shift_family: StressShiftFamily | null;
}

export interface GetManyStressShiftFamiliesRequest {
    keys: StressShiftFamilyKey[];
}

export interface GetManyStressShiftFamiliesResponse {
    result: Result;
    entries: StressShiftFamilyLookup[];
}

export interface PutStressShiftFamilyRequest {
    change: StressShiftFamilyChange;
    intent: ChangeIntent;
}

export interface PutStressShiftFamilyResponse {
    result: Result;
    stress_shift_family: StressShiftFamily | null;
}

export interface PutManyStressShiftFamiliesRequest {
    changes: StressShiftFamilyChange[];
    intent: ChangeIntent;
}

export interface PutManyStressShiftFamiliesResponse {
    result: Result;
    families: StressShiftFamily[];
}

export interface DeleteStressShiftFamilyRequest {
    removal: StressShiftFamilyRemoval;
    intent: ChangeIntent;
}

export interface DeleteStressShiftFamilyResponse {
    result: Result;
}

export interface DeleteManyStressShiftFamiliesRequest {
    removals: StressShiftFamilyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyStressShiftFamiliesResponse {
    result: Result;
}

export interface ListStressShiftFamilyVersionsRequest {
    key: StressShiftFamilyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: StressShiftFamilyVersionsFilter | null;
}

export interface ListStressShiftFamilyVersionsResponse {
    result: Result;
    versions: StressShiftFamily[];
    total: number;
}

export interface GetStressShiftFamilyVersionRequest {
    key: StressShiftFamilyVersionKey;
}

export interface GetStressShiftFamilyVersionResponse {
    result: Result;
    version: StressShiftFamily | null;
}

export const subjects = {
    list_stress_shift_families_request: 'analytics.v1.stress_shift_families.list',
    get_stress_shift_family_request: 'analytics.v1.stress_shift_families.get',
    get_many_stress_shift_families_request: 'analytics.v1.stress_shift_families.get_many',
    put_stress_shift_family_request: 'analytics.v1.stress_shift_families.put',
    put_many_stress_shift_families_request: 'analytics.v1.stress_shift_families.put_many',
    delete_stress_shift_family_request: 'analytics.v1.stress_shift_families.delete',
    delete_many_stress_shift_families_request: 'analytics.v1.stress_shift_families.delete_many',
    list_stress_shift_family_versions_request: 'analytics.v1.stress_shift_families_versions.list',
    get_stress_shift_family_version_request: 'analytics.v1.stress_shift_families_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_stress_shift_families_request: true,
    get_stress_shift_family_request: true,
    get_many_stress_shift_families_request: true,
    put_stress_shift_family_request: true,
    put_many_stress_shift_families_request: true,
    delete_stress_shift_family_request: true,
    delete_many_stress_shift_families_request: true,
    list_stress_shift_family_versions_request: true,
    get_stress_shift_family_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.stress_shift_families_events.created',
    updated: 'analytics.v1.stress_shift_families_events.updated',
    deleted: 'analytics.v1.stress_shift_families_events.deleted',
} as const;
