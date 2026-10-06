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
import type { StressTestLibrary } from '../domain/stress_test_library.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface StressTestLibraryKey {
    id: string;
}

export interface StressTestLibraryWrite {
    id: string;
    name: string;
    configuration_id: string;
    use_spreaded_term_structures: boolean | null;
}

export interface StressTestLibraryChange {
    write: StressTestLibraryWrite;
    precondition: Precondition;
}

export interface StressTestLibraryRemoval {
    key: StressTestLibraryKey;
    precondition: Precondition;
}

export interface StressTestLibraryLookup {
    key: StressTestLibraryKey;
    stress_test_library: StressTestLibrary | null;
}

export interface StressTestLibrariesFilter {
    id_one_of: string[] | null;
}

export interface StressTestLibraryEvent {
    event_id: string;
    key: StressTestLibraryKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface StressTestLibraryVersionKey {
    stress_test_library: StressTestLibraryKey;
    version: number;
}

export interface StressTestLibraryVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListStressTestLibrariesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: StressTestLibrariesFilter | null;
    as_of: string | null;
}

export interface ListStressTestLibrariesResponse {
    result: Result;
    stress_test_libraries: StressTestLibrary[];
    total: number;
}

export interface GetStressTestLibraryRequest {
    key: StressTestLibraryKey;
}

export interface GetStressTestLibraryResponse {
    result: Result;
    stress_test_library: StressTestLibrary | null;
}

export interface GetManyStressTestLibrariesRequest {
    keys: StressTestLibraryKey[];
}

export interface GetManyStressTestLibrariesResponse {
    result: Result;
    entries: StressTestLibraryLookup[];
}

export interface PutStressTestLibraryRequest {
    change: StressTestLibraryChange;
    intent: ChangeIntent;
}

export interface PutStressTestLibraryResponse {
    result: Result;
    stress_test_library: StressTestLibrary | null;
}

export interface PutManyStressTestLibrariesRequest {
    changes: StressTestLibraryChange[];
    intent: ChangeIntent;
}

export interface PutManyStressTestLibrariesResponse {
    result: Result;
    stress_test_libraries: StressTestLibrary[];
}

export interface DeleteStressTestLibraryRequest {
    removal: StressTestLibraryRemoval;
    intent: ChangeIntent;
}

export interface DeleteStressTestLibraryResponse {
    result: Result;
}

export interface DeleteManyStressTestLibrariesRequest {
    removals: StressTestLibraryRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyStressTestLibrariesResponse {
    result: Result;
}

export interface ListStressTestLibraryVersionsRequest {
    key: StressTestLibraryKey;
    offset: number;
    limit: number;
    order: Order;
    filter: StressTestLibraryVersionsFilter | null;
}

export interface ListStressTestLibraryVersionsResponse {
    result: Result;
    versions: StressTestLibrary[];
    total: number;
}

export interface GetStressTestLibraryVersionRequest {
    key: StressTestLibraryVersionKey;
}

export interface GetStressTestLibraryVersionResponse {
    result: Result;
    version: StressTestLibrary | null;
}

export const subjects = {
    list_stress_test_libraries_request: 'analytics.v1.stress_test_libraries.list',
    get_stress_test_library_request: 'analytics.v1.stress_test_libraries.get',
    get_many_stress_test_libraries_request: 'analytics.v1.stress_test_libraries.get_many',
    put_stress_test_library_request: 'analytics.v1.stress_test_libraries.put',
    put_many_stress_test_libraries_request: 'analytics.v1.stress_test_libraries.put_many',
    delete_stress_test_library_request: 'analytics.v1.stress_test_libraries.delete',
    delete_many_stress_test_libraries_request: 'analytics.v1.stress_test_libraries.delete_many',
    list_stress_test_library_versions_request: 'analytics.v1.stress_test_libraries_versions.list',
    get_stress_test_library_version_request: 'analytics.v1.stress_test_libraries_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_stress_test_libraries_request: true,
    get_stress_test_library_request: true,
    get_many_stress_test_libraries_request: true,
    put_stress_test_library_request: true,
    put_many_stress_test_libraries_request: true,
    delete_stress_test_library_request: true,
    delete_many_stress_test_libraries_request: true,
    list_stress_test_library_versions_request: true,
    get_stress_test_library_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.stress_test_libraries_events.created',
    updated: 'analytics.v1.stress_test_libraries_events.updated',
    deleted: 'analytics.v1.stress_test_libraries_events.deleted',
} as const;
