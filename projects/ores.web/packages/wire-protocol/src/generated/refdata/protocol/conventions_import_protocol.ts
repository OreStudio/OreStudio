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
import type { ConventionsImport } from '../domain/conventions_import.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ConventionsImportKey {
    id: string;
}

export interface ConventionsImportWrite {
    id: string;
    source: string | null;
    state: string;
    families_expected: number | null;
    expires_at: string;
}

export interface ConventionsImportChange {
    write: ConventionsImportWrite;
    precondition: Precondition;
}

export interface ConventionsImportRemoval {
    key: ConventionsImportKey;
    precondition: Precondition;
}

export interface ConventionsImportLookup {
    key: ConventionsImportKey;
    conventions_import: ConventionsImport | null;
}

export interface ConventionsImportsFilter {
    id_one_of: string[] | null;
}

export interface ConventionsImportEvent {
    event_id: string;
    key: ConventionsImportKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ConventionsImportVersionKey {
    conventions_import: ConventionsImportKey;
    version: number;
}

export interface ConventionsImportVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListConventionsImportsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ConventionsImportsFilter | null;
    as_of: string | null;
}

export interface ListConventionsImportsResponse {
    result: Result;
    conventions_imports: ConventionsImport[];
    total: number;
}

export interface GetConventionsImportRequest {
    key: ConventionsImportKey;
}

export interface GetConventionsImportResponse {
    result: Result;
    conventions_import: ConventionsImport | null;
}

export interface GetManyConventionsImportsRequest {
    keys: ConventionsImportKey[];
}

export interface GetManyConventionsImportsResponse {
    result: Result;
    entries: ConventionsImportLookup[];
}

export interface PutConventionsImportRequest {
    change: ConventionsImportChange;
    intent: ChangeIntent;
}

export interface PutConventionsImportResponse {
    result: Result;
    conventions_import: ConventionsImport | null;
}

export interface PutManyConventionsImportsRequest {
    changes: ConventionsImportChange[];
    intent: ChangeIntent;
}

export interface PutManyConventionsImportsResponse {
    result: Result;
    conventions_imports: ConventionsImport[];
}

export interface DeleteConventionsImportRequest {
    removal: ConventionsImportRemoval;
    intent: ChangeIntent;
}

export interface DeleteConventionsImportResponse {
    result: Result;
}

export interface DeleteManyConventionsImportsRequest {
    removals: ConventionsImportRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyConventionsImportsResponse {
    result: Result;
}

export interface ListConventionsImportVersionsRequest {
    key: ConventionsImportKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ConventionsImportVersionsFilter | null;
}

export interface ListConventionsImportVersionsResponse {
    result: Result;
    versions: ConventionsImport[];
    total: number;
}

export interface GetConventionsImportVersionRequest {
    key: ConventionsImportVersionKey;
}

export interface GetConventionsImportVersionResponse {
    result: Result;
    version: ConventionsImport | null;
}

export const subjects = {
    list_conventions_imports_request: 'refdata.v1.conventions_imports.list',
    get_conventions_import_request: 'refdata.v1.conventions_imports.get',
    get_many_conventions_imports_request: 'refdata.v1.conventions_imports.get_many',
    put_conventions_import_request: 'refdata.v1.conventions_imports.put',
    put_many_conventions_imports_request: 'refdata.v1.conventions_imports.put_many',
    delete_conventions_import_request: 'refdata.v1.conventions_imports.delete',
    delete_many_conventions_imports_request: 'refdata.v1.conventions_imports.delete_many',
    list_conventions_import_versions_request: 'refdata.v1.conventions_imports_versions.list',
    get_conventions_import_version_request: 'refdata.v1.conventions_imports_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_conventions_imports_request: true,
    get_conventions_import_request: true,
    get_many_conventions_imports_request: true,
    put_conventions_import_request: true,
    put_many_conventions_imports_request: true,
    delete_conventions_import_request: true,
    delete_many_conventions_imports_request: true,
    list_conventions_import_versions_request: true,
    get_conventions_import_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.conventions_imports_events.created',
    updated: 'refdata.v1.conventions_imports_events.updated',
    deleted: 'refdata.v1.conventions_imports_events.deleted',
} as const;
