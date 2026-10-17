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
import type { BundleImport } from '../domain/bundle_import.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BundleImportKey {
    id: string;
}

export interface BundleImportWrite {
    id: string;
    source: string | null;
    state: string;
    families_expected: number | null;
    expires_at: string;
}

export interface BundleImportChange {
    write: BundleImportWrite;
    precondition: Precondition;
}

export interface BundleImportRemoval {
    key: BundleImportKey;
    precondition: Precondition;
}

export interface BundleImportLookup {
    key: BundleImportKey;
    bundle_import: BundleImport | null;
}

export interface BundleImportsFilter {
    id_one_of: string[] | null;
}

export interface BundleImportEvent {
    event_id: string;
    key: BundleImportKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BundleImportVersionKey {
    bundle_import: BundleImportKey;
    version: number;
}

export interface BundleImportVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBundleImportsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BundleImportsFilter | null;
    as_of: string | null;
}

export interface ListBundleImportsResponse {
    result: Result;
    bundle_imports: BundleImport[];
    total: number;
}

export interface GetBundleImportRequest {
    key: BundleImportKey;
}

export interface GetBundleImportResponse {
    result: Result;
    bundle_import: BundleImport | null;
}

export interface GetManyBundleImportsRequest {
    keys: BundleImportKey[];
}

export interface GetManyBundleImportsResponse {
    result: Result;
    entries: BundleImportLookup[];
}

export interface PutBundleImportRequest {
    change: BundleImportChange;
    intent: ChangeIntent;
}

export interface PutBundleImportResponse {
    result: Result;
    bundle_import: BundleImport | null;
}

export interface PutManyBundleImportsRequest {
    changes: BundleImportChange[];
    intent: ChangeIntent;
}

export interface PutManyBundleImportsResponse {
    result: Result;
    bundle_imports: BundleImport[];
}

export interface DeleteBundleImportRequest {
    removal: BundleImportRemoval;
    intent: ChangeIntent;
}

export interface DeleteBundleImportResponse {
    result: Result;
}

export interface DeleteManyBundleImportsRequest {
    removals: BundleImportRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBundleImportsResponse {
    result: Result;
}

export interface ListBundleImportVersionsRequest {
    key: BundleImportKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BundleImportVersionsFilter | null;
}

export interface ListBundleImportVersionsResponse {
    result: Result;
    versions: BundleImport[];
    total: number;
}

export interface GetBundleImportVersionRequest {
    key: BundleImportVersionKey;
}

export interface GetBundleImportVersionResponse {
    result: Result;
    version: BundleImport | null;
}

export const subjects = {
    list_bundle_imports_request: 'refdata.v1.bundle_imports.list',
    get_bundle_import_request: 'refdata.v1.bundle_imports.get',
    get_many_bundle_imports_request: 'refdata.v1.bundle_imports.get_many',
    put_bundle_import_request: 'refdata.v1.bundle_imports.put',
    put_many_bundle_imports_request: 'refdata.v1.bundle_imports.put_many',
    delete_bundle_import_request: 'refdata.v1.bundle_imports.delete',
    delete_many_bundle_imports_request: 'refdata.v1.bundle_imports.delete_many',
    list_bundle_import_versions_request: 'refdata.v1.bundle_imports_versions.list',
    get_bundle_import_version_request: 'refdata.v1.bundle_imports_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bundle_imports_request: true,
    get_bundle_import_request: true,
    get_many_bundle_imports_request: true,
    put_bundle_import_request: true,
    put_many_bundle_imports_request: true,
    delete_bundle_import_request: true,
    delete_many_bundle_imports_request: true,
    list_bundle_import_versions_request: true,
    get_bundle_import_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.bundle_imports_events.created',
    updated: 'refdata.v1.bundle_imports_events.updated',
    deleted: 'refdata.v1.bundle_imports_events.deleted',
} as const;
