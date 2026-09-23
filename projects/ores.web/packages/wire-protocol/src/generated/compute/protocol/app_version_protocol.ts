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
import type { AppVersion } from '../domain/app_version.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface AppVersionKey {
    wrapper_version: string;
}

export interface AppVersionWrite {
    id: string;
    app_id: string;
    wrapper_version: string;
    engine_version: string;
    min_ram_mb: number;
}

export interface AppVersionChange {
    write: AppVersionWrite;
    precondition: Precondition;
}

export interface AppVersionRemoval {
    key: AppVersionKey;
    precondition: Precondition;
}

export interface AppVersionLookup {
    key: AppVersionKey;
    app_version: AppVersion | null;
}

export interface AppVersionEvent {
    event_id: string;
    key: AppVersionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AppVersionVersionKey {
    app_version: AppVersionKey;
    version: number;
}

export interface AppVersionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAppVersionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListAppVersionsResponse {
    result: Result;
    app_versions: AppVersion[];
    total: number;
}

export interface GetAppVersionRequest {
    key: AppVersionKey;
}

export interface GetAppVersionResponse {
    result: Result;
    app_version: AppVersion | null;
}

export interface GetManyAppVersionsRequest {
    keys: AppVersionKey[];
}

export interface GetManyAppVersionsResponse {
    result: Result;
    entries: AppVersionLookup[];
}

export interface PutAppVersionRequest {
    change: AppVersionChange;
    intent: ChangeIntent;
}

export interface PutAppVersionResponse {
    result: Result;
    app_version: AppVersion;
}

export interface PutManyAppVersionsRequest {
    changes: AppVersionChange[];
    intent: ChangeIntent;
}

export interface PutManyAppVersionsResponse {
    result: Result;
    app_versions: AppVersion[];
}

export interface DeleteAppVersionRequest {
    removal: AppVersionRemoval;
    intent: ChangeIntent;
}

export interface DeleteAppVersionResponse {
    result: Result;
}

export interface DeleteManyAppVersionsRequest {
    removals: AppVersionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAppVersionsResponse {
    result: Result;
}

export interface ListAppVersionVersionsRequest {
    key: AppVersionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AppVersionVersionsFilter | null;
}

export interface ListAppVersionVersionsResponse {
    result: Result;
    versions: AppVersion[];
    total: number;
}

export interface GetAppVersionVersionRequest {
    key: AppVersionVersionKey;
}

export interface GetAppVersionVersionResponse {
    result: Result;
    version: AppVersion;
}

export const subjects = {
    list_app_versions_request: "compute.v1.app_versions.list",
    get_app_version_request: "compute.v1.app_versions.get",
    get_many_app_versions_request: "compute.v1.app_versions.get_many",
    put_app_version_request: "compute.v1.app_versions.put",
    put_many_app_versions_request: "compute.v1.app_versions.put_many",
    delete_app_version_request: "compute.v1.app_versions.delete",
    delete_many_app_versions_request: "compute.v1.app_versions.delete_many",
    list_app_version_versions_request: "compute.v1.app_versions_versions.list",
    get_app_version_version_request: "compute.v1.app_versions_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_app_versions_request: true,
    get_app_version_request: true,
    get_many_app_versions_request: true,
    put_app_version_request: true,
    put_many_app_versions_request: true,
    delete_app_version_request: true,
    delete_many_app_versions_request: true,
    list_app_version_versions_request: true,
    get_app_version_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "compute.v1.app_versions_events.created",
    updated: "compute.v1.app_versions_events.updated",
    deleted: "compute.v1.app_versions_events.deleted",
} as const;
