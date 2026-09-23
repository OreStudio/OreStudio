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
import type { App } from '../domain/app.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface AppKey {
    name: string;
}

export interface AppWrite {
    id: string;
    name: string;
    description: string;
}

export interface AppChange {
    write: AppWrite;
    precondition: Precondition;
}

export interface AppRemoval {
    key: AppKey;
    precondition: Precondition;
}

export interface AppLookup {
    key: AppKey;
    app: App | null;
}

export interface AppEvent {
    event_id: string;
    key: AppKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AppsVersionKey {
    app: AppKey;
    version: number;
}

export interface AppsVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAppsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListAppsResponse {
    result: Result;
    apps: App[];
    total: number;
}

export interface GetAppRequest {
    key: AppKey;
}

export interface GetAppResponse {
    result: Result;
    app: App | null;
}

export interface GetManyAppsRequest {
    keys: AppKey[];
}

export interface GetManyAppsResponse {
    result: Result;
    entries: AppLookup[];
}

export interface PutAppRequest {
    change: AppChange;
    intent: ChangeIntent;
}

export interface PutAppResponse {
    result: Result;
    app: App;
}

export interface PutManyAppsRequest {
    changes: AppChange[];
    intent: ChangeIntent;
}

export interface PutManyAppsResponse {
    result: Result;
    apps: App[];
}

export interface DeleteAppRequest {
    removal: AppRemoval;
    intent: ChangeIntent;
}

export interface DeleteAppResponse {
    result: Result;
}

export interface DeleteManyAppsRequest {
    removals: AppRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAppsResponse {
    result: Result;
}

export interface ListAppsVersionsRequest {
    key: AppKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AppsVersionsFilter | null;
}

export interface ListAppsVersionsResponse {
    result: Result;
    versions: App[];
    total: number;
}

export interface GetAppsVersionRequest {
    key: AppsVersionKey;
}

export interface GetAppsVersionResponse {
    result: Result;
    version: App;
}

export const subjects = {
    list_apps_request: "compute.v1.apps.list",
    get_app_request: "compute.v1.apps.get",
    get_many_apps_request: "compute.v1.apps.get_many",
    put_app_request: "compute.v1.apps.put",
    put_many_apps_request: "compute.v1.apps.put_many",
    delete_app_request: "compute.v1.apps.delete",
    delete_many_apps_request: "compute.v1.apps.delete_many",
    list_apps_versions_request: "compute.v1.apps_versions.list",
    get_apps_version_request: "compute.v1.apps_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_apps_request: true,
    get_app_request: true,
    get_many_apps_request: true,
    put_app_request: true,
    put_many_apps_request: true,
    delete_app_request: true,
    delete_many_apps_request: true,
    list_apps_versions_request: true,
    get_apps_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "compute.v1.apps_events.created",
    updated: "compute.v1.apps_events.updated",
    deleted: "compute.v1.apps_events.deleted",
} as const;
