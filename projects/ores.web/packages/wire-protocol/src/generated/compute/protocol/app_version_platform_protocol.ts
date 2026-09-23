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
import type { AppVersionPlatform } from '../domain/app_version_platform.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface AppVersionPlatformKey {
    app_version_id: string;
    platform_id: string;
}

export interface AppVersionPlatformWrite {
    app_version_id: string;
    platform_id: string;
    package_uri: string;
    sha256: string;
}

export interface AppVersionPlatformChange {
    write: AppVersionPlatformWrite;
    precondition: Precondition;
}

export interface AppVersionPlatformRemoval {
    key: AppVersionPlatformKey;
    precondition: Precondition;
}

export interface AppVersionPlatformLookup {
    key: AppVersionPlatformKey;
    app_version_platform: AppVersionPlatform | null;
}

export interface AppVersionPlatformsFilter {
    app_version_id: string | null;
}

export interface AppVersionPlatformEvent {
    event_id: string;
    key: AppVersionPlatformKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListAppVersionPlatformsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: AppVersionPlatformsFilter | null;
}

export interface ListAppVersionPlatformsResponse {
    result: Result;
    app_version_platforms: AppVersionPlatform[];
    total: number;
}

export interface GetAppVersionPlatformRequest {
    key: AppVersionPlatformKey;
}

export interface GetAppVersionPlatformResponse {
    result: Result;
    app_version_platform: AppVersionPlatform | null;
}

export interface GetManyAppVersionPlatformsRequest {
    keys: AppVersionPlatformKey[];
}

export interface GetManyAppVersionPlatformsResponse {
    result: Result;
    entries: AppVersionPlatformLookup[];
}

export interface PutAppVersionPlatformRequest {
    change: AppVersionPlatformChange;
    intent: ChangeIntent;
}

export interface PutAppVersionPlatformResponse {
    result: Result;
    app_version_platform: AppVersionPlatform;
}

export interface PutManyAppVersionPlatformsRequest {
    changes: AppVersionPlatformChange[];
    intent: ChangeIntent;
}

export interface PutManyAppVersionPlatformsResponse {
    result: Result;
    app_version_platforms: AppVersionPlatform[];
}

export interface DeleteAppVersionPlatformRequest {
    removal: AppVersionPlatformRemoval;
    intent: ChangeIntent;
}

export interface DeleteAppVersionPlatformResponse {
    result: Result;
}

export interface DeleteManyAppVersionPlatformsRequest {
    removals: AppVersionPlatformRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAppVersionPlatformsResponse {
    result: Result;
}

export interface ListByAppVersionIdAppVersionPlatformsRequest {
    app_version_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: AppVersionPlatformsFilter | null;
}

export interface ListByAppVersionIdAppVersionPlatformsResponse {
    result: Result;
    app_version_platforms: AppVersionPlatform[];
    total: number;
}

export const subjects = {
    list_app_version_platforms_request: "compute.v1.app_version_platforms.list",
    get_app_version_platform_request: "compute.v1.app_version_platforms.get",
    get_many_app_version_platforms_request: "compute.v1.app_version_platforms.get_many",
    put_app_version_platform_request: "compute.v1.app_version_platforms.put",
    put_many_app_version_platforms_request: "compute.v1.app_version_platforms.put_many",
    delete_app_version_platform_request: "compute.v1.app_version_platforms.delete",
    delete_many_app_version_platforms_request: "compute.v1.app_version_platforms.delete_many",
    list_by_app_version_id_app_version_platforms_request: "compute.v1.app_version_platforms.list_by_app_version_id",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_app_version_platforms_request: true,
    get_app_version_platform_request: true,
    get_many_app_version_platforms_request: true,
    put_app_version_platform_request: true,
    put_many_app_version_platforms_request: true,
    delete_app_version_platform_request: true,
    delete_many_app_version_platforms_request: true,
    list_by_app_version_id_app_version_platforms_request: true,
} as const;
