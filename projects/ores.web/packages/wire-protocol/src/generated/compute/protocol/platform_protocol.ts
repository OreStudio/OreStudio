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
import type { Platform } from '../domain/platform.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PlatformKey {
    code: string;
}

export interface PlatformWrite {
    id: string;
    code: string;
    display_name: string;
    description: string;
    os_family: string;
    cpu_arch: string;
    abi: string;
    is_active: boolean;
}

export interface PlatformChange {
    write: PlatformWrite;
    precondition: Precondition;
}

export interface PlatformRemoval {
    key: PlatformKey;
    precondition: Precondition;
}

export interface PlatformLookup {
    key: PlatformKey;
    platform: Platform | null;
}

export interface PlatformEvent {
    event_id: string;
    key: PlatformKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PlatformVersionKey {
    platform: PlatformKey;
    version: number;
}

export interface PlatformVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPlatformsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListPlatformsResponse {
    result: Result;
    platforms: Platform[];
    total: number;
}

export interface GetPlatformRequest {
    key: PlatformKey;
}

export interface GetPlatformResponse {
    result: Result;
    platform: Platform | null;
}

export interface GetManyPlatformsRequest {
    keys: PlatformKey[];
}

export interface GetManyPlatformsResponse {
    result: Result;
    entries: PlatformLookup[];
}

export interface PutPlatformRequest {
    change: PlatformChange;
    intent: ChangeIntent;
}

export interface PutPlatformResponse {
    result: Result;
    platform: Platform;
}

export interface PutManyPlatformsRequest {
    changes: PlatformChange[];
    intent: ChangeIntent;
}

export interface PutManyPlatformsResponse {
    result: Result;
    platforms: Platform[];
}

export interface DeletePlatformRequest {
    removal: PlatformRemoval;
    intent: ChangeIntent;
}

export interface DeletePlatformResponse {
    result: Result;
}

export interface DeleteManyPlatformsRequest {
    removals: PlatformRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPlatformsResponse {
    result: Result;
}

export interface ListPlatformVersionsRequest {
    key: PlatformKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PlatformVersionsFilter | null;
}

export interface ListPlatformVersionsResponse {
    result: Result;
    versions: Platform[];
    total: number;
}

export interface GetPlatformVersionRequest {
    key: PlatformVersionKey;
}

export interface GetPlatformVersionResponse {
    result: Result;
    version: Platform;
}

export const subjects = {
    list_platforms_request: "compute.v1.platforms.list",
    get_platform_request: "compute.v1.platforms.get",
    get_many_platforms_request: "compute.v1.platforms.get_many",
    put_platform_request: "compute.v1.platforms.put",
    put_many_platforms_request: "compute.v1.platforms.put_many",
    delete_platform_request: "compute.v1.platforms.delete",
    delete_many_platforms_request: "compute.v1.platforms.delete_many",
    list_platform_versions_request: "compute.v1.platforms_versions.list",
    get_platform_version_request: "compute.v1.platforms_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_platforms_request: true,
    get_platform_request: true,
    get_many_platforms_request: true,
    put_platform_request: true,
    put_many_platforms_request: true,
    delete_platform_request: true,
    delete_many_platforms_request: true,
    list_platform_versions_request: true,
    get_platform_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "compute.v1.platforms_events.created",
    updated: "compute.v1.platforms_events.updated",
    deleted: "compute.v1.platforms_events.deleted",
} as const;
