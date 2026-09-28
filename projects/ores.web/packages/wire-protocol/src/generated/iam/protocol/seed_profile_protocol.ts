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
import type { SeedProfile } from '../domain/seed_profile.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SeedProfileKey {
    id: string;
}

export interface SeedProfileWrite {
    id: string;
    code: string;
    name: string;
    summary: string;
    audience: string;
    bullets_json: string;
    tenant_type: string;
    tenant_name: string;
    tenant_code: string;
    tenant_hostname: string;
    admin_username: string;
    admin_email: string;
    inherits_admin_password: boolean;
    force_password_change: boolean;
    display_order: number;
}

export interface SeedProfileChange {
    write: SeedProfileWrite;
    precondition: Precondition;
}

export interface SeedProfileRemoval {
    key: SeedProfileKey;
    precondition: Precondition;
}

export interface SeedProfileLookup {
    key: SeedProfileKey;
    seed_profile: SeedProfile | null;
}

export interface SeedProfileEvent {
    event_id: string;
    key: SeedProfileKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SeedProfileVersionKey {
    seed_profile: SeedProfileKey;
    version: number;
}

export interface SeedProfileVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSeedProfilesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListSeedProfilesResponse {
    result: Result;
    seed_profiles: SeedProfile[];
    total: number;
}

export interface GetSeedProfileRequest {
    key: SeedProfileKey;
}

export interface GetSeedProfileResponse {
    result: Result;
    seed_profile: SeedProfile | null;
}

export interface GetManySeedProfilesRequest {
    keys: SeedProfileKey[];
}

export interface GetManySeedProfilesResponse {
    result: Result;
    entries: SeedProfileLookup[];
}

export interface PutSeedProfileRequest {
    change: SeedProfileChange;
    intent: ChangeIntent;
}

export interface PutSeedProfileResponse {
    result: Result;
    seed_profile: SeedProfile | null;
}

export interface PutManySeedProfilesRequest {
    changes: SeedProfileChange[];
    intent: ChangeIntent;
}

export interface PutManySeedProfilesResponse {
    result: Result;
    seed_profiles: SeedProfile[];
}

export interface DeleteSeedProfileRequest {
    removal: SeedProfileRemoval;
    intent: ChangeIntent;
}

export interface DeleteSeedProfileResponse {
    result: Result;
}

export interface DeleteManySeedProfilesRequest {
    removals: SeedProfileRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySeedProfilesResponse {
    result: Result;
}

export interface ListSeedProfileVersionsRequest {
    key: SeedProfileKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SeedProfileVersionsFilter | null;
}

export interface ListSeedProfileVersionsResponse {
    result: Result;
    versions: SeedProfile[];
    total: number;
}

export interface GetSeedProfileVersionRequest {
    key: SeedProfileVersionKey;
}

export interface GetSeedProfileVersionResponse {
    result: Result;
    version: SeedProfile | null;
}

export const subjects = {
    list_seed_profiles_request: 'iam.v1.seed_profiles.list',
    get_seed_profile_request: 'iam.v1.seed_profiles.get',
    get_many_seed_profiles_request: 'iam.v1.seed_profiles.get_many',
    put_seed_profile_request: 'iam.v1.seed_profiles.put',
    put_many_seed_profiles_request: 'iam.v1.seed_profiles.put_many',
    delete_seed_profile_request: 'iam.v1.seed_profiles.delete',
    delete_many_seed_profiles_request: 'iam.v1.seed_profiles.delete_many',
    list_seed_profile_versions_request: 'iam.v1.seed_profiles_versions.list',
    get_seed_profile_version_request: 'iam.v1.seed_profiles_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_seed_profiles_request: true,
    get_seed_profile_request: true,
    get_many_seed_profiles_request: true,
    put_seed_profile_request: true,
    put_many_seed_profiles_request: true,
    delete_seed_profile_request: true,
    delete_many_seed_profiles_request: true,
    list_seed_profile_versions_request: true,
    get_seed_profile_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'iam.v1.seed_profiles_events.created',
    updated: 'iam.v1.seed_profiles_events.updated',
    deleted: 'iam.v1.seed_profiles_events.deleted',
} as const;
