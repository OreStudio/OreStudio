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
import type { RunGrant } from '../domain/run_grant.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface RunGrantKey {
    resource: string;
}

export interface RunGrantLookup {
    key: RunGrantKey;
    run_grant: RunGrant | null;
}

export interface RunGrantsFilter {
    grantor_account_id: string | null;
    id_one_of: string[] | null;
    grantor_account_id_one_of: string[] | null;
}

export interface RunGrantEvent {
    event_id: string;
    key: RunGrantKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface RunGrantVersionKey {
    run_grant: RunGrantKey;
    version: number;
}

export interface RunGrantVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRunGrantsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RunGrantsFilter | null;
    as_of: string | null;
}

export interface ListRunGrantsResponse {
    result: Result;
    grants: RunGrant[];
    total: number;
}

export interface GetRunGrantRequest {
    key: RunGrantKey;
}

export interface GetRunGrantResponse {
    result: Result;
    run_grant: RunGrant | null;
}

export interface GetManyRunGrantsRequest {
    keys: RunGrantKey[];
}

export interface GetManyRunGrantsResponse {
    result: Result;
    entries: RunGrantLookup[];
}

export interface ListByGrantorAccountIdRunGrantsRequest {
    grantor_account_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: RunGrantsFilter | null;
}

export interface ListByGrantorAccountIdRunGrantsResponse {
    result: Result;
    grants: RunGrant[];
    total: number;
}

export interface ListRunGrantVersionsRequest {
    key: RunGrantKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RunGrantVersionsFilter | null;
}

export interface ListRunGrantVersionsResponse {
    result: Result;
    versions: RunGrant[];
    total: number;
}

export interface GetRunGrantVersionRequest {
    key: RunGrantVersionKey;
}

export interface GetRunGrantVersionResponse {
    result: Result;
    version: RunGrant | null;
}

export const subjects = {
    list_run_grants_request: 'iam.v1.run_grants.list',
    get_run_grant_request: 'iam.v1.run_grants.get',
    get_many_run_grants_request: 'iam.v1.run_grants.get_many',
    list_by_grantor_account_id_run_grants_request: 'iam.v1.run_grants.list_by_grantor_account_id',
    list_run_grant_versions_request: 'iam.v1.run_grants_versions.list',
    get_run_grant_version_request: 'iam.v1.run_grants_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_run_grants_request: true,
    get_run_grant_request: true,
    get_many_run_grants_request: true,
    list_by_grantor_account_id_run_grants_request: true,
    list_run_grant_versions_request: true,
    get_run_grant_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'iam.v1.run_grants_events.created',
    updated: 'iam.v1.run_grants_events.updated',
    deleted: 'iam.v1.run_grants_events.deleted',
} as const;
