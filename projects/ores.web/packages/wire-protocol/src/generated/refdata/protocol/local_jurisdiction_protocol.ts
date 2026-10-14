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
import type { LocalJurisdiction } from '../domain/local_jurisdiction.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface LocalJurisdictionKey {
    code: string;
    coding_scheme_code: string;
}

export interface LocalJurisdictionWrite {
    code: string;
    coding_scheme_code: string;
    source: string;
    description: string;
}

export interface LocalJurisdictionChange {
    write: LocalJurisdictionWrite;
    precondition: Precondition;
}

export interface LocalJurisdictionRemoval {
    key: LocalJurisdictionKey;
    precondition: Precondition;
}

export interface LocalJurisdictionLookup {
    key: LocalJurisdictionKey;
    local_jurisdiction: LocalJurisdiction | null;
}

export interface LocalJurisdictionEvent {
    event_id: string;
    key: LocalJurisdictionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface LocalJurisdictionVersionKey {
    local_jurisdiction: LocalJurisdictionKey;
    version: number;
}

export interface LocalJurisdictionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListLocalJurisdictionsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListLocalJurisdictionsResponse {
    result: Result;
    jurisdictions: LocalJurisdiction[];
    total: number;
}

export interface GetLocalJurisdictionRequest {
    key: LocalJurisdictionKey;
}

export interface GetLocalJurisdictionResponse {
    result: Result;
    local_jurisdiction: LocalJurisdiction | null;
}

export interface GetManyLocalJurisdictionsRequest {
    keys: LocalJurisdictionKey[];
}

export interface GetManyLocalJurisdictionsResponse {
    result: Result;
    entries: LocalJurisdictionLookup[];
}

export interface PutLocalJurisdictionRequest {
    change: LocalJurisdictionChange;
    intent: ChangeIntent;
}

export interface PutLocalJurisdictionResponse {
    result: Result;
    local_jurisdiction: LocalJurisdiction | null;
}

export interface PutManyLocalJurisdictionsRequest {
    changes: LocalJurisdictionChange[];
    intent: ChangeIntent;
}

export interface PutManyLocalJurisdictionsResponse {
    result: Result;
    jurisdictions: LocalJurisdiction[];
}

export interface DeleteLocalJurisdictionRequest {
    removal: LocalJurisdictionRemoval;
    intent: ChangeIntent;
}

export interface DeleteLocalJurisdictionResponse {
    result: Result;
}

export interface DeleteManyLocalJurisdictionsRequest {
    removals: LocalJurisdictionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyLocalJurisdictionsResponse {
    result: Result;
}

export interface ListLocalJurisdictionVersionsRequest {
    key: LocalJurisdictionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: LocalJurisdictionVersionsFilter | null;
}

export interface ListLocalJurisdictionVersionsResponse {
    result: Result;
    versions: LocalJurisdiction[];
    total: number;
}

export interface GetLocalJurisdictionVersionRequest {
    key: LocalJurisdictionVersionKey;
}

export interface GetLocalJurisdictionVersionResponse {
    result: Result;
    version: LocalJurisdiction | null;
}

export const subjects = {
    list_local_jurisdictions_request: 'refdata.v1.local_jurisdictions.list',
    get_local_jurisdiction_request: 'refdata.v1.local_jurisdictions.get',
    get_many_local_jurisdictions_request: 'refdata.v1.local_jurisdictions.get_many',
    put_local_jurisdiction_request: 'refdata.v1.local_jurisdictions.put',
    put_many_local_jurisdictions_request: 'refdata.v1.local_jurisdictions.put_many',
    delete_local_jurisdiction_request: 'refdata.v1.local_jurisdictions.delete',
    delete_many_local_jurisdictions_request: 'refdata.v1.local_jurisdictions.delete_many',
    list_local_jurisdiction_versions_request: 'refdata.v1.local_jurisdictions_versions.list',
    get_local_jurisdiction_version_request: 'refdata.v1.local_jurisdictions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_local_jurisdictions_request: true,
    get_local_jurisdiction_request: true,
    get_many_local_jurisdictions_request: true,
    put_local_jurisdiction_request: true,
    put_many_local_jurisdictions_request: true,
    delete_local_jurisdiction_request: true,
    delete_many_local_jurisdictions_request: true,
    list_local_jurisdiction_versions_request: true,
    get_local_jurisdiction_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.local_jurisdictions_events.created',
    updated: 'refdata.v1.local_jurisdictions_events.updated',
    deleted: 'refdata.v1.local_jurisdictions_events.deleted',
} as const;
