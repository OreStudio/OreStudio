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
import type { OreFileVersion } from '../domain/ore_file_version.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface OreFileVersionKey {
    id: string;
}

export interface OreFileVersionWrite {
    id: string;
    ore_file_code: string;
    engine_version: string;
    content: string;
    sha256: string;
    bytes: number;
}

export interface OreFileVersionChange {
    write: OreFileVersionWrite;
    precondition: Precondition;
}

export interface OreFileVersionRemoval {
    key: OreFileVersionKey;
    precondition: Precondition;
}

export interface OreFileVersionLookup {
    key: OreFileVersionKey;
    ore_file_version: OreFileVersion | null;
}

export interface OreFileVersionsFilter {
    ore_file_code: string | null;
    engine_version: string | null;
    id_one_of: string[] | null;
    ore_file_code_one_of: string[] | null;
    engine_version_one_of: string[] | null;
}

export interface OreFileVersionEvent {
    event_id: string;
    key: OreFileVersionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface OreFileVersionVersionKey {
    ore_file_version: OreFileVersionKey;
    version: number;
}

export interface OreFileVersionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListOreFileVersionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: OreFileVersionsFilter | null;
    as_of: string | null;
}

export interface ListOreFileVersionsResponse {
    result: Result;
    versions: OreFileVersion[];
    total: number;
}

export interface GetOreFileVersionRequest {
    key: OreFileVersionKey;
}

export interface GetOreFileVersionResponse {
    result: Result;
    ore_file_version: OreFileVersion | null;
}

export interface GetManyOreFileVersionsRequest {
    keys: OreFileVersionKey[];
}

export interface GetManyOreFileVersionsResponse {
    result: Result;
    entries: OreFileVersionLookup[];
}

export interface PutOreFileVersionRequest {
    change: OreFileVersionChange;
    intent: ChangeIntent;
}

export interface PutOreFileVersionResponse {
    result: Result;
    ore_file_version: OreFileVersion | null;
}

export interface PutManyOreFileVersionsRequest {
    changes: OreFileVersionChange[];
    intent: ChangeIntent;
}

export interface PutManyOreFileVersionsResponse {
    result: Result;
    versions: OreFileVersion[];
}

export interface DeleteOreFileVersionRequest {
    removal: OreFileVersionRemoval;
    intent: ChangeIntent;
}

export interface DeleteOreFileVersionResponse {
    result: Result;
}

export interface DeleteManyOreFileVersionsRequest {
    removals: OreFileVersionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyOreFileVersionsResponse {
    result: Result;
}

export interface ListByOreFileCodeOreFileVersionsRequest {
    ore_file_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: OreFileVersionsFilter | null;
}

export interface ListByOreFileCodeOreFileVersionsResponse {
    result: Result;
    versions: OreFileVersion[];
    total: number;
}

export interface ListOreFileVersionVersionsRequest {
    key: OreFileVersionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: OreFileVersionVersionsFilter | null;
}

export interface ListOreFileVersionVersionsResponse {
    result: Result;
    versions: OreFileVersion[];
    total: number;
}

export interface GetOreFileVersionVersionRequest {
    key: OreFileVersionVersionKey;
}

export interface GetOreFileVersionVersionResponse {
    result: Result;
    version: OreFileVersion | null;
}

export const subjects = {
    list_ore_file_versions_request: 'reporting.v1.ore_file_versions.list',
    get_ore_file_version_request: 'reporting.v1.ore_file_versions.get',
    get_many_ore_file_versions_request: 'reporting.v1.ore_file_versions.get_many',
    put_ore_file_version_request: 'reporting.v1.ore_file_versions.put',
    put_many_ore_file_versions_request: 'reporting.v1.ore_file_versions.put_many',
    delete_ore_file_version_request: 'reporting.v1.ore_file_versions.delete',
    delete_many_ore_file_versions_request: 'reporting.v1.ore_file_versions.delete_many',
    list_by_ore_file_code_ore_file_versions_request:
        'reporting.v1.ore_file_versions.list_by_ore_file_code',
    list_ore_file_version_versions_request: 'reporting.v1.ore_file_versions_versions.list',
    get_ore_file_version_version_request: 'reporting.v1.ore_file_versions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ore_file_versions_request: true,
    get_ore_file_version_request: true,
    get_many_ore_file_versions_request: true,
    put_ore_file_version_request: true,
    put_many_ore_file_versions_request: true,
    delete_ore_file_version_request: true,
    delete_many_ore_file_versions_request: true,
    list_by_ore_file_code_ore_file_versions_request: true,
    list_ore_file_version_versions_request: true,
    get_ore_file_version_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.ore_file_versions_events.created',
    updated: 'reporting.v1.ore_file_versions_events.updated',
    deleted: 'reporting.v1.ore_file_versions_events.deleted',
} as const;
