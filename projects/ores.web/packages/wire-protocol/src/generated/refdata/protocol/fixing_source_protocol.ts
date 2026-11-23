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
import type { FixingSource } from '../domain/fixing_source.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FixingSourceKey {
    code: string;
}

export interface FixingSourceWrite {
    code: string;
    name: string;
    description: string;
    kind_code: string;
    asset_class: string;
    administrator: string;
    publication_time: string;
    publication_time_zone: string;
    calendar_code: string;
    rounding_decimals: number;
    status_code: string;
    display_order: number;
}

export interface FixingSourceChange {
    write: FixingSourceWrite;
    precondition: Precondition;
}

export interface FixingSourceRemoval {
    key: FixingSourceKey;
    precondition: Precondition;
}

export interface FixingSourceLookup {
    key: FixingSourceKey;
    fixing_source: FixingSource | null;
}

export interface FixingSourcesFilter {
    code_one_of: string[] | null;
}

export interface FixingSourceEvent {
    event_id: string;
    key: FixingSourceKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FixingSourceVersionKey {
    fixing_source: FixingSourceKey;
    version: number;
}

export interface FixingSourceVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFixingSourcesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FixingSourcesFilter | null;
    as_of: string | null;
}

export interface ListFixingSourcesResponse {
    result: Result;
    sources: FixingSource[];
    total: number;
}

export interface GetFixingSourceRequest {
    key: FixingSourceKey;
}

export interface GetFixingSourceResponse {
    result: Result;
    fixing_source: FixingSource | null;
}

export interface GetManyFixingSourcesRequest {
    keys: FixingSourceKey[];
}

export interface GetManyFixingSourcesResponse {
    result: Result;
    entries: FixingSourceLookup[];
}

export interface PutFixingSourceRequest {
    change: FixingSourceChange;
    intent: ChangeIntent;
}

export interface PutFixingSourceResponse {
    result: Result;
    fixing_source: FixingSource | null;
}

export interface PutManyFixingSourcesRequest {
    changes: FixingSourceChange[];
    intent: ChangeIntent;
}

export interface PutManyFixingSourcesResponse {
    result: Result;
    sources: FixingSource[];
}

export interface DeleteFixingSourceRequest {
    removal: FixingSourceRemoval;
    intent: ChangeIntent;
}

export interface DeleteFixingSourceResponse {
    result: Result;
}

export interface DeleteManyFixingSourcesRequest {
    removals: FixingSourceRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFixingSourcesResponse {
    result: Result;
}

export interface ListFixingSourceVersionsRequest {
    key: FixingSourceKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FixingSourceVersionsFilter | null;
}

export interface ListFixingSourceVersionsResponse {
    result: Result;
    versions: FixingSource[];
    total: number;
}

export interface GetFixingSourceVersionRequest {
    key: FixingSourceVersionKey;
}

export interface GetFixingSourceVersionResponse {
    result: Result;
    version: FixingSource | null;
}

export const subjects = {
    list_fixing_sources_request: 'refdata.v1.fixing_sources.list',
    get_fixing_source_request: 'refdata.v1.fixing_sources.get',
    get_many_fixing_sources_request: 'refdata.v1.fixing_sources.get_many',
    put_fixing_source_request: 'refdata.v1.fixing_sources.put',
    put_many_fixing_sources_request: 'refdata.v1.fixing_sources.put_many',
    delete_fixing_source_request: 'refdata.v1.fixing_sources.delete',
    delete_many_fixing_sources_request: 'refdata.v1.fixing_sources.delete_many',
    list_fixing_source_versions_request: 'refdata.v1.fixing_sources_versions.list',
    get_fixing_source_version_request: 'refdata.v1.fixing_sources_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fixing_sources_request: true,
    get_fixing_source_request: true,
    get_many_fixing_sources_request: true,
    put_fixing_source_request: true,
    put_many_fixing_sources_request: true,
    delete_fixing_source_request: true,
    delete_many_fixing_sources_request: true,
    list_fixing_source_versions_request: true,
    get_fixing_source_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.fixing_sources.created',
    updated: 'refdata.v1.events.fixing_sources.updated',
    deleted: 'refdata.v1.events.fixing_sources.deleted',
} as const;
