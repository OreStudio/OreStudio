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
import type { TenorConvention } from '../domain/tenor_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TenorConventionKey {
    code: string;
}

export interface TenorConventionWrite {
    code: string;
    description: string;
    measured_from: string;
    resolution_algorithm: string;
}

export interface TenorConventionChange {
    write: TenorConventionWrite;
    precondition: Precondition;
}

export interface TenorConventionRemoval {
    key: TenorConventionKey;
    precondition: Precondition;
}

export interface TenorConventionLookup {
    key: TenorConventionKey;
    tenor_convention: TenorConvention | null;
}

export interface TenorConventionEvent {
    event_id: string;
    key: TenorConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TenorConventionVersionKey {
    tenor_convention: TenorConventionKey;
    version: number;
}

export interface TenorConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTenorConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTenorConventionsResponse {
    result: Result;
    conventions: TenorConvention[];
    total: number;
}

export interface GetTenorConventionRequest {
    key: TenorConventionKey;
}

export interface GetTenorConventionResponse {
    result: Result;
    tenor_convention: TenorConvention | null;
}

export interface GetManyTenorConventionsRequest {
    keys: TenorConventionKey[];
}

export interface GetManyTenorConventionsResponse {
    result: Result;
    entries: TenorConventionLookup[];
}

export interface PutTenorConventionRequest {
    change: TenorConventionChange;
    intent: ChangeIntent;
}

export interface PutTenorConventionResponse {
    result: Result;
    tenor_convention: TenorConvention;
}

export interface PutManyTenorConventionsRequest {
    changes: TenorConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyTenorConventionsResponse {
    result: Result;
    conventions: TenorConvention[];
}

export interface DeleteTenorConventionRequest {
    removal: TenorConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteTenorConventionResponse {
    result: Result;
}

export interface DeleteManyTenorConventionsRequest {
    removals: TenorConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTenorConventionsResponse {
    result: Result;
}

export interface ListTenorConventionVersionsRequest {
    key: TenorConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorConventionVersionsFilter | null;
}

export interface ListTenorConventionVersionsResponse {
    result: Result;
    versions: TenorConvention[];
    total: number;
}

export interface GetTenorConventionVersionRequest {
    key: TenorConventionVersionKey;
}

export interface GetTenorConventionVersionResponse {
    result: Result;
    version: TenorConvention;
}

export const subjects = {
    list_tenor_conventions_request: "refdata.v1.tenor_conventions.list",
    get_tenor_convention_request: "refdata.v1.tenor_conventions.get",
    get_many_tenor_conventions_request: "refdata.v1.tenor_conventions.get_many",
    put_tenor_convention_request: "refdata.v1.tenor_conventions.put",
    put_many_tenor_conventions_request: "refdata.v1.tenor_conventions.put_many",
    delete_tenor_convention_request: "refdata.v1.tenor_conventions.delete",
    delete_many_tenor_conventions_request: "refdata.v1.tenor_conventions.delete_many",
    list_tenor_convention_versions_request: "refdata.v1.tenor_conventions_versions.list",
    get_tenor_convention_version_request: "refdata.v1.tenor_conventions_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_tenor_conventions_request: true,
    get_tenor_convention_request: true,
    get_many_tenor_conventions_request: true,
    put_tenor_convention_request: true,
    put_many_tenor_conventions_request: true,
    delete_tenor_convention_request: true,
    delete_many_tenor_conventions_request: true,
    list_tenor_convention_versions_request: true,
    get_tenor_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.tenor_conventions_events.created",
    updated: "refdata.v1.tenor_conventions_events.updated",
    deleted: "refdata.v1.tenor_conventions_events.deleted",
} as const;
