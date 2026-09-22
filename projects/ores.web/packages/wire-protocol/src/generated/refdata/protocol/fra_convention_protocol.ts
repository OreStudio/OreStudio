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
import type { FraConvention } from '../domain/fra_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FraConventionKey {
    id: string;
}

export interface FraConventionWrite {
    id: string;
    index: string;
}

export interface FraConventionChange {
    write: FraConventionWrite;
    precondition: Precondition;
}

export interface FraConventionRemoval {
    key: FraConventionKey;
    precondition: Precondition;
}

export interface FraConventionLookup {
    key: FraConventionKey;
    fra_convention: FraConvention | null;
}

export interface FraConventionEvent {
    event_id: string;
    key: FraConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FraConventionVersionKey {
    fra_convention: FraConventionKey;
    version: number;
}

export interface FraConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFraConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListFraConventionsResponse {
    result: Result;
    fra_conventions: FraConvention[];
    total: number;
}

export interface GetFraConventionRequest {
    key: FraConventionKey;
}

export interface GetFraConventionResponse {
    result: Result;
    fra_convention: FraConvention | null;
}

export interface GetManyFraConventionsRequest {
    keys: FraConventionKey[];
}

export interface GetManyFraConventionsResponse {
    result: Result;
    entries: FraConventionLookup[];
}

export interface PutFraConventionRequest {
    change: FraConventionChange;
    intent: ChangeIntent;
}

export interface PutFraConventionResponse {
    result: Result;
    fra_convention: FraConvention;
}

export interface PutManyFraConventionsRequest {
    changes: FraConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyFraConventionsResponse {
    result: Result;
    fra_conventions: FraConvention[];
}

export interface DeleteFraConventionRequest {
    removal: FraConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteFraConventionResponse {
    result: Result;
}

export interface DeleteManyFraConventionsRequest {
    removals: FraConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFraConventionsResponse {
    result: Result;
}

export interface ListFraConventionVersionsRequest {
    key: FraConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FraConventionVersionsFilter | null;
}

export interface ListFraConventionVersionsResponse {
    result: Result;
    versions: FraConvention[];
    total: number;
}

export interface GetFraConventionVersionRequest {
    key: FraConventionVersionKey;
}

export interface GetFraConventionVersionResponse {
    result: Result;
    version: FraConvention;
}

export const subjects = {
    list_fra_conventions_request: "refdata.v1.fra_conventions.list",
    get_fra_convention_request: "refdata.v1.fra_conventions.get",
    get_many_fra_conventions_request: "refdata.v1.fra_conventions.get_many",
    put_fra_convention_request: "refdata.v1.fra_conventions.put",
    put_many_fra_conventions_request: "refdata.v1.fra_conventions.put_many",
    delete_fra_convention_request: "refdata.v1.fra_conventions.delete",
    delete_many_fra_conventions_request: "refdata.v1.fra_conventions.delete_many",
    list_fra_convention_versions_request: "refdata.v1.fra_conventions_versions.list",
    get_fra_convention_version_request: "refdata.v1.fra_conventions_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fra_conventions_request: true,
    get_fra_convention_request: true,
    get_many_fra_conventions_request: true,
    put_fra_convention_request: true,
    put_many_fra_conventions_request: true,
    delete_fra_convention_request: true,
    delete_many_fra_conventions_request: true,
    list_fra_convention_versions_request: true,
    get_fra_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.fra_conventions_events.created",
    updated: "refdata.v1.fra_conventions_events.updated",
    deleted: "refdata.v1.fra_conventions_events.deleted",
} as const;
