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
import type { Tenor } from '../domain/tenor.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TenorKey {
    code: string;
}

export interface TenorWrite {
    code: string;
    display_name: string;
    description: string;
    sort_order: number;
    kind: string;
    unit: string;
    multiplier: number | null;
}

export interface TenorChange {
    write: TenorWrite;
    precondition: Precondition;
}

export interface TenorRemoval {
    key: TenorKey;
    precondition: Precondition;
}

export interface TenorLookup {
    key: TenorKey;
    tenor: Tenor | null;
}

export interface TenorEvent {
    event_id: string;
    key: TenorKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TenorVersionKey {
    tenor: TenorKey;
    version: number;
}

export interface TenorVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTenorsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTenorsResponse {
    result: Result;
    tenors: Tenor[];
    total: number;
}

export interface GetTenorRequest {
    key: TenorKey;
}

export interface GetTenorResponse {
    result: Result;
    tenor: Tenor | null;
}

export interface GetManyTenorsRequest {
    keys: TenorKey[];
}

export interface GetManyTenorsResponse {
    result: Result;
    entries: TenorLookup[];
}

export interface PutTenorRequest {
    change: TenorChange;
    intent: ChangeIntent;
}

export interface PutTenorResponse {
    result: Result;
    tenor: Tenor;
}

export interface PutManyTenorsRequest {
    changes: TenorChange[];
    intent: ChangeIntent;
}

export interface PutManyTenorsResponse {
    result: Result;
    tenors: Tenor[];
}

export interface DeleteTenorRequest {
    removal: TenorRemoval;
    intent: ChangeIntent;
}

export interface DeleteTenorResponse {
    result: Result;
}

export interface DeleteManyTenorsRequest {
    removals: TenorRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTenorsResponse {
    result: Result;
}

export interface ListTenorVersionsRequest {
    key: TenorKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorVersionsFilter | null;
}

export interface ListTenorVersionsResponse {
    result: Result;
    versions: Tenor[];
    total: number;
}

export interface GetTenorVersionRequest {
    key: TenorVersionKey;
}

export interface GetTenorVersionResponse {
    result: Result;
    version: Tenor;
}

export const subjects = {
    list_tenors_request: "refdata.v1.tenors.list",
    get_tenor_request: "refdata.v1.tenors.get",
    get_many_tenors_request: "refdata.v1.tenors.get_many",
    put_tenor_request: "refdata.v1.tenors.put",
    put_many_tenors_request: "refdata.v1.tenors.put_many",
    delete_tenor_request: "refdata.v1.tenors.delete",
    delete_many_tenors_request: "refdata.v1.tenors.delete_many",
    list_tenor_versions_request: "refdata.v1.tenors_versions.list",
    get_tenor_version_request: "refdata.v1.tenors_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_tenors_request: true,
    get_tenor_request: true,
    get_many_tenors_request: true,
    put_tenor_request: true,
    put_many_tenors_request: true,
    delete_tenor_request: true,
    delete_many_tenors_request: true,
    list_tenor_versions_request: true,
    get_tenor_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.tenors_events.created",
    updated: "refdata.v1.tenors_events.updated",
    deleted: "refdata.v1.tenors_events.deleted",
} as const;
