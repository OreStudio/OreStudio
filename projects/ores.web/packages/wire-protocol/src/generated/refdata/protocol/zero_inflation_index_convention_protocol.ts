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
import type { ZeroInflationIndexConvention } from '../domain/zero_inflation_index_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ZeroInflationIndexConventionKey {
    id: string;
}

export interface ZeroInflationIndexConventionWrite {
    id: string;
    region_name: string;
    region_code: string;
    revised: boolean;
    frequency: string;
    availability_lag: string;
    currency: string;
    oresmd_uri: string | null;
}

export interface ZeroInflationIndexConventionChange {
    write: ZeroInflationIndexConventionWrite;
    precondition: Precondition;
}

export interface ZeroInflationIndexConventionRemoval {
    key: ZeroInflationIndexConventionKey;
    precondition: Precondition;
}

export interface ZeroInflationIndexConventionLookup {
    key: ZeroInflationIndexConventionKey;
    zero_inflation_index_convention: ZeroInflationIndexConvention | null;
}

export interface ZeroInflationIndexConventionsFilter {
    id_one_of: string[] | null;
}

export interface ZeroInflationIndexConventionEvent {
    event_id: string;
    key: ZeroInflationIndexConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ZeroInflationIndexConventionVersionKey {
    zero_inflation_index_convention: ZeroInflationIndexConventionKey;
    version: number;
}

export interface ZeroInflationIndexConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListZeroInflationIndexConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ZeroInflationIndexConventionsFilter | null;
    as_of: string | null;
}

export interface ListZeroInflationIndexConventionsResponse {
    result: Result;
    zero_inflation_index_conventions: ZeroInflationIndexConvention[];
    total: number;
}

export interface GetZeroInflationIndexConventionRequest {
    key: ZeroInflationIndexConventionKey;
}

export interface GetZeroInflationIndexConventionResponse {
    result: Result;
    zero_inflation_index_convention: ZeroInflationIndexConvention | null;
}

export interface GetManyZeroInflationIndexConventionsRequest {
    keys: ZeroInflationIndexConventionKey[];
}

export interface GetManyZeroInflationIndexConventionsResponse {
    result: Result;
    entries: ZeroInflationIndexConventionLookup[];
}

export interface PutZeroInflationIndexConventionRequest {
    change: ZeroInflationIndexConventionChange;
    intent: ChangeIntent;
}

export interface PutZeroInflationIndexConventionResponse {
    result: Result;
    zero_inflation_index_convention: ZeroInflationIndexConvention | null;
}

export interface PutManyZeroInflationIndexConventionsRequest {
    changes: ZeroInflationIndexConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyZeroInflationIndexConventionsResponse {
    result: Result;
    zero_inflation_index_conventions: ZeroInflationIndexConvention[];
}

export interface DeleteZeroInflationIndexConventionRequest {
    removal: ZeroInflationIndexConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteZeroInflationIndexConventionResponse {
    result: Result;
}

export interface DeleteManyZeroInflationIndexConventionsRequest {
    removals: ZeroInflationIndexConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyZeroInflationIndexConventionsResponse {
    result: Result;
}

export interface ListZeroInflationIndexConventionVersionsRequest {
    key: ZeroInflationIndexConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ZeroInflationIndexConventionVersionsFilter | null;
}

export interface ListZeroInflationIndexConventionVersionsResponse {
    result: Result;
    versions: ZeroInflationIndexConvention[];
    total: number;
}

export interface GetZeroInflationIndexConventionVersionRequest {
    key: ZeroInflationIndexConventionVersionKey;
}

export interface GetZeroInflationIndexConventionVersionResponse {
    result: Result;
    version: ZeroInflationIndexConvention | null;
}

export const subjects = {
    list_zero_inflation_index_conventions_request:
        'refdata.v1.zero_inflation_index_conventions.list',
    get_zero_inflation_index_convention_request: 'refdata.v1.zero_inflation_index_conventions.get',
    get_many_zero_inflation_index_conventions_request:
        'refdata.v1.zero_inflation_index_conventions.get_many',
    put_zero_inflation_index_convention_request: 'refdata.v1.zero_inflation_index_conventions.put',
    put_many_zero_inflation_index_conventions_request:
        'refdata.v1.zero_inflation_index_conventions.put_many',
    delete_zero_inflation_index_convention_request:
        'refdata.v1.zero_inflation_index_conventions.delete',
    delete_many_zero_inflation_index_conventions_request:
        'refdata.v1.zero_inflation_index_conventions.delete_many',
    list_zero_inflation_index_convention_versions_request:
        'refdata.v1.zero_inflation_index_conventions_versions.list',
    get_zero_inflation_index_convention_version_request:
        'refdata.v1.zero_inflation_index_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_zero_inflation_index_conventions_request: true,
    get_zero_inflation_index_convention_request: true,
    get_many_zero_inflation_index_conventions_request: true,
    put_zero_inflation_index_convention_request: true,
    put_many_zero_inflation_index_conventions_request: true,
    delete_zero_inflation_index_convention_request: true,
    delete_many_zero_inflation_index_conventions_request: true,
    list_zero_inflation_index_convention_versions_request: true,
    get_zero_inflation_index_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.zero_inflation_index_conventions_events.created',
    updated: 'refdata.v1.zero_inflation_index_conventions_events.updated',
    deleted: 'refdata.v1.zero_inflation_index_conventions_events.deleted',
} as const;
