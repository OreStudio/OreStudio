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
import type { TenorBasisTwoSwapConvention } from '../domain/tenor_basis_two_swap_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TenorBasisTwoSwapConventionKey {
    id: string;
}

export interface TenorBasisTwoSwapConventionWrite {
    id: string;
    calendar: string;
    long_fixed_frequency: string;
    long_fixed_convention: string;
    long_fixed_day_count_fraction: string;
    long_index: string;
    short_fixed_frequency: string;
    short_fixed_convention: string;
    short_fixed_day_count_fraction: string;
    short_index: string;
    long_minus_short: boolean | null;
}

export interface TenorBasisTwoSwapConventionChange {
    write: TenorBasisTwoSwapConventionWrite;
    precondition: Precondition;
}

export interface TenorBasisTwoSwapConventionRemoval {
    key: TenorBasisTwoSwapConventionKey;
    precondition: Precondition;
}

export interface TenorBasisTwoSwapConventionLookup {
    key: TenorBasisTwoSwapConventionKey;
    tenor_basis_two_swap_convention: TenorBasisTwoSwapConvention | null;
}

export interface TenorBasisTwoSwapConventionsFilter {
    id_one_of: string[] | null;
}

export interface TenorBasisTwoSwapConventionEvent {
    event_id: string;
    key: TenorBasisTwoSwapConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TenorBasisTwoSwapConventionVersionKey {
    tenor_basis_two_swap_convention: TenorBasisTwoSwapConventionKey;
    version: number;
}

export interface TenorBasisTwoSwapConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTenorBasisTwoSwapConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TenorBasisTwoSwapConventionsFilter | null;
    as_of: string | null;
}

export interface ListTenorBasisTwoSwapConventionsResponse {
    result: Result;
    tenor_basis_two_swap_conventions: TenorBasisTwoSwapConvention[];
    total: number;
}

export interface GetTenorBasisTwoSwapConventionRequest {
    key: TenorBasisTwoSwapConventionKey;
}

export interface GetTenorBasisTwoSwapConventionResponse {
    result: Result;
    tenor_basis_two_swap_convention: TenorBasisTwoSwapConvention | null;
}

export interface GetManyTenorBasisTwoSwapConventionsRequest {
    keys: TenorBasisTwoSwapConventionKey[];
}

export interface GetManyTenorBasisTwoSwapConventionsResponse {
    result: Result;
    entries: TenorBasisTwoSwapConventionLookup[];
}

export interface PutTenorBasisTwoSwapConventionRequest {
    change: TenorBasisTwoSwapConventionChange;
    intent: ChangeIntent;
}

export interface PutTenorBasisTwoSwapConventionResponse {
    result: Result;
    tenor_basis_two_swap_convention: TenorBasisTwoSwapConvention | null;
}

export interface PutManyTenorBasisTwoSwapConventionsRequest {
    changes: TenorBasisTwoSwapConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyTenorBasisTwoSwapConventionsResponse {
    result: Result;
    tenor_basis_two_swap_conventions: TenorBasisTwoSwapConvention[];
}

export interface DeleteTenorBasisTwoSwapConventionRequest {
    removal: TenorBasisTwoSwapConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteTenorBasisTwoSwapConventionResponse {
    result: Result;
}

export interface DeleteManyTenorBasisTwoSwapConventionsRequest {
    removals: TenorBasisTwoSwapConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTenorBasisTwoSwapConventionsResponse {
    result: Result;
}

export interface ListTenorBasisTwoSwapConventionVersionsRequest {
    key: TenorBasisTwoSwapConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorBasisTwoSwapConventionVersionsFilter | null;
}

export interface ListTenorBasisTwoSwapConventionVersionsResponse {
    result: Result;
    versions: TenorBasisTwoSwapConvention[];
    total: number;
}

export interface GetTenorBasisTwoSwapConventionVersionRequest {
    key: TenorBasisTwoSwapConventionVersionKey;
}

export interface GetTenorBasisTwoSwapConventionVersionResponse {
    result: Result;
    version: TenorBasisTwoSwapConvention | null;
}

export const subjects = {
    list_tenor_basis_two_swap_conventions_request:
        'refdata.v1.tenor_basis_two_swap_conventions.list',
    get_tenor_basis_two_swap_convention_request: 'refdata.v1.tenor_basis_two_swap_conventions.get',
    get_many_tenor_basis_two_swap_conventions_request:
        'refdata.v1.tenor_basis_two_swap_conventions.get_many',
    put_tenor_basis_two_swap_convention_request: 'refdata.v1.tenor_basis_two_swap_conventions.put',
    put_many_tenor_basis_two_swap_conventions_request:
        'refdata.v1.tenor_basis_two_swap_conventions.put_many',
    delete_tenor_basis_two_swap_convention_request:
        'refdata.v1.tenor_basis_two_swap_conventions.delete',
    delete_many_tenor_basis_two_swap_conventions_request:
        'refdata.v1.tenor_basis_two_swap_conventions.delete_many',
    list_tenor_basis_two_swap_convention_versions_request:
        'refdata.v1.tenor_basis_two_swap_conventions_versions.list',
    get_tenor_basis_two_swap_convention_version_request:
        'refdata.v1.tenor_basis_two_swap_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_tenor_basis_two_swap_conventions_request: true,
    get_tenor_basis_two_swap_convention_request: true,
    get_many_tenor_basis_two_swap_conventions_request: true,
    put_tenor_basis_two_swap_convention_request: true,
    put_many_tenor_basis_two_swap_conventions_request: true,
    delete_tenor_basis_two_swap_convention_request: true,
    delete_many_tenor_basis_two_swap_conventions_request: true,
    list_tenor_basis_two_swap_convention_versions_request: true,
    get_tenor_basis_two_swap_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.tenor_basis_two_swap_conventions_events.created',
    updated: 'refdata.v1.tenor_basis_two_swap_conventions_events.updated',
    deleted: 'refdata.v1.tenor_basis_two_swap_conventions_events.deleted',
} as const;
