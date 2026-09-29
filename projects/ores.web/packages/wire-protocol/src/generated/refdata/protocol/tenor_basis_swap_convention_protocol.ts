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
import type { TenorBasisSwapConvention } from '../domain/tenor_basis_swap_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TenorBasisSwapConventionKey {
    id: string;
}

export interface TenorBasisSwapConventionWrite {
    id: string;
    pay_index: string | null;
    pay_frequency: string | null;
    receive_index: string | null;
    receive_frequency: string | null;
    spread_on_rec: boolean | null;
    include_spread: boolean | null;
    sub_periods_coupon_type: string | null;
    pay_is_averaged: boolean | null;
    rec_is_averaged: boolean | null;
    long_index: string | null;
    long_pay_tenor: string | null;
    short_index: string | null;
    short_pay_tenor: string | null;
    spread_on_short: boolean | null;
}

export interface TenorBasisSwapConventionChange {
    write: TenorBasisSwapConventionWrite;
    precondition: Precondition;
}

export interface TenorBasisSwapConventionRemoval {
    key: TenorBasisSwapConventionKey;
    precondition: Precondition;
}

export interface TenorBasisSwapConventionLookup {
    key: TenorBasisSwapConventionKey;
    tenor_basis_swap_convention: TenorBasisSwapConvention | null;
}

export interface TenorBasisSwapConventionEvent {
    event_id: string;
    key: TenorBasisSwapConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TenorBasisSwapConventionVersionKey {
    tenor_basis_swap_convention: TenorBasisSwapConventionKey;
    version: number;
}

export interface TenorBasisSwapConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTenorBasisSwapConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTenorBasisSwapConventionsResponse {
    result: Result;
    tenor_basis_swap_conventions: TenorBasisSwapConvention[];
    total: number;
}

export interface GetTenorBasisSwapConventionRequest {
    key: TenorBasisSwapConventionKey;
}

export interface GetTenorBasisSwapConventionResponse {
    result: Result;
    tenor_basis_swap_convention: TenorBasisSwapConvention | null;
}

export interface GetManyTenorBasisSwapConventionsRequest {
    keys: TenorBasisSwapConventionKey[];
}

export interface GetManyTenorBasisSwapConventionsResponse {
    result: Result;
    entries: TenorBasisSwapConventionLookup[];
}

export interface PutTenorBasisSwapConventionRequest {
    change: TenorBasisSwapConventionChange;
    intent: ChangeIntent;
}

export interface PutTenorBasisSwapConventionResponse {
    result: Result;
    tenor_basis_swap_convention: TenorBasisSwapConvention | null;
}

export interface PutManyTenorBasisSwapConventionsRequest {
    changes: TenorBasisSwapConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyTenorBasisSwapConventionsResponse {
    result: Result;
    tenor_basis_swap_conventions: TenorBasisSwapConvention[];
}

export interface DeleteTenorBasisSwapConventionRequest {
    removal: TenorBasisSwapConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteTenorBasisSwapConventionResponse {
    result: Result;
}

export interface DeleteManyTenorBasisSwapConventionsRequest {
    removals: TenorBasisSwapConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTenorBasisSwapConventionsResponse {
    result: Result;
}

export interface ListTenorBasisSwapConventionVersionsRequest {
    key: TenorBasisSwapConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorBasisSwapConventionVersionsFilter | null;
}

export interface ListTenorBasisSwapConventionVersionsResponse {
    result: Result;
    versions: TenorBasisSwapConvention[];
    total: number;
}

export interface GetTenorBasisSwapConventionVersionRequest {
    key: TenorBasisSwapConventionVersionKey;
}

export interface GetTenorBasisSwapConventionVersionResponse {
    result: Result;
    version: TenorBasisSwapConvention | null;
}

export const subjects = {
    list_tenor_basis_swap_conventions_request: 'refdata.v1.tenor_basis_swap_conventions.list',
    get_tenor_basis_swap_convention_request: 'refdata.v1.tenor_basis_swap_conventions.get',
    get_many_tenor_basis_swap_conventions_request:
        'refdata.v1.tenor_basis_swap_conventions.get_many',
    put_tenor_basis_swap_convention_request: 'refdata.v1.tenor_basis_swap_conventions.put',
    put_many_tenor_basis_swap_conventions_request:
        'refdata.v1.tenor_basis_swap_conventions.put_many',
    delete_tenor_basis_swap_convention_request: 'refdata.v1.tenor_basis_swap_conventions.delete',
    delete_many_tenor_basis_swap_conventions_request:
        'refdata.v1.tenor_basis_swap_conventions.delete_many',
    list_tenor_basis_swap_convention_versions_request:
        'refdata.v1.tenor_basis_swap_conventions_versions.list',
    get_tenor_basis_swap_convention_version_request:
        'refdata.v1.tenor_basis_swap_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_tenor_basis_swap_conventions_request: true,
    get_tenor_basis_swap_convention_request: true,
    get_many_tenor_basis_swap_conventions_request: true,
    put_tenor_basis_swap_convention_request: true,
    put_many_tenor_basis_swap_conventions_request: true,
    delete_tenor_basis_swap_convention_request: true,
    delete_many_tenor_basis_swap_conventions_request: true,
    list_tenor_basis_swap_convention_versions_request: true,
    get_tenor_basis_swap_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.tenor_basis_swap_conventions_events.created',
    updated: 'refdata.v1.tenor_basis_swap_conventions_events.updated',
    deleted: 'refdata.v1.tenor_basis_swap_conventions_events.deleted',
} as const;
