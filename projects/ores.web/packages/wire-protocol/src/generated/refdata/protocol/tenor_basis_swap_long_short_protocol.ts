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
import type { TenorBasisSwapLongShort } from '../domain/tenor_basis_swap_long_short.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface TenorBasisSwapLongShortKey {
    id: string;
}

export interface TenorBasisSwapLongShortWrite {
    id: string;
    convention_id: string;
    long_index: string | null;
    long_pay_tenor: string | null;
    short_index: string | null;
    short_pay_tenor: string | null;
    spread_on_short: boolean | null;
    include_spread: boolean | null;
    sub_periods_coupon_type: string | null;
}

export interface TenorBasisSwapLongShortChange {
    write: TenorBasisSwapLongShortWrite;
    precondition: Precondition;
}

export interface TenorBasisSwapLongShortRemoval {
    key: TenorBasisSwapLongShortKey;
    precondition: Precondition;
}

export interface TenorBasisSwapLongShortLookup {
    key: TenorBasisSwapLongShortKey;
    tenor_basis_swap_long_short: TenorBasisSwapLongShort | null;
}

export interface TenorBasisSwapLongShortsFilter {
    convention_id: string | null;
    id_one_of: string[] | null;
    convention_id_one_of: string[] | null;
}

export interface TenorBasisSwapLongShortEvent {
    event_id: string;
    key: TenorBasisSwapLongShortKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TenorBasisSwapLongShortVersionKey {
    tenor_basis_swap_long_short: TenorBasisSwapLongShortKey;
    version: number;
}

export interface TenorBasisSwapLongShortVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTenorBasisSwapLongShortsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TenorBasisSwapLongShortsFilter | null;
    as_of: string | null;
}

export interface ListTenorBasisSwapLongShortsResponse {
    result: Result;
    tenor_basis_swap_long_shorts: TenorBasisSwapLongShort[];
    total: number;
}

export interface GetTenorBasisSwapLongShortRequest {
    key: TenorBasisSwapLongShortKey;
}

export interface GetTenorBasisSwapLongShortResponse {
    result: Result;
    tenor_basis_swap_long_short: TenorBasisSwapLongShort | null;
}

export interface GetManyTenorBasisSwapLongShortsRequest {
    keys: TenorBasisSwapLongShortKey[];
}

export interface GetManyTenorBasisSwapLongShortsResponse {
    result: Result;
    entries: TenorBasisSwapLongShortLookup[];
}

export interface PutTenorBasisSwapLongShortRequest {
    change: TenorBasisSwapLongShortChange;
    intent: ChangeIntent;
}

export interface PutTenorBasisSwapLongShortResponse {
    result: Result;
    tenor_basis_swap_long_short: TenorBasisSwapLongShort | null;
}

export interface PutManyTenorBasisSwapLongShortsRequest {
    changes: TenorBasisSwapLongShortChange[];
    intent: ChangeIntent;
}

export interface PutManyTenorBasisSwapLongShortsResponse {
    result: Result;
    tenor_basis_swap_long_shorts: TenorBasisSwapLongShort[];
}

export interface DeleteTenorBasisSwapLongShortRequest {
    removal: TenorBasisSwapLongShortRemoval;
    intent: ChangeIntent;
}

export interface DeleteTenorBasisSwapLongShortResponse {
    result: Result;
}

export interface DeleteManyTenorBasisSwapLongShortsRequest {
    removals: TenorBasisSwapLongShortRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTenorBasisSwapLongShortsResponse {
    result: Result;
}

export interface ListByConventionIdTenorBasisSwapLongShortsRequest {
    convention_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorBasisSwapLongShortsFilter | null;
}

export interface ListByConventionIdTenorBasisSwapLongShortsResponse {
    result: Result;
    tenor_basis_swap_long_shorts: TenorBasisSwapLongShort[];
    total: number;
}

export interface ListTenorBasisSwapLongShortVersionsRequest {
    key: TenorBasisSwapLongShortKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorBasisSwapLongShortVersionsFilter | null;
}

export interface ListTenorBasisSwapLongShortVersionsResponse {
    result: Result;
    versions: TenorBasisSwapLongShort[];
    total: number;
}

export interface GetTenorBasisSwapLongShortVersionRequest {
    key: TenorBasisSwapLongShortVersionKey;
}

export interface GetTenorBasisSwapLongShortVersionResponse {
    result: Result;
    version: TenorBasisSwapLongShort | null;
}

export const subjects = {
    list_tenor_basis_swap_long_shorts_request: 'refdata.v1.tenor_basis_swap_long_shorts.list',
    get_tenor_basis_swap_long_short_request: 'refdata.v1.tenor_basis_swap_long_shorts.get',
    get_many_tenor_basis_swap_long_shorts_request:
        'refdata.v1.tenor_basis_swap_long_shorts.get_many',
    put_tenor_basis_swap_long_short_request: 'refdata.v1.tenor_basis_swap_long_shorts.put',
    put_many_tenor_basis_swap_long_shorts_request:
        'refdata.v1.tenor_basis_swap_long_shorts.put_many',
    delete_tenor_basis_swap_long_short_request: 'refdata.v1.tenor_basis_swap_long_shorts.delete',
    delete_many_tenor_basis_swap_long_shorts_request:
        'refdata.v1.tenor_basis_swap_long_shorts.delete_many',
    list_by_convention_id_tenor_basis_swap_long_shorts_request:
        'refdata.v1.tenor_basis_swap_long_shorts.list_by_convention_id',
    list_tenor_basis_swap_long_short_versions_request:
        'refdata.v1.tenor_basis_swap_long_shorts_versions.list',
    get_tenor_basis_swap_long_short_version_request:
        'refdata.v1.tenor_basis_swap_long_shorts_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_tenor_basis_swap_long_shorts_request: true,
    get_tenor_basis_swap_long_short_request: true,
    get_many_tenor_basis_swap_long_shorts_request: true,
    put_tenor_basis_swap_long_short_request: true,
    put_many_tenor_basis_swap_long_shorts_request: true,
    delete_tenor_basis_swap_long_short_request: true,
    delete_many_tenor_basis_swap_long_shorts_request: true,
    list_by_convention_id_tenor_basis_swap_long_shorts_request: true,
    list_tenor_basis_swap_long_short_versions_request: true,
    get_tenor_basis_swap_long_short_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.tenor_basis_swap_long_shorts_events.created',
    updated: 'refdata.v1.tenor_basis_swap_long_shorts_events.updated',
    deleted: 'refdata.v1.tenor_basis_swap_long_shorts_events.deleted',
} as const;
