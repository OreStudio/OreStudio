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
import type { BmaBasisSwapConvention } from '../domain/bma_basis_swap_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BmaBasisSwapConventionKey {
    id: string;
}

export interface BmaBasisSwapConventionWrite {
    id: string;
    index: string;
    bma_index: string;
    bma_payment_calendar: string | null;
    bma_payment_convention: string | null;
    bma_payment_lag: number | null;
    index_payment_calendar: string | null;
    index_payment_convention: string | null;
    index_payment_lag: number | null;
    index_settlement_days: number | null;
    index_payment_period: string | null;
    overnight_lockout_days: number | null;
}

export interface BmaBasisSwapConventionChange {
    write: BmaBasisSwapConventionWrite;
    precondition: Precondition;
}

export interface BmaBasisSwapConventionRemoval {
    key: BmaBasisSwapConventionKey;
    precondition: Precondition;
}

export interface BmaBasisSwapConventionLookup {
    key: BmaBasisSwapConventionKey;
    bma_basis_swap_convention: BmaBasisSwapConvention | null;
}

export interface BmaBasisSwapConventionsFilter {
    id_one_of: string[] | null;
}

export interface BmaBasisSwapConventionEvent {
    event_id: string;
    key: BmaBasisSwapConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BmaBasisSwapConventionVersionKey {
    bma_basis_swap_convention: BmaBasisSwapConventionKey;
    version: number;
}

export interface BmaBasisSwapConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBmaBasisSwapConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BmaBasisSwapConventionsFilter | null;
    as_of: string | null;
}

export interface ListBmaBasisSwapConventionsResponse {
    result: Result;
    bma_basis_swap_conventions: BmaBasisSwapConvention[];
    total: number;
}

export interface GetBmaBasisSwapConventionRequest {
    key: BmaBasisSwapConventionKey;
}

export interface GetBmaBasisSwapConventionResponse {
    result: Result;
    bma_basis_swap_convention: BmaBasisSwapConvention | null;
}

export interface GetManyBmaBasisSwapConventionsRequest {
    keys: BmaBasisSwapConventionKey[];
}

export interface GetManyBmaBasisSwapConventionsResponse {
    result: Result;
    entries: BmaBasisSwapConventionLookup[];
}

export interface PutBmaBasisSwapConventionRequest {
    change: BmaBasisSwapConventionChange;
    intent: ChangeIntent;
}

export interface PutBmaBasisSwapConventionResponse {
    result: Result;
    bma_basis_swap_convention: BmaBasisSwapConvention | null;
}

export interface PutManyBmaBasisSwapConventionsRequest {
    changes: BmaBasisSwapConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyBmaBasisSwapConventionsResponse {
    result: Result;
    bma_basis_swap_conventions: BmaBasisSwapConvention[];
}

export interface DeleteBmaBasisSwapConventionRequest {
    removal: BmaBasisSwapConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteBmaBasisSwapConventionResponse {
    result: Result;
}

export interface DeleteManyBmaBasisSwapConventionsRequest {
    removals: BmaBasisSwapConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBmaBasisSwapConventionsResponse {
    result: Result;
}

export interface ListBmaBasisSwapConventionVersionsRequest {
    key: BmaBasisSwapConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BmaBasisSwapConventionVersionsFilter | null;
}

export interface ListBmaBasisSwapConventionVersionsResponse {
    result: Result;
    versions: BmaBasisSwapConvention[];
    total: number;
}

export interface GetBmaBasisSwapConventionVersionRequest {
    key: BmaBasisSwapConventionVersionKey;
}

export interface GetBmaBasisSwapConventionVersionResponse {
    result: Result;
    version: BmaBasisSwapConvention | null;
}

export const subjects = {
    list_bma_basis_swap_conventions_request: 'refdata.v1.bma_basis_swap_conventions.list',
    get_bma_basis_swap_convention_request: 'refdata.v1.bma_basis_swap_conventions.get',
    get_many_bma_basis_swap_conventions_request: 'refdata.v1.bma_basis_swap_conventions.get_many',
    put_bma_basis_swap_convention_request: 'refdata.v1.bma_basis_swap_conventions.put',
    put_many_bma_basis_swap_conventions_request: 'refdata.v1.bma_basis_swap_conventions.put_many',
    delete_bma_basis_swap_convention_request: 'refdata.v1.bma_basis_swap_conventions.delete',
    delete_many_bma_basis_swap_conventions_request:
        'refdata.v1.bma_basis_swap_conventions.delete_many',
    list_bma_basis_swap_convention_versions_request:
        'refdata.v1.bma_basis_swap_conventions_versions.list',
    get_bma_basis_swap_convention_version_request:
        'refdata.v1.bma_basis_swap_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bma_basis_swap_conventions_request: true,
    get_bma_basis_swap_convention_request: true,
    get_many_bma_basis_swap_conventions_request: true,
    put_bma_basis_swap_convention_request: true,
    put_many_bma_basis_swap_conventions_request: true,
    delete_bma_basis_swap_convention_request: true,
    delete_many_bma_basis_swap_conventions_request: true,
    list_bma_basis_swap_convention_versions_request: true,
    get_bma_basis_swap_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.bma_basis_swap_conventions_events.created',
    updated: 'refdata.v1.bma_basis_swap_conventions_events.updated',
    deleted: 'refdata.v1.bma_basis_swap_conventions_events.deleted',
} as const;
