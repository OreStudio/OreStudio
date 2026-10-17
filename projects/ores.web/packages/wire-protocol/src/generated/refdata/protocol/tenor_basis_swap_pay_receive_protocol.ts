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
import type { TenorBasisSwapPayReceive } from '../domain/tenor_basis_swap_pay_receive.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface TenorBasisSwapPayReceiveKey {
    id: string;
}

export interface TenorBasisSwapPayReceiveWrite {
    id: string;
    convention_id: string;
    pay_index: string | null;
    pay_frequency: string | null;
    receive_index: string | null;
    receive_frequency: string | null;
    spread_on_rec: boolean | null;
    include_spread: boolean | null;
    sub_periods_coupon_type: string | null;
    pay_is_averaged: boolean | null;
    rec_is_averaged: boolean | null;
}

export interface TenorBasisSwapPayReceiveChange {
    write: TenorBasisSwapPayReceiveWrite;
    precondition: Precondition;
}

export interface TenorBasisSwapPayReceiveRemoval {
    key: TenorBasisSwapPayReceiveKey;
    precondition: Precondition;
}

export interface TenorBasisSwapPayReceiveLookup {
    key: TenorBasisSwapPayReceiveKey;
    tenor_basis_swap_pay_receive: TenorBasisSwapPayReceive | null;
}

export interface TenorBasisSwapPayReceivesFilter {
    convention_id: string | null;
    id_one_of: string[] | null;
    convention_id_one_of: string[] | null;
}

export interface TenorBasisSwapPayReceiveEvent {
    event_id: string;
    key: TenorBasisSwapPayReceiveKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TenorBasisSwapPayReceiveVersionKey {
    tenor_basis_swap_pay_receive: TenorBasisSwapPayReceiveKey;
    version: number;
}

export interface TenorBasisSwapPayReceiveVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTenorBasisSwapPayReceivesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TenorBasisSwapPayReceivesFilter | null;
    as_of: string | null;
}

export interface ListTenorBasisSwapPayReceivesResponse {
    result: Result;
    tenor_basis_swap_pay_receives: TenorBasisSwapPayReceive[];
    total: number;
}

export interface GetTenorBasisSwapPayReceiveRequest {
    key: TenorBasisSwapPayReceiveKey;
}

export interface GetTenorBasisSwapPayReceiveResponse {
    result: Result;
    tenor_basis_swap_pay_receive: TenorBasisSwapPayReceive | null;
}

export interface GetManyTenorBasisSwapPayReceivesRequest {
    keys: TenorBasisSwapPayReceiveKey[];
}

export interface GetManyTenorBasisSwapPayReceivesResponse {
    result: Result;
    entries: TenorBasisSwapPayReceiveLookup[];
}

export interface PutTenorBasisSwapPayReceiveRequest {
    change: TenorBasisSwapPayReceiveChange;
    intent: ChangeIntent;
}

export interface PutTenorBasisSwapPayReceiveResponse {
    result: Result;
    tenor_basis_swap_pay_receive: TenorBasisSwapPayReceive | null;
}

export interface PutManyTenorBasisSwapPayReceivesRequest {
    changes: TenorBasisSwapPayReceiveChange[];
    intent: ChangeIntent;
}

export interface PutManyTenorBasisSwapPayReceivesResponse {
    result: Result;
    tenor_basis_swap_pay_receives: TenorBasisSwapPayReceive[];
}

export interface DeleteTenorBasisSwapPayReceiveRequest {
    removal: TenorBasisSwapPayReceiveRemoval;
    intent: ChangeIntent;
}

export interface DeleteTenorBasisSwapPayReceiveResponse {
    result: Result;
}

export interface DeleteManyTenorBasisSwapPayReceivesRequest {
    removals: TenorBasisSwapPayReceiveRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTenorBasisSwapPayReceivesResponse {
    result: Result;
}

export interface ListByConventionIdTenorBasisSwapPayReceivesRequest {
    convention_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorBasisSwapPayReceivesFilter | null;
}

export interface ListByConventionIdTenorBasisSwapPayReceivesResponse {
    result: Result;
    tenor_basis_swap_pay_receives: TenorBasisSwapPayReceive[];
    total: number;
}

export interface ListTenorBasisSwapPayReceiveVersionsRequest {
    key: TenorBasisSwapPayReceiveKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorBasisSwapPayReceiveVersionsFilter | null;
}

export interface ListTenorBasisSwapPayReceiveVersionsResponse {
    result: Result;
    versions: TenorBasisSwapPayReceive[];
    total: number;
}

export interface GetTenorBasisSwapPayReceiveVersionRequest {
    key: TenorBasisSwapPayReceiveVersionKey;
}

export interface GetTenorBasisSwapPayReceiveVersionResponse {
    result: Result;
    version: TenorBasisSwapPayReceive | null;
}

export const subjects = {
    list_tenor_basis_swap_pay_receives_request: 'refdata.v1.tenor_basis_swap_pay_receives.list',
    get_tenor_basis_swap_pay_receive_request: 'refdata.v1.tenor_basis_swap_pay_receives.get',
    get_many_tenor_basis_swap_pay_receives_request:
        'refdata.v1.tenor_basis_swap_pay_receives.get_many',
    put_tenor_basis_swap_pay_receive_request: 'refdata.v1.tenor_basis_swap_pay_receives.put',
    put_many_tenor_basis_swap_pay_receives_request:
        'refdata.v1.tenor_basis_swap_pay_receives.put_many',
    delete_tenor_basis_swap_pay_receive_request: 'refdata.v1.tenor_basis_swap_pay_receives.delete',
    delete_many_tenor_basis_swap_pay_receives_request:
        'refdata.v1.tenor_basis_swap_pay_receives.delete_many',
    list_by_convention_id_tenor_basis_swap_pay_receives_request:
        'refdata.v1.tenor_basis_swap_pay_receives.list_by_convention_id',
    list_tenor_basis_swap_pay_receive_versions_request:
        'refdata.v1.tenor_basis_swap_pay_receives_versions.list',
    get_tenor_basis_swap_pay_receive_version_request:
        'refdata.v1.tenor_basis_swap_pay_receives_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_tenor_basis_swap_pay_receives_request: true,
    get_tenor_basis_swap_pay_receive_request: true,
    get_many_tenor_basis_swap_pay_receives_request: true,
    put_tenor_basis_swap_pay_receive_request: true,
    put_many_tenor_basis_swap_pay_receives_request: true,
    delete_tenor_basis_swap_pay_receive_request: true,
    delete_many_tenor_basis_swap_pay_receives_request: true,
    list_by_convention_id_tenor_basis_swap_pay_receives_request: true,
    list_tenor_basis_swap_pay_receive_versions_request: true,
    get_tenor_basis_swap_pay_receive_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.tenor_basis_swap_pay_receives_events.created',
    updated: 'refdata.v1.tenor_basis_swap_pay_receives_events.updated',
    deleted: 'refdata.v1.tenor_basis_swap_pay_receives_events.deleted',
} as const;
