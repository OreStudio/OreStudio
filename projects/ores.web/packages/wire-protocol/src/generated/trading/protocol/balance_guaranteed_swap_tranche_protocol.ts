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
import type { BalanceGuaranteedSwapTranche } from '../domain/balance_guaranteed_swap_tranche.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BalanceGuaranteedSwapTrancheKey {
    trade_id: string;
    sequence_number: number;
}

export interface BalanceGuaranteedSwapTrancheWrite {
    trade_id: string;
    sequence_number: number;
    trade_activity_id: string;
    description: string | null;
    security_id: string;
    seniority: number;
}

export interface BalanceGuaranteedSwapTrancheChange {
    write: BalanceGuaranteedSwapTrancheWrite;
    precondition: Precondition;
}

export interface BalanceGuaranteedSwapTrancheRemoval {
    key: BalanceGuaranteedSwapTrancheKey;
    precondition: Precondition;
}

export interface BalanceGuaranteedSwapTrancheLookup {
    key: BalanceGuaranteedSwapTrancheKey;
    balance_guaranteed_swap_tranche: BalanceGuaranteedSwapTranche | null;
}

export interface BalanceGuaranteedSwapTrancheEvent {
    event_id: string;
    key: BalanceGuaranteedSwapTrancheKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BalanceGuaranteedSwapTrancheVersionKey {
    balance_guaranteed_swap_tranche: BalanceGuaranteedSwapTrancheKey;
    version: number;
}

export interface BalanceGuaranteedSwapTrancheVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBalanceGuaranteedSwapTranchesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListBalanceGuaranteedSwapTranchesResponse {
    result: Result;
    balance_guaranteed_swap_tranches: BalanceGuaranteedSwapTranche[];
    total: number;
}

export interface GetBalanceGuaranteedSwapTrancheRequest {
    key: BalanceGuaranteedSwapTrancheKey;
}

export interface GetBalanceGuaranteedSwapTrancheResponse {
    result: Result;
    balance_guaranteed_swap_tranche: BalanceGuaranteedSwapTranche | null;
}

export interface GetManyBalanceGuaranteedSwapTranchesRequest {
    keys: BalanceGuaranteedSwapTrancheKey[];
}

export interface GetManyBalanceGuaranteedSwapTranchesResponse {
    result: Result;
    entries: BalanceGuaranteedSwapTrancheLookup[];
}

export interface PutBalanceGuaranteedSwapTrancheRequest {
    change: BalanceGuaranteedSwapTrancheChange;
    intent: ChangeIntent;
}

export interface PutBalanceGuaranteedSwapTrancheResponse {
    result: Result;
    balance_guaranteed_swap_tranche: BalanceGuaranteedSwapTranche | null;
}

export interface PutManyBalanceGuaranteedSwapTranchesRequest {
    changes: BalanceGuaranteedSwapTrancheChange[];
    intent: ChangeIntent;
}

export interface PutManyBalanceGuaranteedSwapTranchesResponse {
    result: Result;
    balance_guaranteed_swap_tranches: BalanceGuaranteedSwapTranche[];
}

export interface DeleteBalanceGuaranteedSwapTrancheRequest {
    removal: BalanceGuaranteedSwapTrancheRemoval;
    intent: ChangeIntent;
}

export interface DeleteBalanceGuaranteedSwapTrancheResponse {
    result: Result;
}

export interface DeleteManyBalanceGuaranteedSwapTranchesRequest {
    removals: BalanceGuaranteedSwapTrancheRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBalanceGuaranteedSwapTranchesResponse {
    result: Result;
}

export interface ListBalanceGuaranteedSwapTrancheVersionsRequest {
    key: BalanceGuaranteedSwapTrancheKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BalanceGuaranteedSwapTrancheVersionsFilter | null;
}

export interface ListBalanceGuaranteedSwapTrancheVersionsResponse {
    result: Result;
    versions: BalanceGuaranteedSwapTranche[];
    total: number;
}

export interface GetBalanceGuaranteedSwapTrancheVersionRequest {
    key: BalanceGuaranteedSwapTrancheVersionKey;
}

export interface GetBalanceGuaranteedSwapTrancheVersionResponse {
    result: Result;
    version: BalanceGuaranteedSwapTranche | null;
}

export const subjects = {
    list_balance_guaranteed_swap_tranches_request:
        'trading.v1.balance_guaranteed_swap_tranches.list',
    get_balance_guaranteed_swap_tranche_request: 'trading.v1.balance_guaranteed_swap_tranches.get',
    get_many_balance_guaranteed_swap_tranches_request:
        'trading.v1.balance_guaranteed_swap_tranches.get_many',
    put_balance_guaranteed_swap_tranche_request: 'trading.v1.balance_guaranteed_swap_tranches.put',
    put_many_balance_guaranteed_swap_tranches_request:
        'trading.v1.balance_guaranteed_swap_tranches.put_many',
    delete_balance_guaranteed_swap_tranche_request:
        'trading.v1.balance_guaranteed_swap_tranches.delete',
    delete_many_balance_guaranteed_swap_tranches_request:
        'trading.v1.balance_guaranteed_swap_tranches.delete_many',
    list_balance_guaranteed_swap_tranche_versions_request:
        'trading.v1.balance_guaranteed_swap_tranches_versions.list',
    get_balance_guaranteed_swap_tranche_version_request:
        'trading.v1.balance_guaranteed_swap_tranches_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_balance_guaranteed_swap_tranches_request: true,
    get_balance_guaranteed_swap_tranche_request: true,
    get_many_balance_guaranteed_swap_tranches_request: true,
    put_balance_guaranteed_swap_tranche_request: true,
    put_many_balance_guaranteed_swap_tranches_request: true,
    delete_balance_guaranteed_swap_tranche_request: true,
    delete_many_balance_guaranteed_swap_tranches_request: true,
    list_balance_guaranteed_swap_tranche_versions_request: true,
    get_balance_guaranteed_swap_tranche_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.balance_guaranteed_swap_tranches_events.created',
    updated: 'trading.v1.balance_guaranteed_swap_tranches_events.updated',
    deleted: 'trading.v1.balance_guaranteed_swap_tranches_events.deleted',
} as const;
