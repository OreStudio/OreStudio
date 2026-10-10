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
import type { BalanceGuaranteedSwapTrancheNotional } from '../domain/balance_guaranteed_swap_tranche_notional.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BalanceGuaranteedSwapTrancheNotionalKey {
    trade_id: string;
    tranche_number: number;
    sequence_number: number;
}

export interface BalanceGuaranteedSwapTrancheNotionalWrite {
    trade_id: string;
    tranche_number: number;
    sequence_number: number;
    trade_activity_id: string;
    start_date: string | null;
    notional: string;
}

export interface BalanceGuaranteedSwapTrancheNotionalChange {
    write: BalanceGuaranteedSwapTrancheNotionalWrite;
    precondition: Precondition;
}

export interface BalanceGuaranteedSwapTrancheNotionalRemoval {
    key: BalanceGuaranteedSwapTrancheNotionalKey;
    precondition: Precondition;
}

export interface BalanceGuaranteedSwapTrancheNotionalLookup {
    key: BalanceGuaranteedSwapTrancheNotionalKey;
    balance_guaranteed_swap_tranche_notional: BalanceGuaranteedSwapTrancheNotional | null;
}

export interface BalanceGuaranteedSwapTrancheNotionalEvent {
    event_id: string;
    key: BalanceGuaranteedSwapTrancheNotionalKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BalanceGuaranteedSwapTrancheNotionalVersionKey {
    balance_guaranteed_swap_tranche_notional: BalanceGuaranteedSwapTrancheNotionalKey;
    version: number;
}

export interface BalanceGuaranteedSwapTrancheNotionalVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBalanceGuaranteedSwapTrancheNotionalsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListBalanceGuaranteedSwapTrancheNotionalsResponse {
    result: Result;
    balance_guaranteed_swap_tranche_notionals: BalanceGuaranteedSwapTrancheNotional[];
    total: number;
}

export interface GetBalanceGuaranteedSwapTrancheNotionalRequest {
    key: BalanceGuaranteedSwapTrancheNotionalKey;
}

export interface GetBalanceGuaranteedSwapTrancheNotionalResponse {
    result: Result;
    balance_guaranteed_swap_tranche_notional: BalanceGuaranteedSwapTrancheNotional | null;
}

export interface GetManyBalanceGuaranteedSwapTrancheNotionalsRequest {
    keys: BalanceGuaranteedSwapTrancheNotionalKey[];
}

export interface GetManyBalanceGuaranteedSwapTrancheNotionalsResponse {
    result: Result;
    entries: BalanceGuaranteedSwapTrancheNotionalLookup[];
}

export interface PutBalanceGuaranteedSwapTrancheNotionalRequest {
    change: BalanceGuaranteedSwapTrancheNotionalChange;
    intent: ChangeIntent;
}

export interface PutBalanceGuaranteedSwapTrancheNotionalResponse {
    result: Result;
    balance_guaranteed_swap_tranche_notional: BalanceGuaranteedSwapTrancheNotional | null;
}

export interface PutManyBalanceGuaranteedSwapTrancheNotionalsRequest {
    changes: BalanceGuaranteedSwapTrancheNotionalChange[];
    intent: ChangeIntent;
}

export interface PutManyBalanceGuaranteedSwapTrancheNotionalsResponse {
    result: Result;
    balance_guaranteed_swap_tranche_notionals: BalanceGuaranteedSwapTrancheNotional[];
}

export interface DeleteBalanceGuaranteedSwapTrancheNotionalRequest {
    removal: BalanceGuaranteedSwapTrancheNotionalRemoval;
    intent: ChangeIntent;
}

export interface DeleteBalanceGuaranteedSwapTrancheNotionalResponse {
    result: Result;
}

export interface DeleteManyBalanceGuaranteedSwapTrancheNotionalsRequest {
    removals: BalanceGuaranteedSwapTrancheNotionalRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBalanceGuaranteedSwapTrancheNotionalsResponse {
    result: Result;
}

export interface ListBalanceGuaranteedSwapTrancheNotionalVersionsRequest {
    key: BalanceGuaranteedSwapTrancheNotionalKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BalanceGuaranteedSwapTrancheNotionalVersionsFilter | null;
}

export interface ListBalanceGuaranteedSwapTrancheNotionalVersionsResponse {
    result: Result;
    versions: BalanceGuaranteedSwapTrancheNotional[];
    total: number;
}

export interface GetBalanceGuaranteedSwapTrancheNotionalVersionRequest {
    key: BalanceGuaranteedSwapTrancheNotionalVersionKey;
}

export interface GetBalanceGuaranteedSwapTrancheNotionalVersionResponse {
    result: Result;
    version: BalanceGuaranteedSwapTrancheNotional | null;
}

export const subjects = {
    list_balance_guaranteed_swap_tranche_notionals_request:
        'trading.v1.balance_guaranteed_swap_tranche_notionals.list',
    get_balance_guaranteed_swap_tranche_notional_request:
        'trading.v1.balance_guaranteed_swap_tranche_notionals.get',
    get_many_balance_guaranteed_swap_tranche_notionals_request:
        'trading.v1.balance_guaranteed_swap_tranche_notionals.get_many',
    put_balance_guaranteed_swap_tranche_notional_request:
        'trading.v1.balance_guaranteed_swap_tranche_notionals.put',
    put_many_balance_guaranteed_swap_tranche_notionals_request:
        'trading.v1.balance_guaranteed_swap_tranche_notionals.put_many',
    delete_balance_guaranteed_swap_tranche_notional_request:
        'trading.v1.balance_guaranteed_swap_tranche_notionals.delete',
    delete_many_balance_guaranteed_swap_tranche_notionals_request:
        'trading.v1.balance_guaranteed_swap_tranche_notionals.delete_many',
    list_balance_guaranteed_swap_tranche_notional_versions_request:
        'trading.v1.balance_guaranteed_swap_tranche_notionals_versions.list',
    get_balance_guaranteed_swap_tranche_notional_version_request:
        'trading.v1.balance_guaranteed_swap_tranche_notionals_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_balance_guaranteed_swap_tranche_notionals_request: true,
    get_balance_guaranteed_swap_tranche_notional_request: true,
    get_many_balance_guaranteed_swap_tranche_notionals_request: true,
    put_balance_guaranteed_swap_tranche_notional_request: true,
    put_many_balance_guaranteed_swap_tranche_notionals_request: true,
    delete_balance_guaranteed_swap_tranche_notional_request: true,
    delete_many_balance_guaranteed_swap_tranche_notionals_request: true,
    list_balance_guaranteed_swap_tranche_notional_versions_request: true,
    get_balance_guaranteed_swap_tranche_notional_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.balance_guaranteed_swap_tranche_notionals_events.created',
    updated: 'trading.v1.balance_guaranteed_swap_tranche_notionals_events.updated',
    deleted: 'trading.v1.balance_guaranteed_swap_tranche_notionals_events.deleted',
} as const;
