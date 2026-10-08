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
import type { SwapLegAmount } from '../domain/swap_leg_amount.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SwapLegAmountKey {
    trade_id: string;
    leg_number: number;
    sequence_number: number;
}

export interface SwapLegAmountWrite {
    trade_id: string;
    leg_number: number;
    sequence_number: number;
    trade_activity_id: string;
    start_date: string | null;
    amount: string;
}

export interface SwapLegAmountChange {
    write: SwapLegAmountWrite;
    precondition: Precondition;
}

export interface SwapLegAmountRemoval {
    key: SwapLegAmountKey;
    precondition: Precondition;
}

export interface SwapLegAmountLookup {
    key: SwapLegAmountKey;
    swap_leg_amount: SwapLegAmount | null;
}

export interface SwapLegAmountEvent {
    event_id: string;
    key: SwapLegAmountKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SwapLegAmountVersionKey {
    swap_leg_amount: SwapLegAmountKey;
    version: number;
}

export interface SwapLegAmountVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSwapLegAmountsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListSwapLegAmountsResponse {
    result: Result;
    swap_leg_amounts: SwapLegAmount[];
    total: number;
}

export interface GetSwapLegAmountRequest {
    key: SwapLegAmountKey;
}

export interface GetSwapLegAmountResponse {
    result: Result;
    swap_leg_amount: SwapLegAmount | null;
}

export interface GetManySwapLegAmountsRequest {
    keys: SwapLegAmountKey[];
}

export interface GetManySwapLegAmountsResponse {
    result: Result;
    entries: SwapLegAmountLookup[];
}

export interface PutSwapLegAmountRequest {
    change: SwapLegAmountChange;
    intent: ChangeIntent;
}

export interface PutSwapLegAmountResponse {
    result: Result;
    swap_leg_amount: SwapLegAmount | null;
}

export interface PutManySwapLegAmountsRequest {
    changes: SwapLegAmountChange[];
    intent: ChangeIntent;
}

export interface PutManySwapLegAmountsResponse {
    result: Result;
    swap_leg_amounts: SwapLegAmount[];
}

export interface DeleteSwapLegAmountRequest {
    removal: SwapLegAmountRemoval;
    intent: ChangeIntent;
}

export interface DeleteSwapLegAmountResponse {
    result: Result;
}

export interface DeleteManySwapLegAmountsRequest {
    removals: SwapLegAmountRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySwapLegAmountsResponse {
    result: Result;
}

export interface ListSwapLegAmountVersionsRequest {
    key: SwapLegAmountKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SwapLegAmountVersionsFilter | null;
}

export interface ListSwapLegAmountVersionsResponse {
    result: Result;
    versions: SwapLegAmount[];
    total: number;
}

export interface GetSwapLegAmountVersionRequest {
    key: SwapLegAmountVersionKey;
}

export interface GetSwapLegAmountVersionResponse {
    result: Result;
    version: SwapLegAmount | null;
}

export const subjects = {
    list_swap_leg_amounts_request: 'trading.v1.swap_leg_amounts.list',
    get_swap_leg_amount_request: 'trading.v1.swap_leg_amounts.get',
    get_many_swap_leg_amounts_request: 'trading.v1.swap_leg_amounts.get_many',
    put_swap_leg_amount_request: 'trading.v1.swap_leg_amounts.put',
    put_many_swap_leg_amounts_request: 'trading.v1.swap_leg_amounts.put_many',
    delete_swap_leg_amount_request: 'trading.v1.swap_leg_amounts.delete',
    delete_many_swap_leg_amounts_request: 'trading.v1.swap_leg_amounts.delete_many',
    list_swap_leg_amount_versions_request: 'trading.v1.swap_leg_amounts_versions.list',
    get_swap_leg_amount_version_request: 'trading.v1.swap_leg_amounts_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_swap_leg_amounts_request: true,
    get_swap_leg_amount_request: true,
    get_many_swap_leg_amounts_request: true,
    put_swap_leg_amount_request: true,
    put_many_swap_leg_amounts_request: true,
    delete_swap_leg_amount_request: true,
    delete_many_swap_leg_amounts_request: true,
    list_swap_leg_amount_versions_request: true,
    get_swap_leg_amount_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.swap_leg_amounts_events.created',
    updated: 'trading.v1.swap_leg_amounts_events.updated',
    deleted: 'trading.v1.swap_leg_amounts_events.deleted',
} as const;
