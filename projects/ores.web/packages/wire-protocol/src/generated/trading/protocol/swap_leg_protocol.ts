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
import type { SwapLeg } from '../domain/swap_leg.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface SwapLegKey {
    id: string;
}

export interface SwapLegWrite {
    id: string;
    trade_id: string;
    leg_number: number;
    leg_type_code: string;
    day_count_fraction_code: string;
    business_day_convention_code: string;
    payment_frequency_code: string;
    floating_index_code: string;
    fixed_rate: number;
    spread: number;
    notional: string;
    currency: string;
}

export interface SwapLegChange {
    write: SwapLegWrite;
    precondition: Precondition;
}

export interface SwapLegRemoval {
    key: SwapLegKey;
    precondition: Precondition;
}

export interface SwapLegLookup {
    key: SwapLegKey;
    swap_leg: SwapLeg | null;
}

export interface SwapLegsFilter {
    trade_id: string | null;
    id_one_of: string[] | null;
    trade_id_one_of: string[] | null;
}

export interface SwapLegEvent {
    event_id: string;
    key: SwapLegKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SwapLegVersionKey {
    swap_leg: SwapLegKey;
    version: number;
}

export interface SwapLegVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSwapLegsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SwapLegsFilter | null;
    as_of: string | null;
}

export interface ListSwapLegsResponse {
    result: Result;
    swap_legs: SwapLeg[];
    total: number;
}

export interface GetSwapLegRequest {
    key: SwapLegKey;
}

export interface GetSwapLegResponse {
    result: Result;
    swap_leg: SwapLeg | null;
}

export interface GetManySwapLegsRequest {
    keys: SwapLegKey[];
}

export interface GetManySwapLegsResponse {
    result: Result;
    entries: SwapLegLookup[];
}

export interface PutSwapLegRequest {
    change: SwapLegChange;
    intent: ChangeIntent;
}

export interface PutSwapLegResponse {
    result: Result;
    swap_leg: SwapLeg | null;
}

export interface PutManySwapLegsRequest {
    changes: SwapLegChange[];
    intent: ChangeIntent;
}

export interface PutManySwapLegsResponse {
    result: Result;
    swap_legs: SwapLeg[];
}

export interface DeleteSwapLegRequest {
    removal: SwapLegRemoval;
    intent: ChangeIntent;
}

export interface DeleteSwapLegResponse {
    result: Result;
}

export interface DeleteManySwapLegsRequest {
    removals: SwapLegRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySwapLegsResponse {
    result: Result;
}

export interface ListByTradeIdSwapLegsRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: SwapLegsFilter | null;
}

export interface ListByTradeIdSwapLegsResponse {
    result: Result;
    swap_legs: SwapLeg[];
    total: number;
}

export interface ListSwapLegVersionsRequest {
    key: SwapLegKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SwapLegVersionsFilter | null;
}

export interface ListSwapLegVersionsResponse {
    result: Result;
    versions: SwapLeg[];
    total: number;
}

export interface GetSwapLegVersionRequest {
    key: SwapLegVersionKey;
}

export interface GetSwapLegVersionResponse {
    result: Result;
    version: SwapLeg | null;
}

export const subjects = {
    list_swap_legs_request: 'trading.v1.swap_legs.list',
    get_swap_leg_request: 'trading.v1.swap_legs.get',
    get_many_swap_legs_request: 'trading.v1.swap_legs.get_many',
    put_swap_leg_request: 'trading.v1.swap_legs.put',
    put_many_swap_legs_request: 'trading.v1.swap_legs.put_many',
    delete_swap_leg_request: 'trading.v1.swap_legs.delete',
    delete_many_swap_legs_request: 'trading.v1.swap_legs.delete_many',
    list_by_trade_id_swap_legs_request: 'trading.v1.swap_legs.list_by_trade_id',
    list_swap_leg_versions_request: 'trading.v1.swap_legs_versions.list',
    get_swap_leg_version_request: 'trading.v1.swap_legs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_swap_legs_request: true,
    get_swap_leg_request: true,
    get_many_swap_legs_request: true,
    put_swap_leg_request: true,
    put_many_swap_legs_request: true,
    delete_swap_leg_request: true,
    delete_many_swap_legs_request: true,
    list_by_trade_id_swap_legs_request: true,
    list_swap_leg_versions_request: true,
    get_swap_leg_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.swap_legs_events.created',
    updated: 'trading.v1.swap_legs_events.updated',
    deleted: 'trading.v1.swap_legs_events.deleted',
} as const;
