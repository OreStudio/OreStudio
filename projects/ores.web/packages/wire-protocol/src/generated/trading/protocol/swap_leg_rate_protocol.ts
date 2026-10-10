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
import type { SwapLegRate } from '../domain/swap_leg_rate.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SwapLegRateKey {
    trade_id: string;
    leg_number: number;
    rate_role: string;
    sequence_number: number;
}

export interface SwapLegRateWrite {
    trade_id: string;
    leg_number: number;
    rate_role: string;
    sequence_number: number;
    trade_activity_id: string;
    start_date: string | null;
    value: string;
}

export interface SwapLegRateChange {
    write: SwapLegRateWrite;
    precondition: Precondition;
}

export interface SwapLegRateRemoval {
    key: SwapLegRateKey;
    precondition: Precondition;
}

export interface SwapLegRateLookup {
    key: SwapLegRateKey;
    swap_leg_rate: SwapLegRate | null;
}

export interface SwapLegRateEvent {
    event_id: string;
    key: SwapLegRateKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SwapLegRateVersionKey {
    swap_leg_rate: SwapLegRateKey;
    version: number;
}

export interface SwapLegRateVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSwapLegRatesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListSwapLegRatesResponse {
    result: Result;
    swap_leg_rates: SwapLegRate[];
    total: number;
}

export interface GetSwapLegRateRequest {
    key: SwapLegRateKey;
}

export interface GetSwapLegRateResponse {
    result: Result;
    swap_leg_rate: SwapLegRate | null;
}

export interface GetManySwapLegRatesRequest {
    keys: SwapLegRateKey[];
}

export interface GetManySwapLegRatesResponse {
    result: Result;
    entries: SwapLegRateLookup[];
}

export interface PutSwapLegRateRequest {
    change: SwapLegRateChange;
    intent: ChangeIntent;
}

export interface PutSwapLegRateResponse {
    result: Result;
    swap_leg_rate: SwapLegRate | null;
}

export interface PutManySwapLegRatesRequest {
    changes: SwapLegRateChange[];
    intent: ChangeIntent;
}

export interface PutManySwapLegRatesResponse {
    result: Result;
    swap_leg_rates: SwapLegRate[];
}

export interface DeleteSwapLegRateRequest {
    removal: SwapLegRateRemoval;
    intent: ChangeIntent;
}

export interface DeleteSwapLegRateResponse {
    result: Result;
}

export interface DeleteManySwapLegRatesRequest {
    removals: SwapLegRateRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySwapLegRatesResponse {
    result: Result;
}

export interface ListSwapLegRateVersionsRequest {
    key: SwapLegRateKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SwapLegRateVersionsFilter | null;
}

export interface ListSwapLegRateVersionsResponse {
    result: Result;
    versions: SwapLegRate[];
    total: number;
}

export interface GetSwapLegRateVersionRequest {
    key: SwapLegRateVersionKey;
}

export interface GetSwapLegRateVersionResponse {
    result: Result;
    version: SwapLegRate | null;
}

export const subjects = {
    list_swap_leg_rates_request: 'trading.v1.swap_leg_rates.list',
    get_swap_leg_rate_request: 'trading.v1.swap_leg_rates.get',
    get_many_swap_leg_rates_request: 'trading.v1.swap_leg_rates.get_many',
    put_swap_leg_rate_request: 'trading.v1.swap_leg_rates.put',
    put_many_swap_leg_rates_request: 'trading.v1.swap_leg_rates.put_many',
    delete_swap_leg_rate_request: 'trading.v1.swap_leg_rates.delete',
    delete_many_swap_leg_rates_request: 'trading.v1.swap_leg_rates.delete_many',
    list_swap_leg_rate_versions_request: 'trading.v1.swap_leg_rates_versions.list',
    get_swap_leg_rate_version_request: 'trading.v1.swap_leg_rates_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_swap_leg_rates_request: true,
    get_swap_leg_rate_request: true,
    get_many_swap_leg_rates_request: true,
    put_swap_leg_rate_request: true,
    put_many_swap_leg_rates_request: true,
    delete_swap_leg_rate_request: true,
    delete_many_swap_leg_rates_request: true,
    list_swap_leg_rate_versions_request: true,
    get_swap_leg_rate_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.swap_leg_rates_events.created',
    updated: 'trading.v1.swap_leg_rates_events.updated',
    deleted: 'trading.v1.swap_leg_rates_events.deleted',
} as const;
