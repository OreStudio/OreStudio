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
import type { SwapLegAmortization } from '../domain/swap_leg_amortization.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface SwapLegAmortizationKey {
    trade_id: string;
    leg_number: number;
    sequence_number: number;
}

export interface SwapLegAmortizationWrite {
    trade_id: string;
    leg_number: number;
    sequence_number: number;
    trade_activity_id: string;
    amortization_type: string;
    value: string | null;
    start_date: string | null;
    end_date: string | null;
    frequency: string | null;
    underflow: boolean | null;
}

export interface SwapLegAmortizationChange {
    write: SwapLegAmortizationWrite;
    precondition: Precondition;
}

export interface SwapLegAmortizationRemoval {
    key: SwapLegAmortizationKey;
    precondition: Precondition;
}

export interface SwapLegAmortizationLookup {
    key: SwapLegAmortizationKey;
    swap_leg_amortization: SwapLegAmortization | null;
}

export interface SwapLegAmortizationsFilter {
    trade_id: string | null;
    trade_id_one_of: string[] | null;
}

export interface SwapLegAmortizationEvent {
    event_id: string;
    key: SwapLegAmortizationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SwapLegAmortizationVersionKey {
    swap_leg_amortization: SwapLegAmortizationKey;
    version: number;
}

export interface SwapLegAmortizationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSwapLegAmortizationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SwapLegAmortizationsFilter | null;
    as_of: string | null;
}

export interface ListSwapLegAmortizationsResponse {
    result: Result;
    swap_leg_amortizations: SwapLegAmortization[];
    total: number;
}

export interface GetSwapLegAmortizationRequest {
    key: SwapLegAmortizationKey;
}

export interface GetSwapLegAmortizationResponse {
    result: Result;
    swap_leg_amortization: SwapLegAmortization | null;
}

export interface GetManySwapLegAmortizationsRequest {
    keys: SwapLegAmortizationKey[];
}

export interface GetManySwapLegAmortizationsResponse {
    result: Result;
    entries: SwapLegAmortizationLookup[];
}

export interface PutSwapLegAmortizationRequest {
    change: SwapLegAmortizationChange;
    intent: ChangeIntent;
}

export interface PutSwapLegAmortizationResponse {
    result: Result;
    swap_leg_amortization: SwapLegAmortization | null;
}

export interface PutManySwapLegAmortizationsRequest {
    changes: SwapLegAmortizationChange[];
    intent: ChangeIntent;
}

export interface PutManySwapLegAmortizationsResponse {
    result: Result;
    swap_leg_amortizations: SwapLegAmortization[];
}

export interface DeleteSwapLegAmortizationRequest {
    removal: SwapLegAmortizationRemoval;
    intent: ChangeIntent;
}

export interface DeleteSwapLegAmortizationResponse {
    result: Result;
}

export interface DeleteManySwapLegAmortizationsRequest {
    removals: SwapLegAmortizationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySwapLegAmortizationsResponse {
    result: Result;
}

export interface ListByTradeIdSwapLegAmortizationsRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: SwapLegAmortizationsFilter | null;
}

export interface ListByTradeIdSwapLegAmortizationsResponse {
    result: Result;
    swap_leg_amortizations: SwapLegAmortization[];
    total: number;
}

export interface ListSwapLegAmortizationVersionsRequest {
    key: SwapLegAmortizationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SwapLegAmortizationVersionsFilter | null;
}

export interface ListSwapLegAmortizationVersionsResponse {
    result: Result;
    versions: SwapLegAmortization[];
    total: number;
}

export interface GetSwapLegAmortizationVersionRequest {
    key: SwapLegAmortizationVersionKey;
}

export interface GetSwapLegAmortizationVersionResponse {
    result: Result;
    version: SwapLegAmortization | null;
}

export const subjects = {
    list_swap_leg_amortizations_request: 'trading.v1.swap_leg_amortizations.list',
    get_swap_leg_amortization_request: 'trading.v1.swap_leg_amortizations.get',
    get_many_swap_leg_amortizations_request: 'trading.v1.swap_leg_amortizations.get_many',
    put_swap_leg_amortization_request: 'trading.v1.swap_leg_amortizations.put',
    put_many_swap_leg_amortizations_request: 'trading.v1.swap_leg_amortizations.put_many',
    delete_swap_leg_amortization_request: 'trading.v1.swap_leg_amortizations.delete',
    delete_many_swap_leg_amortizations_request: 'trading.v1.swap_leg_amortizations.delete_many',
    list_by_trade_id_swap_leg_amortizations_request:
        'trading.v1.swap_leg_amortizations.list_by_trade_id',
    list_swap_leg_amortization_versions_request: 'trading.v1.swap_leg_amortizations_versions.list',
    get_swap_leg_amortization_version_request: 'trading.v1.swap_leg_amortizations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_swap_leg_amortizations_request: true,
    get_swap_leg_amortization_request: true,
    get_many_swap_leg_amortizations_request: true,
    put_swap_leg_amortization_request: true,
    put_many_swap_leg_amortizations_request: true,
    delete_swap_leg_amortization_request: true,
    delete_many_swap_leg_amortizations_request: true,
    list_by_trade_id_swap_leg_amortizations_request: true,
    list_swap_leg_amortization_versions_request: true,
    get_swap_leg_amortization_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.swap_leg_amortizations_events.created',
    updated: 'trading.v1.swap_leg_amortizations_events.updated',
    deleted: 'trading.v1.swap_leg_amortizations_events.deleted',
} as const;
