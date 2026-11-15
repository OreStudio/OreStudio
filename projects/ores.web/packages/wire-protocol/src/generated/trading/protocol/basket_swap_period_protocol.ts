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
import type { BasketSwapPeriod } from '../domain/basket_swap_period.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BasketSwapPeriodKey {
    trade_id: string;
    trade_activity_id: string;
    sequence_number: number;
}

export interface BasketSwapPeriodWrite {
    trade_id: string;
    trade_activity_id: string;
    sequence_number: number;
    period_date: string | null;
    knock_out_level: number | null;
    fixed_trigger_level: number | null;
}

export interface BasketSwapPeriodChange {
    write: BasketSwapPeriodWrite;
    precondition: Precondition;
}

export interface BasketSwapPeriodRemoval {
    key: BasketSwapPeriodKey;
    precondition: Precondition;
}

export interface BasketSwapPeriodLookup {
    key: BasketSwapPeriodKey;
    basket_swap_period: BasketSwapPeriod | null;
}

export interface BasketSwapPeriodEvent {
    event_id: string;
    key: BasketSwapPeriodKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BasketSwapPeriodVersionKey {
    basket_swap_period: BasketSwapPeriodKey;
    version: number;
}

export interface BasketSwapPeriodVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBasketSwapPeriodsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListBasketSwapPeriodsResponse {
    result: Result;
    basket_swap_periods: BasketSwapPeriod[];
    total: number;
}

export interface GetBasketSwapPeriodRequest {
    key: BasketSwapPeriodKey;
}

export interface GetBasketSwapPeriodResponse {
    result: Result;
    basket_swap_period: BasketSwapPeriod | null;
}

export interface GetManyBasketSwapPeriodsRequest {
    keys: BasketSwapPeriodKey[];
}

export interface GetManyBasketSwapPeriodsResponse {
    result: Result;
    entries: BasketSwapPeriodLookup[];
}

export interface PutBasketSwapPeriodRequest {
    change: BasketSwapPeriodChange;
    intent: ChangeIntent;
}

export interface PutBasketSwapPeriodResponse {
    result: Result;
    basket_swap_period: BasketSwapPeriod | null;
}

export interface PutManyBasketSwapPeriodsRequest {
    changes: BasketSwapPeriodChange[];
    intent: ChangeIntent;
}

export interface PutManyBasketSwapPeriodsResponse {
    result: Result;
    basket_swap_periods: BasketSwapPeriod[];
}

export interface DeleteBasketSwapPeriodRequest {
    removal: BasketSwapPeriodRemoval;
    intent: ChangeIntent;
}

export interface DeleteBasketSwapPeriodResponse {
    result: Result;
}

export interface DeleteManyBasketSwapPeriodsRequest {
    removals: BasketSwapPeriodRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBasketSwapPeriodsResponse {
    result: Result;
}

export interface ListBasketSwapPeriodVersionsRequest {
    key: BasketSwapPeriodKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BasketSwapPeriodVersionsFilter | null;
}

export interface ListBasketSwapPeriodVersionsResponse {
    result: Result;
    versions: BasketSwapPeriod[];
    total: number;
}

export interface GetBasketSwapPeriodVersionRequest {
    key: BasketSwapPeriodVersionKey;
}

export interface GetBasketSwapPeriodVersionResponse {
    result: Result;
    version: BasketSwapPeriod | null;
}

export const subjects = {
    list_basket_swap_periods_request: 'trading.v1.basket_swap_periods.list',
    get_basket_swap_period_request: 'trading.v1.basket_swap_periods.get',
    get_many_basket_swap_periods_request: 'trading.v1.basket_swap_periods.get_many',
    put_basket_swap_period_request: 'trading.v1.basket_swap_periods.put',
    put_many_basket_swap_periods_request: 'trading.v1.basket_swap_periods.put_many',
    delete_basket_swap_period_request: 'trading.v1.basket_swap_periods.delete',
    delete_many_basket_swap_periods_request: 'trading.v1.basket_swap_periods.delete_many',
    list_basket_swap_period_versions_request: 'trading.v1.basket_swap_periods_versions.list',
    get_basket_swap_period_version_request: 'trading.v1.basket_swap_periods_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_basket_swap_periods_request: true,
    get_basket_swap_period_request: true,
    get_many_basket_swap_periods_request: true,
    put_basket_swap_period_request: true,
    put_many_basket_swap_periods_request: true,
    delete_basket_swap_period_request: true,
    delete_many_basket_swap_periods_request: true,
    list_basket_swap_period_versions_request: true,
    get_basket_swap_period_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.basket_swap_periods.created',
    updated: 'trading.v1.events.basket_swap_periods.updated',
    deleted: 'trading.v1.events.basket_swap_periods.deleted',
} as const;
