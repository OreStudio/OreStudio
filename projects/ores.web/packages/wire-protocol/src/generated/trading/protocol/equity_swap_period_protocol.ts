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
import type { EquitySwapPeriod } from '../domain/equity_swap_period.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquitySwapPeriodKey {
    trade_id: string;
    trade_activity_id: string;
    sequence_number: number;
}

export interface EquitySwapPeriodWrite {
    trade_id: string;
    trade_activity_id: string;
    sequence_number: number;
    period_date: string | null;
    knock_out_level: number | null;
    fixed_trigger_level: number | null;
}

export interface EquitySwapPeriodChange {
    write: EquitySwapPeriodWrite;
    precondition: Precondition;
}

export interface EquitySwapPeriodRemoval {
    key: EquitySwapPeriodKey;
    precondition: Precondition;
}

export interface EquitySwapPeriodLookup {
    key: EquitySwapPeriodKey;
    equity_swap_period: EquitySwapPeriod | null;
}

export interface EquitySwapPeriodEvent {
    event_id: string;
    key: EquitySwapPeriodKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquitySwapPeriodVersionKey {
    equity_swap_period: EquitySwapPeriodKey;
    version: number;
}

export interface EquitySwapPeriodVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquitySwapPeriodsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListEquitySwapPeriodsResponse {
    result: Result;
    equity_swap_periods: EquitySwapPeriod[];
    total: number;
}

export interface GetEquitySwapPeriodRequest {
    key: EquitySwapPeriodKey;
}

export interface GetEquitySwapPeriodResponse {
    result: Result;
    equity_swap_period: EquitySwapPeriod | null;
}

export interface GetManyEquitySwapPeriodsRequest {
    keys: EquitySwapPeriodKey[];
}

export interface GetManyEquitySwapPeriodsResponse {
    result: Result;
    entries: EquitySwapPeriodLookup[];
}

export interface PutEquitySwapPeriodRequest {
    change: EquitySwapPeriodChange;
    intent: ChangeIntent;
}

export interface PutEquitySwapPeriodResponse {
    result: Result;
    equity_swap_period: EquitySwapPeriod | null;
}

export interface PutManyEquitySwapPeriodsRequest {
    changes: EquitySwapPeriodChange[];
    intent: ChangeIntent;
}

export interface PutManyEquitySwapPeriodsResponse {
    result: Result;
    equity_swap_periods: EquitySwapPeriod[];
}

export interface DeleteEquitySwapPeriodRequest {
    removal: EquitySwapPeriodRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquitySwapPeriodResponse {
    result: Result;
}

export interface DeleteManyEquitySwapPeriodsRequest {
    removals: EquitySwapPeriodRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquitySwapPeriodsResponse {
    result: Result;
}

export interface ListEquitySwapPeriodVersionsRequest {
    key: EquitySwapPeriodKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquitySwapPeriodVersionsFilter | null;
}

export interface ListEquitySwapPeriodVersionsResponse {
    result: Result;
    versions: EquitySwapPeriod[];
    total: number;
}

export interface GetEquitySwapPeriodVersionRequest {
    key: EquitySwapPeriodVersionKey;
}

export interface GetEquitySwapPeriodVersionResponse {
    result: Result;
    version: EquitySwapPeriod | null;
}

export const subjects = {
    list_equity_swap_periods_request: 'trading.v1.equity_swap_periods.list',
    get_equity_swap_period_request: 'trading.v1.equity_swap_periods.get',
    get_many_equity_swap_periods_request: 'trading.v1.equity_swap_periods.get_many',
    put_equity_swap_period_request: 'trading.v1.equity_swap_periods.put',
    put_many_equity_swap_periods_request: 'trading.v1.equity_swap_periods.put_many',
    delete_equity_swap_period_request: 'trading.v1.equity_swap_periods.delete',
    delete_many_equity_swap_periods_request: 'trading.v1.equity_swap_periods.delete_many',
    list_equity_swap_period_versions_request: 'trading.v1.equity_swap_periods_versions.list',
    get_equity_swap_period_version_request: 'trading.v1.equity_swap_periods_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_swap_periods_request: true,
    get_equity_swap_period_request: true,
    get_many_equity_swap_periods_request: true,
    put_equity_swap_period_request: true,
    put_many_equity_swap_periods_request: true,
    delete_equity_swap_period_request: true,
    delete_many_equity_swap_periods_request: true,
    list_equity_swap_period_versions_request: true,
    get_equity_swap_period_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_swap_periods_events.created',
    updated: 'trading.v1.equity_swap_periods_events.updated',
    deleted: 'trading.v1.equity_swap_periods_events.deleted',
} as const;
