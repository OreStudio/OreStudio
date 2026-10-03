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
import type { YieldVolatility } from '../domain/yield_volatility.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface YieldVolatilityKey {
    id: string;
}

export interface YieldVolatilityWrite {
    id: string;
    curve_definition_id: string;
    qualifier: string;
    dimension: string | null;
    volatility_type: string;
    extrapolation: string;
    day_counter: string;
    calendar: string;
    business_day_convention: string;
    option_tenors: string;
    bond_tenors: string;
}

export interface YieldVolatilityChange {
    write: YieldVolatilityWrite;
    precondition: Precondition;
}

export interface YieldVolatilityRemoval {
    key: YieldVolatilityKey;
    precondition: Precondition;
}

export interface YieldVolatilityLookup {
    key: YieldVolatilityKey;
    yield_volatility: YieldVolatility | null;
}

export interface YieldVolatilityEvent {
    event_id: string;
    key: YieldVolatilityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface YieldVolatilityVersionKey {
    yield_volatility: YieldVolatilityKey;
    version: number;
}

export interface YieldVolatilityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListYieldVolatilitiesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListYieldVolatilitiesResponse {
    result: Result;
    yield_volatilities: YieldVolatility[];
    total: number;
}

export interface GetYieldVolatilityRequest {
    key: YieldVolatilityKey;
}

export interface GetYieldVolatilityResponse {
    result: Result;
    yield_volatility: YieldVolatility | null;
}

export interface GetManyYieldVolatilitiesRequest {
    keys: YieldVolatilityKey[];
}

export interface GetManyYieldVolatilitiesResponse {
    result: Result;
    entries: YieldVolatilityLookup[];
}

export interface PutYieldVolatilityRequest {
    change: YieldVolatilityChange;
    intent: ChangeIntent;
}

export interface PutYieldVolatilityResponse {
    result: Result;
    yield_volatility: YieldVolatility | null;
}

export interface PutManyYieldVolatilitiesRequest {
    changes: YieldVolatilityChange[];
    intent: ChangeIntent;
}

export interface PutManyYieldVolatilitiesResponse {
    result: Result;
    yield_volatilities: YieldVolatility[];
}

export interface DeleteYieldVolatilityRequest {
    removal: YieldVolatilityRemoval;
    intent: ChangeIntent;
}

export interface DeleteYieldVolatilityResponse {
    result: Result;
}

export interface DeleteManyYieldVolatilitiesRequest {
    removals: YieldVolatilityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyYieldVolatilitiesResponse {
    result: Result;
}

export interface ListYieldVolatilityVersionsRequest {
    key: YieldVolatilityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: YieldVolatilityVersionsFilter | null;
}

export interface ListYieldVolatilityVersionsResponse {
    result: Result;
    versions: YieldVolatility[];
    total: number;
}

export interface GetYieldVolatilityVersionRequest {
    key: YieldVolatilityVersionKey;
}

export interface GetYieldVolatilityVersionResponse {
    result: Result;
    version: YieldVolatility | null;
}

export const subjects = {
    list_yield_volatilities_request: 'refdata.v1.yield_volatilities.list',
    get_yield_volatility_request: 'refdata.v1.yield_volatilities.get',
    get_many_yield_volatilities_request: 'refdata.v1.yield_volatilities.get_many',
    put_yield_volatility_request: 'refdata.v1.yield_volatilities.put',
    put_many_yield_volatilities_request: 'refdata.v1.yield_volatilities.put_many',
    delete_yield_volatility_request: 'refdata.v1.yield_volatilities.delete',
    delete_many_yield_volatilities_request: 'refdata.v1.yield_volatilities.delete_many',
    list_yield_volatility_versions_request: 'refdata.v1.yield_volatilities_versions.list',
    get_yield_volatility_version_request: 'refdata.v1.yield_volatilities_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_yield_volatilities_request: true,
    get_yield_volatility_request: true,
    get_many_yield_volatilities_request: true,
    put_yield_volatility_request: true,
    put_many_yield_volatilities_request: true,
    delete_yield_volatility_request: true,
    delete_many_yield_volatilities_request: true,
    list_yield_volatility_versions_request: true,
    get_yield_volatility_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.yield_volatilities_events.created',
    updated: 'refdata.v1.yield_volatilities_events.updated',
    deleted: 'refdata.v1.yield_volatilities_events.deleted',
} as const;
