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
import type { InflationCapFloorVolatility } from '../domain/inflation_cap_floor_volatility.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InflationCapFloorVolatilityKey {
    id: string;
}

export interface InflationCapFloorVolatilityWrite {
    id: string;
    curve_definition_id: string;
    inflation_type: string;
    quote_type: string;
    volatility_type: string;
    extrapolation: string;
    tenors: string;
    settlement_days: number | null;
    cap_strikes: string | null;
    floor_strikes: string | null;
    strikes: string | null;
    calendar: string;
    day_counter: string;
    business_day_convention: string;
    index: string;
    index_curve: string;
    index_interpolated: string | null;
    observation_lag: string;
    yield_term_structure: string;
    quote_index: string | null;
    conventions: string | null;
}

export interface InflationCapFloorVolatilityChange {
    write: InflationCapFloorVolatilityWrite;
    precondition: Precondition;
}

export interface InflationCapFloorVolatilityRemoval {
    key: InflationCapFloorVolatilityKey;
    precondition: Precondition;
}

export interface InflationCapFloorVolatilityLookup {
    key: InflationCapFloorVolatilityKey;
    inflation_cap_floor_volatility: InflationCapFloorVolatility | null;
}

export interface InflationCapFloorVolatilityEvent {
    event_id: string;
    key: InflationCapFloorVolatilityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InflationCapFloorVolatilityVersionKey {
    inflation_cap_floor_volatility: InflationCapFloorVolatilityKey;
    version: number;
}

export interface InflationCapFloorVolatilityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInflationCapFloorVolatilitiesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListInflationCapFloorVolatilitiesResponse {
    result: Result;
    inflation_cap_floor_volatilities: InflationCapFloorVolatility[];
    total: number;
}

export interface GetInflationCapFloorVolatilityRequest {
    key: InflationCapFloorVolatilityKey;
}

export interface GetInflationCapFloorVolatilityResponse {
    result: Result;
    inflation_cap_floor_volatility: InflationCapFloorVolatility | null;
}

export interface GetManyInflationCapFloorVolatilitiesRequest {
    keys: InflationCapFloorVolatilityKey[];
}

export interface GetManyInflationCapFloorVolatilitiesResponse {
    result: Result;
    entries: InflationCapFloorVolatilityLookup[];
}

export interface PutInflationCapFloorVolatilityRequest {
    change: InflationCapFloorVolatilityChange;
    intent: ChangeIntent;
}

export interface PutInflationCapFloorVolatilityResponse {
    result: Result;
    inflation_cap_floor_volatility: InflationCapFloorVolatility | null;
}

export interface PutManyInflationCapFloorVolatilitiesRequest {
    changes: InflationCapFloorVolatilityChange[];
    intent: ChangeIntent;
}

export interface PutManyInflationCapFloorVolatilitiesResponse {
    result: Result;
    inflation_cap_floor_volatilities: InflationCapFloorVolatility[];
}

export interface DeleteInflationCapFloorVolatilityRequest {
    removal: InflationCapFloorVolatilityRemoval;
    intent: ChangeIntent;
}

export interface DeleteInflationCapFloorVolatilityResponse {
    result: Result;
}

export interface DeleteManyInflationCapFloorVolatilitiesRequest {
    removals: InflationCapFloorVolatilityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInflationCapFloorVolatilitiesResponse {
    result: Result;
}

export interface ListInflationCapFloorVolatilityVersionsRequest {
    key: InflationCapFloorVolatilityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InflationCapFloorVolatilityVersionsFilter | null;
}

export interface ListInflationCapFloorVolatilityVersionsResponse {
    result: Result;
    versions: InflationCapFloorVolatility[];
    total: number;
}

export interface GetInflationCapFloorVolatilityVersionRequest {
    key: InflationCapFloorVolatilityVersionKey;
}

export interface GetInflationCapFloorVolatilityVersionResponse {
    result: Result;
    version: InflationCapFloorVolatility | null;
}

export const subjects = {
    list_inflation_cap_floor_volatilities_request:
        'refdata.v1.inflation_cap_floor_volatilities.list',
    get_inflation_cap_floor_volatility_request: 'refdata.v1.inflation_cap_floor_volatilities.get',
    get_many_inflation_cap_floor_volatilities_request:
        'refdata.v1.inflation_cap_floor_volatilities.get_many',
    put_inflation_cap_floor_volatility_request: 'refdata.v1.inflation_cap_floor_volatilities.put',
    put_many_inflation_cap_floor_volatilities_request:
        'refdata.v1.inflation_cap_floor_volatilities.put_many',
    delete_inflation_cap_floor_volatility_request:
        'refdata.v1.inflation_cap_floor_volatilities.delete',
    delete_many_inflation_cap_floor_volatilities_request:
        'refdata.v1.inflation_cap_floor_volatilities.delete_many',
    list_inflation_cap_floor_volatility_versions_request:
        'refdata.v1.inflation_cap_floor_volatilities_versions.list',
    get_inflation_cap_floor_volatility_version_request:
        'refdata.v1.inflation_cap_floor_volatilities_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_inflation_cap_floor_volatilities_request: true,
    get_inflation_cap_floor_volatility_request: true,
    get_many_inflation_cap_floor_volatilities_request: true,
    put_inflation_cap_floor_volatility_request: true,
    put_many_inflation_cap_floor_volatilities_request: true,
    delete_inflation_cap_floor_volatility_request: true,
    delete_many_inflation_cap_floor_volatilities_request: true,
    list_inflation_cap_floor_volatility_versions_request: true,
    get_inflation_cap_floor_volatility_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.inflation_cap_floor_volatilities_events.created',
    updated: 'refdata.v1.inflation_cap_floor_volatilities_events.updated',
    deleted: 'refdata.v1.inflation_cap_floor_volatilities_events.deleted',
} as const;
