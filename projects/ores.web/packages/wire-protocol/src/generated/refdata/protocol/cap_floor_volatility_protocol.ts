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
import type { CapFloorVolatility } from '../domain/cap_floor_volatility.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CapFloorVolatilityKey {
    id: string;
}

export interface CapFloorVolatilityWrite {
    id: string;
    curve_definition_id: string;
    volatility_type: string | null;
    output_volatility_type: string | null;
    model_shift: number | null;
    output_shift: number | null;
    extrapolation: string | null;
    interpolation_method: string | null;
    include_atm: string | null;
    day_counter: string | null;
    calendar: string | null;
    business_day_convention: string | null;
    tenors: string | null;
    strikes: string | null;
    optional_quotes: string | null;
    ibor_index: string | null;
    index: string | null;
    rate_computation_period: string | null;
    on_cap_settlement_days: number | null;
    discount_curve: string | null;
    atm_tenors: string | null;
    settlement_days: number | null;
    interpolate_on: string | null;
    time_interpolation: string | null;
    strike_interpolation: string | null;
    input_type: string | null;
    quote_includes_index_name: string | null;
    flat_first_period: string | null;
    use_effecive_volatility: string | null;
    use_effective_volatility: string | null;
    has_proxy_config: boolean;
    proxy_source_curve_id: string | null;
    proxy_source_index: string | null;
    proxy_source_rate_computation_period: string | null;
    proxy_target_index: string | null;
    proxy_target_rate_computation_period: string | null;
    proxy_target_on_cap_settlement_days: number | null;
    proxy_scaling_factor: number | null;
}

export interface CapFloorVolatilityChange {
    write: CapFloorVolatilityWrite;
    precondition: Precondition;
}

export interface CapFloorVolatilityRemoval {
    key: CapFloorVolatilityKey;
    precondition: Precondition;
}

export interface CapFloorVolatilityLookup {
    key: CapFloorVolatilityKey;
    cap_floor_volatility: CapFloorVolatility | null;
}

export interface CapFloorVolatilityEvent {
    event_id: string;
    key: CapFloorVolatilityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CapFloorVolatilityVersionKey {
    cap_floor_volatility: CapFloorVolatilityKey;
    version: number;
}

export interface CapFloorVolatilityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCapFloorVolatilitiesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCapFloorVolatilitiesResponse {
    result: Result;
    cap_floor_volatilities: CapFloorVolatility[];
    total: number;
}

export interface GetCapFloorVolatilityRequest {
    key: CapFloorVolatilityKey;
}

export interface GetCapFloorVolatilityResponse {
    result: Result;
    cap_floor_volatility: CapFloorVolatility | null;
}

export interface GetManyCapFloorVolatilitiesRequest {
    keys: CapFloorVolatilityKey[];
}

export interface GetManyCapFloorVolatilitiesResponse {
    result: Result;
    entries: CapFloorVolatilityLookup[];
}

export interface PutCapFloorVolatilityRequest {
    change: CapFloorVolatilityChange;
    intent: ChangeIntent;
}

export interface PutCapFloorVolatilityResponse {
    result: Result;
    cap_floor_volatility: CapFloorVolatility | null;
}

export interface PutManyCapFloorVolatilitiesRequest {
    changes: CapFloorVolatilityChange[];
    intent: ChangeIntent;
}

export interface PutManyCapFloorVolatilitiesResponse {
    result: Result;
    cap_floor_volatilities: CapFloorVolatility[];
}

export interface DeleteCapFloorVolatilityRequest {
    removal: CapFloorVolatilityRemoval;
    intent: ChangeIntent;
}

export interface DeleteCapFloorVolatilityResponse {
    result: Result;
}

export interface DeleteManyCapFloorVolatilitiesRequest {
    removals: CapFloorVolatilityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCapFloorVolatilitiesResponse {
    result: Result;
}

export interface ListCapFloorVolatilityVersionsRequest {
    key: CapFloorVolatilityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CapFloorVolatilityVersionsFilter | null;
}

export interface ListCapFloorVolatilityVersionsResponse {
    result: Result;
    versions: CapFloorVolatility[];
    total: number;
}

export interface GetCapFloorVolatilityVersionRequest {
    key: CapFloorVolatilityVersionKey;
}

export interface GetCapFloorVolatilityVersionResponse {
    result: Result;
    version: CapFloorVolatility | null;
}

export const subjects = {
    list_cap_floor_volatilities_request: 'refdata.v1.cap_floor_volatilities.list',
    get_cap_floor_volatility_request: 'refdata.v1.cap_floor_volatilities.get',
    get_many_cap_floor_volatilities_request: 'refdata.v1.cap_floor_volatilities.get_many',
    put_cap_floor_volatility_request: 'refdata.v1.cap_floor_volatilities.put',
    put_many_cap_floor_volatilities_request: 'refdata.v1.cap_floor_volatilities.put_many',
    delete_cap_floor_volatility_request: 'refdata.v1.cap_floor_volatilities.delete',
    delete_many_cap_floor_volatilities_request: 'refdata.v1.cap_floor_volatilities.delete_many',
    list_cap_floor_volatility_versions_request: 'refdata.v1.cap_floor_volatilities_versions.list',
    get_cap_floor_volatility_version_request: 'refdata.v1.cap_floor_volatilities_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cap_floor_volatilities_request: true,
    get_cap_floor_volatility_request: true,
    get_many_cap_floor_volatilities_request: true,
    put_cap_floor_volatility_request: true,
    put_many_cap_floor_volatilities_request: true,
    delete_cap_floor_volatility_request: true,
    delete_many_cap_floor_volatilities_request: true,
    list_cap_floor_volatility_versions_request: true,
    get_cap_floor_volatility_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.cap_floor_volatilities_events.created',
    updated: 'refdata.v1.cap_floor_volatilities_events.updated',
    deleted: 'refdata.v1.cap_floor_volatilities_events.deleted',
} as const;
