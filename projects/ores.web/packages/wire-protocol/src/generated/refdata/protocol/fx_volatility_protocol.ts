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
import type { FxVolatility } from '../domain/fx_volatility.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxVolatilityKey {
    id: string;
}

export interface FxVolatilityWrite {
    id: string;
    curve_definition_id: string;
    dimension: string;
    smile_type: string | null;
    smile_interpolation: string | null;
    deltas: string | null;
    smile_delta: string | null;
    conventions: string | null;
    expiries: string | null;
    fx_spot_id: string | null;
    fx_foreign_curve_id: string | null;
    fx_domestic_curve_id: string | null;
    calendar: string | null;
    day_counter: string | null;
    fx_index_tag: string | null;
    base_volatility_1: string | null;
    base_volatility_2: string | null;
    smile_extrapolation: string | null;
    time_interpolation: string | null;
    time_weighting: string | null;
    butterfly_error_tolerance: number | null;
}

export interface FxVolatilityChange {
    write: FxVolatilityWrite;
    precondition: Precondition;
}

export interface FxVolatilityRemoval {
    key: FxVolatilityKey;
    precondition: Precondition;
}

export interface FxVolatilityLookup {
    key: FxVolatilityKey;
    fx_volatility: FxVolatility | null;
}

export interface FxVolatilityEvent {
    event_id: string;
    key: FxVolatilityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxVolatilityVersionKey {
    fx_volatility: FxVolatilityKey;
    version: number;
}

export interface FxVolatilityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxVolatilitiesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListFxVolatilitiesResponse {
    result: Result;
    fx_volatilities: FxVolatility[];
    total: number;
}

export interface GetFxVolatilityRequest {
    key: FxVolatilityKey;
}

export interface GetFxVolatilityResponse {
    result: Result;
    fx_volatility: FxVolatility | null;
}

export interface GetManyFxVolatilitiesRequest {
    keys: FxVolatilityKey[];
}

export interface GetManyFxVolatilitiesResponse {
    result: Result;
    entries: FxVolatilityLookup[];
}

export interface PutFxVolatilityRequest {
    change: FxVolatilityChange;
    intent: ChangeIntent;
}

export interface PutFxVolatilityResponse {
    result: Result;
    fx_volatility: FxVolatility | null;
}

export interface PutManyFxVolatilitiesRequest {
    changes: FxVolatilityChange[];
    intent: ChangeIntent;
}

export interface PutManyFxVolatilitiesResponse {
    result: Result;
    fx_volatilities: FxVolatility[];
}

export interface DeleteFxVolatilityRequest {
    removal: FxVolatilityRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxVolatilityResponse {
    result: Result;
}

export interface DeleteManyFxVolatilitiesRequest {
    removals: FxVolatilityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxVolatilitiesResponse {
    result: Result;
}

export interface ListFxVolatilityVersionsRequest {
    key: FxVolatilityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolatilityVersionsFilter | null;
}

export interface ListFxVolatilityVersionsResponse {
    result: Result;
    versions: FxVolatility[];
    total: number;
}

export interface GetFxVolatilityVersionRequest {
    key: FxVolatilityVersionKey;
}

export interface GetFxVolatilityVersionResponse {
    result: Result;
    version: FxVolatility | null;
}

export const subjects = {
    list_fx_volatilities_request: 'refdata.v1.fx_volatilities.list',
    get_fx_volatility_request: 'refdata.v1.fx_volatilities.get',
    get_many_fx_volatilities_request: 'refdata.v1.fx_volatilities.get_many',
    put_fx_volatility_request: 'refdata.v1.fx_volatilities.put',
    put_many_fx_volatilities_request: 'refdata.v1.fx_volatilities.put_many',
    delete_fx_volatility_request: 'refdata.v1.fx_volatilities.delete',
    delete_many_fx_volatilities_request: 'refdata.v1.fx_volatilities.delete_many',
    list_fx_volatility_versions_request: 'refdata.v1.fx_volatilities_versions.list',
    get_fx_volatility_version_request: 'refdata.v1.fx_volatilities_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_volatilities_request: true,
    get_fx_volatility_request: true,
    get_many_fx_volatilities_request: true,
    put_fx_volatility_request: true,
    put_many_fx_volatilities_request: true,
    delete_fx_volatility_request: true,
    delete_many_fx_volatilities_request: true,
    list_fx_volatility_versions_request: true,
    get_fx_volatility_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.fx_volatilities_events.created',
    updated: 'refdata.v1.fx_volatilities_events.updated',
    deleted: 'refdata.v1.fx_volatilities_events.deleted',
} as const;
