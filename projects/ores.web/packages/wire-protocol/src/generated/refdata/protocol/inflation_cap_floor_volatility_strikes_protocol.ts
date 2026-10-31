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
import type { InflationCapFloorVolatilityStrikes } from '../domain/inflation_cap_floor_volatility_strikes.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InflationCapFloorVolatilityStrikesKey {
    id: string;
}

export interface InflationCapFloorVolatilityStrikesWrite {
    id: string;
    inflation_cap_floor_volatility_config_id: string;
    axis: string;
    strikes: string;
}

export interface InflationCapFloorVolatilityStrikesChange {
    write: InflationCapFloorVolatilityStrikesWrite;
    precondition: Precondition;
}

export interface InflationCapFloorVolatilityStrikesRemoval {
    key: InflationCapFloorVolatilityStrikesKey;
    precondition: Precondition;
}

export interface InflationCapFloorVolatilityStrikesLookup {
    key: InflationCapFloorVolatilityStrikesKey;
    inflation_cap_floor_volatility_strikes: InflationCapFloorVolatilityStrikes | null;
}

export interface InflationCapFloorVolatilityStrikesFilter {
    id_one_of: string[] | null;
}

export interface InflationCapFloorVolatilityStrikesEvent {
    event_id: string;
    key: InflationCapFloorVolatilityStrikesKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InflationCapFloorVolatilityStrikesVersionKey {
    inflation_cap_floor_volatility_strikes: InflationCapFloorVolatilityStrikesKey;
    version: number;
}

export interface InflationCapFloorVolatilityStrikesVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInflationCapFloorVolatilityStrikesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: InflationCapFloorVolatilityStrikesFilter | null;
    as_of: string | null;
}

export interface ListInflationCapFloorVolatilityStrikesResponse {
    result: Result;
    inflation_strikes: InflationCapFloorVolatilityStrikes[];
    total: number;
}

export interface GetInflationCapFloorVolatilityStrikesRequest {
    key: InflationCapFloorVolatilityStrikesKey;
}

export interface GetInflationCapFloorVolatilityStrikesResponse {
    result: Result;
    inflation_cap_floor_volatility_strikes: InflationCapFloorVolatilityStrikes | null;
}

export interface GetManyInflationCapFloorVolatilityStrikesRequest {
    keys: InflationCapFloorVolatilityStrikesKey[];
}

export interface GetManyInflationCapFloorVolatilityStrikesResponse {
    result: Result;
    entries: InflationCapFloorVolatilityStrikesLookup[];
}

export interface PutInflationCapFloorVolatilityStrikesRequest {
    change: InflationCapFloorVolatilityStrikesChange;
    intent: ChangeIntent;
}

export interface PutInflationCapFloorVolatilityStrikesResponse {
    result: Result;
    inflation_cap_floor_volatility_strikes: InflationCapFloorVolatilityStrikes | null;
}

export interface PutManyInflationCapFloorVolatilityStrikesRequest {
    changes: InflationCapFloorVolatilityStrikesChange[];
    intent: ChangeIntent;
}

export interface PutManyInflationCapFloorVolatilityStrikesResponse {
    result: Result;
    inflation_strikes: InflationCapFloorVolatilityStrikes[];
}

export interface DeleteInflationCapFloorVolatilityStrikesRequest {
    removal: InflationCapFloorVolatilityStrikesRemoval;
    intent: ChangeIntent;
}

export interface DeleteInflationCapFloorVolatilityStrikesResponse {
    result: Result;
}

export interface DeleteManyInflationCapFloorVolatilityStrikesRequest {
    removals: InflationCapFloorVolatilityStrikesRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInflationCapFloorVolatilityStrikesResponse {
    result: Result;
}

export interface ListInflationCapFloorVolatilityStrikesVersionsRequest {
    key: InflationCapFloorVolatilityStrikesKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InflationCapFloorVolatilityStrikesVersionsFilter | null;
}

export interface ListInflationCapFloorVolatilityStrikesVersionsResponse {
    result: Result;
    versions: InflationCapFloorVolatilityStrikes[];
    total: number;
}

export interface GetInflationCapFloorVolatilityStrikesVersionRequest {
    key: InflationCapFloorVolatilityStrikesVersionKey;
}

export interface GetInflationCapFloorVolatilityStrikesVersionResponse {
    result: Result;
    version: InflationCapFloorVolatilityStrikes | null;
}

export const subjects = {
    list_inflation_cap_floor_volatility_strikes_request:
        'refdata.v1.inflation_cap_floor_volatility_strikes.list',
    get_inflation_cap_floor_volatility_strikes_request:
        'refdata.v1.inflation_cap_floor_volatility_strikes.get',
    get_many_inflation_cap_floor_volatility_strikes_request:
        'refdata.v1.inflation_cap_floor_volatility_strikes.get_many',
    put_inflation_cap_floor_volatility_strikes_request:
        'refdata.v1.inflation_cap_floor_volatility_strikes.put',
    put_many_inflation_cap_floor_volatility_strikes_request:
        'refdata.v1.inflation_cap_floor_volatility_strikes.put_many',
    delete_inflation_cap_floor_volatility_strikes_request:
        'refdata.v1.inflation_cap_floor_volatility_strikes.delete',
    delete_many_inflation_cap_floor_volatility_strikes_request:
        'refdata.v1.inflation_cap_floor_volatility_strikes.delete_many',
    list_inflation_cap_floor_volatility_strikes_versions_request:
        'refdata.v1.inflation_cap_floor_volatility_strikes_versions.list',
    get_inflation_cap_floor_volatility_strikes_version_request:
        'refdata.v1.inflation_cap_floor_volatility_strikes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_inflation_cap_floor_volatility_strikes_request: true,
    get_inflation_cap_floor_volatility_strikes_request: true,
    get_many_inflation_cap_floor_volatility_strikes_request: true,
    put_inflation_cap_floor_volatility_strikes_request: true,
    put_many_inflation_cap_floor_volatility_strikes_request: true,
    delete_inflation_cap_floor_volatility_strikes_request: true,
    delete_many_inflation_cap_floor_volatility_strikes_request: true,
    list_inflation_cap_floor_volatility_strikes_versions_request: true,
    get_inflation_cap_floor_volatility_strikes_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.inflation_cap_floor_volatility_strikes_events.created',
    updated: 'refdata.v1.inflation_cap_floor_volatility_strikes_events.updated',
    deleted: 'refdata.v1.inflation_cap_floor_volatility_strikes_events.deleted',
} as const;
