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
import type { EquityVolatility } from '../domain/equity_volatility.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityVolatilityKey {
    id: string;
}

export interface EquityVolatilityWrite {
    id: string;
    curve_definition_id: string;
    equity_id: string | null;
    currency: string;
    dimension: string | null;
    expiries: string | null;
    strikes: string | null;
    day_counter: string | null;
    time_extrapolation: string | null;
    strike_extrapolation: string | null;
    calendar: string | null;
    prefer_out_of_the_money: string | null;
    has_volatility_config: boolean;
}

export interface EquityVolatilityChange {
    write: EquityVolatilityWrite;
    precondition: Precondition;
}

export interface EquityVolatilityRemoval {
    key: EquityVolatilityKey;
    precondition: Precondition;
}

export interface EquityVolatilityLookup {
    key: EquityVolatilityKey;
    equity_volatility: EquityVolatility | null;
}

export interface EquityVolatilityEvent {
    event_id: string;
    key: EquityVolatilityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityVolatilityVersionKey {
    equity_volatility: EquityVolatilityKey;
    version: number;
}

export interface EquityVolatilityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityVolatilitiesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListEquityVolatilitiesResponse {
    result: Result;
    equity_volatilities: EquityVolatility[];
    total: number;
}

export interface GetEquityVolatilityRequest {
    key: EquityVolatilityKey;
}

export interface GetEquityVolatilityResponse {
    result: Result;
    equity_volatility: EquityVolatility | null;
}

export interface GetManyEquityVolatilitiesRequest {
    keys: EquityVolatilityKey[];
}

export interface GetManyEquityVolatilitiesResponse {
    result: Result;
    entries: EquityVolatilityLookup[];
}

export interface PutEquityVolatilityRequest {
    change: EquityVolatilityChange;
    intent: ChangeIntent;
}

export interface PutEquityVolatilityResponse {
    result: Result;
    equity_volatility: EquityVolatility | null;
}

export interface PutManyEquityVolatilitiesRequest {
    changes: EquityVolatilityChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityVolatilitiesResponse {
    result: Result;
    equity_volatilities: EquityVolatility[];
}

export interface DeleteEquityVolatilityRequest {
    removal: EquityVolatilityRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityVolatilityResponse {
    result: Result;
}

export interface DeleteManyEquityVolatilitiesRequest {
    removals: EquityVolatilityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityVolatilitiesResponse {
    result: Result;
}

export interface ListEquityVolatilityVersionsRequest {
    key: EquityVolatilityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityVolatilityVersionsFilter | null;
}

export interface ListEquityVolatilityVersionsResponse {
    result: Result;
    versions: EquityVolatility[];
    total: number;
}

export interface GetEquityVolatilityVersionRequest {
    key: EquityVolatilityVersionKey;
}

export interface GetEquityVolatilityVersionResponse {
    result: Result;
    version: EquityVolatility | null;
}

export const subjects = {
    list_equity_volatilities_request: 'refdata.v1.equity_volatilities.list',
    get_equity_volatility_request: 'refdata.v1.equity_volatilities.get',
    get_many_equity_volatilities_request: 'refdata.v1.equity_volatilities.get_many',
    put_equity_volatility_request: 'refdata.v1.equity_volatilities.put',
    put_many_equity_volatilities_request: 'refdata.v1.equity_volatilities.put_many',
    delete_equity_volatility_request: 'refdata.v1.equity_volatilities.delete',
    delete_many_equity_volatilities_request: 'refdata.v1.equity_volatilities.delete_many',
    list_equity_volatility_versions_request: 'refdata.v1.equity_volatilities_versions.list',
    get_equity_volatility_version_request: 'refdata.v1.equity_volatilities_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_volatilities_request: true,
    get_equity_volatility_request: true,
    get_many_equity_volatilities_request: true,
    put_equity_volatility_request: true,
    put_many_equity_volatilities_request: true,
    delete_equity_volatility_request: true,
    delete_many_equity_volatilities_request: true,
    list_equity_volatility_versions_request: true,
    get_equity_volatility_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.equity_volatilities_events.created',
    updated: 'refdata.v1.equity_volatilities_events.updated',
    deleted: 'refdata.v1.equity_volatilities_events.deleted',
} as const;
