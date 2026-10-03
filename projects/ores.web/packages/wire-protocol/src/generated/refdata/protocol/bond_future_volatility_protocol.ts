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
import type { BondFutureVolatility } from '../domain/bond_future_volatility.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondFutureVolatilityKey {
    id: string;
}

export interface BondFutureVolatilityWrite {
    id: string;
    curve_definition_id: string;
    contract_name: string;
    day_counter: string | null;
    calendar: string | null;
    yield_curve_id: string | null;
    strike_factor: number | null;
    use_only_put_call: string | null;
    prefer_out_of_the_money: string | null;
    treat_as_european: string | null;
    has_volatility_config: boolean;
}

export interface BondFutureVolatilityChange {
    write: BondFutureVolatilityWrite;
    precondition: Precondition;
}

export interface BondFutureVolatilityRemoval {
    key: BondFutureVolatilityKey;
    precondition: Precondition;
}

export interface BondFutureVolatilityLookup {
    key: BondFutureVolatilityKey;
    bond_future_volatility: BondFutureVolatility | null;
}

export interface BondFutureVolatilityEvent {
    event_id: string;
    key: BondFutureVolatilityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondFutureVolatilityVersionKey {
    bond_future_volatility: BondFutureVolatilityKey;
    version: number;
}

export interface BondFutureVolatilityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondFutureVolatilitiesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBondFutureVolatilitiesResponse {
    result: Result;
    bond_future_volatilities: BondFutureVolatility[];
    total: number;
}

export interface GetBondFutureVolatilityRequest {
    key: BondFutureVolatilityKey;
}

export interface GetBondFutureVolatilityResponse {
    result: Result;
    bond_future_volatility: BondFutureVolatility | null;
}

export interface GetManyBondFutureVolatilitiesRequest {
    keys: BondFutureVolatilityKey[];
}

export interface GetManyBondFutureVolatilitiesResponse {
    result: Result;
    entries: BondFutureVolatilityLookup[];
}

export interface PutBondFutureVolatilityRequest {
    change: BondFutureVolatilityChange;
    intent: ChangeIntent;
}

export interface PutBondFutureVolatilityResponse {
    result: Result;
    bond_future_volatility: BondFutureVolatility | null;
}

export interface PutManyBondFutureVolatilitiesRequest {
    changes: BondFutureVolatilityChange[];
    intent: ChangeIntent;
}

export interface PutManyBondFutureVolatilitiesResponse {
    result: Result;
    bond_future_volatilities: BondFutureVolatility[];
}

export interface DeleteBondFutureVolatilityRequest {
    removal: BondFutureVolatilityRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondFutureVolatilityResponse {
    result: Result;
}

export interface DeleteManyBondFutureVolatilitiesRequest {
    removals: BondFutureVolatilityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondFutureVolatilitiesResponse {
    result: Result;
}

export interface ListBondFutureVolatilityVersionsRequest {
    key: BondFutureVolatilityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondFutureVolatilityVersionsFilter | null;
}

export interface ListBondFutureVolatilityVersionsResponse {
    result: Result;
    versions: BondFutureVolatility[];
    total: number;
}

export interface GetBondFutureVolatilityVersionRequest {
    key: BondFutureVolatilityVersionKey;
}

export interface GetBondFutureVolatilityVersionResponse {
    result: Result;
    version: BondFutureVolatility | null;
}

export const subjects = {
    list_bond_future_volatilities_request: 'refdata.v1.bond_future_volatilities.list',
    get_bond_future_volatility_request: 'refdata.v1.bond_future_volatilities.get',
    get_many_bond_future_volatilities_request: 'refdata.v1.bond_future_volatilities.get_many',
    put_bond_future_volatility_request: 'refdata.v1.bond_future_volatilities.put',
    put_many_bond_future_volatilities_request: 'refdata.v1.bond_future_volatilities.put_many',
    delete_bond_future_volatility_request: 'refdata.v1.bond_future_volatilities.delete',
    delete_many_bond_future_volatilities_request: 'refdata.v1.bond_future_volatilities.delete_many',
    list_bond_future_volatility_versions_request:
        'refdata.v1.bond_future_volatilities_versions.list',
    get_bond_future_volatility_version_request: 'refdata.v1.bond_future_volatilities_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_future_volatilities_request: true,
    get_bond_future_volatility_request: true,
    get_many_bond_future_volatilities_request: true,
    put_bond_future_volatility_request: true,
    put_many_bond_future_volatilities_request: true,
    delete_bond_future_volatility_request: true,
    delete_many_bond_future_volatilities_request: true,
    list_bond_future_volatility_versions_request: true,
    get_bond_future_volatility_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.bond_future_volatilities_events.created',
    updated: 'refdata.v1.bond_future_volatilities_events.updated',
    deleted: 'refdata.v1.bond_future_volatilities_events.deleted',
} as const;
