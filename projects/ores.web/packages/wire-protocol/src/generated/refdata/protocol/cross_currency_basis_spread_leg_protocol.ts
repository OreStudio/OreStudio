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
import type { CrossCurrencyBasisSpreadLeg } from '../domain/cross_currency_basis_spread_leg.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CrossCurrencyBasisSpreadLegKey {
    id: string;
}

export interface CrossCurrencyBasisSpreadLegWrite {
    id: string;
    convention_id: string;
    spread_index: string;
    spread_tenor: string | null;
    spread_payment_lag: number | null;
    spread_include_spread: boolean | null;
    spread_lookback: string | null;
    spread_fixing_days: number | null;
    spread_rate_cutoff: number | null;
    spread_is_averaged: boolean | null;
    spread_observation_shift: boolean | null;
}

export interface CrossCurrencyBasisSpreadLegChange {
    write: CrossCurrencyBasisSpreadLegWrite;
    precondition: Precondition;
}

export interface CrossCurrencyBasisSpreadLegRemoval {
    key: CrossCurrencyBasisSpreadLegKey;
    precondition: Precondition;
}

export interface CrossCurrencyBasisSpreadLegLookup {
    key: CrossCurrencyBasisSpreadLegKey;
    cross_currency_basis_spread_leg: CrossCurrencyBasisSpreadLeg | null;
}

export interface CrossCurrencyBasisSpreadLegsFilter {
    convention_id: string | null;
    id_one_of: string[] | null;
    convention_id_one_of: string[] | null;
}

export interface CrossCurrencyBasisSpreadLegEvent {
    event_id: string;
    key: CrossCurrencyBasisSpreadLegKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CrossCurrencyBasisSpreadLegVersionKey {
    cross_currency_basis_spread_leg: CrossCurrencyBasisSpreadLegKey;
    version: number;
}

export interface CrossCurrencyBasisSpreadLegVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCrossCurrencyBasisSpreadLegsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyBasisSpreadLegsFilter | null;
    as_of: string | null;
}

export interface ListCrossCurrencyBasisSpreadLegsResponse {
    result: Result;
    cross_currency_basis_spread_legs: CrossCurrencyBasisSpreadLeg[];
    total: number;
}

export interface GetCrossCurrencyBasisSpreadLegRequest {
    key: CrossCurrencyBasisSpreadLegKey;
}

export interface GetCrossCurrencyBasisSpreadLegResponse {
    result: Result;
    cross_currency_basis_spread_leg: CrossCurrencyBasisSpreadLeg | null;
}

export interface GetManyCrossCurrencyBasisSpreadLegsRequest {
    keys: CrossCurrencyBasisSpreadLegKey[];
}

export interface GetManyCrossCurrencyBasisSpreadLegsResponse {
    result: Result;
    entries: CrossCurrencyBasisSpreadLegLookup[];
}

export interface PutCrossCurrencyBasisSpreadLegRequest {
    change: CrossCurrencyBasisSpreadLegChange;
    intent: ChangeIntent;
}

export interface PutCrossCurrencyBasisSpreadLegResponse {
    result: Result;
    cross_currency_basis_spread_leg: CrossCurrencyBasisSpreadLeg | null;
}

export interface PutManyCrossCurrencyBasisSpreadLegsRequest {
    changes: CrossCurrencyBasisSpreadLegChange[];
    intent: ChangeIntent;
}

export interface PutManyCrossCurrencyBasisSpreadLegsResponse {
    result: Result;
    cross_currency_basis_spread_legs: CrossCurrencyBasisSpreadLeg[];
}

export interface DeleteCrossCurrencyBasisSpreadLegRequest {
    removal: CrossCurrencyBasisSpreadLegRemoval;
    intent: ChangeIntent;
}

export interface DeleteCrossCurrencyBasisSpreadLegResponse {
    result: Result;
}

export interface DeleteManyCrossCurrencyBasisSpreadLegsRequest {
    removals: CrossCurrencyBasisSpreadLegRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCrossCurrencyBasisSpreadLegsResponse {
    result: Result;
}

export interface ListByConventionIdCrossCurrencyBasisSpreadLegsRequest {
    convention_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyBasisSpreadLegsFilter | null;
}

export interface ListByConventionIdCrossCurrencyBasisSpreadLegsResponse {
    result: Result;
    cross_currency_basis_spread_legs: CrossCurrencyBasisSpreadLeg[];
    total: number;
}

export interface ListCrossCurrencyBasisSpreadLegVersionsRequest {
    key: CrossCurrencyBasisSpreadLegKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyBasisSpreadLegVersionsFilter | null;
}

export interface ListCrossCurrencyBasisSpreadLegVersionsResponse {
    result: Result;
    versions: CrossCurrencyBasisSpreadLeg[];
    total: number;
}

export interface GetCrossCurrencyBasisSpreadLegVersionRequest {
    key: CrossCurrencyBasisSpreadLegVersionKey;
}

export interface GetCrossCurrencyBasisSpreadLegVersionResponse {
    result: Result;
    version: CrossCurrencyBasisSpreadLeg | null;
}

export const subjects = {
    list_cross_currency_basis_spread_legs_request:
        'refdata.v1.cross_currency_basis_spread_legs.list',
    get_cross_currency_basis_spread_leg_request: 'refdata.v1.cross_currency_basis_spread_legs.get',
    get_many_cross_currency_basis_spread_legs_request:
        'refdata.v1.cross_currency_basis_spread_legs.get_many',
    put_cross_currency_basis_spread_leg_request: 'refdata.v1.cross_currency_basis_spread_legs.put',
    put_many_cross_currency_basis_spread_legs_request:
        'refdata.v1.cross_currency_basis_spread_legs.put_many',
    delete_cross_currency_basis_spread_leg_request:
        'refdata.v1.cross_currency_basis_spread_legs.delete',
    delete_many_cross_currency_basis_spread_legs_request:
        'refdata.v1.cross_currency_basis_spread_legs.delete_many',
    list_by_convention_id_cross_currency_basis_spread_legs_request:
        'refdata.v1.cross_currency_basis_spread_legs.list_by_convention_id',
    list_cross_currency_basis_spread_leg_versions_request:
        'refdata.v1.cross_currency_basis_spread_legs_versions.list',
    get_cross_currency_basis_spread_leg_version_request:
        'refdata.v1.cross_currency_basis_spread_legs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cross_currency_basis_spread_legs_request: true,
    get_cross_currency_basis_spread_leg_request: true,
    get_many_cross_currency_basis_spread_legs_request: true,
    put_cross_currency_basis_spread_leg_request: true,
    put_many_cross_currency_basis_spread_legs_request: true,
    delete_cross_currency_basis_spread_leg_request: true,
    delete_many_cross_currency_basis_spread_legs_request: true,
    list_by_convention_id_cross_currency_basis_spread_legs_request: true,
    list_cross_currency_basis_spread_leg_versions_request: true,
    get_cross_currency_basis_spread_leg_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.cross_currency_basis_spread_legs.created',
    updated: 'refdata.v1.events.cross_currency_basis_spread_legs.updated',
    deleted: 'refdata.v1.events.cross_currency_basis_spread_legs.deleted',
} as const;
