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
import type { CrossCurrencyBasisFlatLeg } from '../domain/cross_currency_basis_flat_leg.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CrossCurrencyBasisFlatLegKey {
    id: string;
}

export interface CrossCurrencyBasisFlatLegWrite {
    id: string;
    convention_id: string;
    flat_index: string;
    flat_index_is_resettable: boolean | null;
    flat_tenor: string | null;
    flat_payment_lag: number | null;
    flat_include_spread: boolean | null;
    flat_lookback: string | null;
    flat_fixing_days: number | null;
    flat_rate_cutoff: number | null;
    flat_is_averaged: boolean | null;
    flat_observation_shift: boolean | null;
}

export interface CrossCurrencyBasisFlatLegChange {
    write: CrossCurrencyBasisFlatLegWrite;
    precondition: Precondition;
}

export interface CrossCurrencyBasisFlatLegRemoval {
    key: CrossCurrencyBasisFlatLegKey;
    precondition: Precondition;
}

export interface CrossCurrencyBasisFlatLegLookup {
    key: CrossCurrencyBasisFlatLegKey;
    cross_currency_basis_flat_leg: CrossCurrencyBasisFlatLeg | null;
}

export interface CrossCurrencyBasisFlatLegsFilter {
    convention_id: string | null;
    id_one_of: string[] | null;
    convention_id_one_of: string[] | null;
}

export interface CrossCurrencyBasisFlatLegEvent {
    event_id: string;
    key: CrossCurrencyBasisFlatLegKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CrossCurrencyBasisFlatLegVersionKey {
    cross_currency_basis_flat_leg: CrossCurrencyBasisFlatLegKey;
    version: number;
}

export interface CrossCurrencyBasisFlatLegVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCrossCurrencyBasisFlatLegsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyBasisFlatLegsFilter | null;
    as_of: string | null;
}

export interface ListCrossCurrencyBasisFlatLegsResponse {
    result: Result;
    cross_currency_basis_flat_legs: CrossCurrencyBasisFlatLeg[];
    total: number;
}

export interface GetCrossCurrencyBasisFlatLegRequest {
    key: CrossCurrencyBasisFlatLegKey;
}

export interface GetCrossCurrencyBasisFlatLegResponse {
    result: Result;
    cross_currency_basis_flat_leg: CrossCurrencyBasisFlatLeg | null;
}

export interface GetManyCrossCurrencyBasisFlatLegsRequest {
    keys: CrossCurrencyBasisFlatLegKey[];
}

export interface GetManyCrossCurrencyBasisFlatLegsResponse {
    result: Result;
    entries: CrossCurrencyBasisFlatLegLookup[];
}

export interface PutCrossCurrencyBasisFlatLegRequest {
    change: CrossCurrencyBasisFlatLegChange;
    intent: ChangeIntent;
}

export interface PutCrossCurrencyBasisFlatLegResponse {
    result: Result;
    cross_currency_basis_flat_leg: CrossCurrencyBasisFlatLeg | null;
}

export interface PutManyCrossCurrencyBasisFlatLegsRequest {
    changes: CrossCurrencyBasisFlatLegChange[];
    intent: ChangeIntent;
}

export interface PutManyCrossCurrencyBasisFlatLegsResponse {
    result: Result;
    cross_currency_basis_flat_legs: CrossCurrencyBasisFlatLeg[];
}

export interface DeleteCrossCurrencyBasisFlatLegRequest {
    removal: CrossCurrencyBasisFlatLegRemoval;
    intent: ChangeIntent;
}

export interface DeleteCrossCurrencyBasisFlatLegResponse {
    result: Result;
}

export interface DeleteManyCrossCurrencyBasisFlatLegsRequest {
    removals: CrossCurrencyBasisFlatLegRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCrossCurrencyBasisFlatLegsResponse {
    result: Result;
}

export interface ListByConventionIdCrossCurrencyBasisFlatLegsRequest {
    convention_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyBasisFlatLegsFilter | null;
}

export interface ListByConventionIdCrossCurrencyBasisFlatLegsResponse {
    result: Result;
    cross_currency_basis_flat_legs: CrossCurrencyBasisFlatLeg[];
    total: number;
}

export interface ListCrossCurrencyBasisFlatLegVersionsRequest {
    key: CrossCurrencyBasisFlatLegKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyBasisFlatLegVersionsFilter | null;
}

export interface ListCrossCurrencyBasisFlatLegVersionsResponse {
    result: Result;
    versions: CrossCurrencyBasisFlatLeg[];
    total: number;
}

export interface GetCrossCurrencyBasisFlatLegVersionRequest {
    key: CrossCurrencyBasisFlatLegVersionKey;
}

export interface GetCrossCurrencyBasisFlatLegVersionResponse {
    result: Result;
    version: CrossCurrencyBasisFlatLeg | null;
}

export const subjects = {
    list_cross_currency_basis_flat_legs_request: 'refdata.v1.cross_currency_basis_flat_legs.list',
    get_cross_currency_basis_flat_leg_request: 'refdata.v1.cross_currency_basis_flat_legs.get',
    get_many_cross_currency_basis_flat_legs_request:
        'refdata.v1.cross_currency_basis_flat_legs.get_many',
    put_cross_currency_basis_flat_leg_request: 'refdata.v1.cross_currency_basis_flat_legs.put',
    put_many_cross_currency_basis_flat_legs_request:
        'refdata.v1.cross_currency_basis_flat_legs.put_many',
    delete_cross_currency_basis_flat_leg_request:
        'refdata.v1.cross_currency_basis_flat_legs.delete',
    delete_many_cross_currency_basis_flat_legs_request:
        'refdata.v1.cross_currency_basis_flat_legs.delete_many',
    list_by_convention_id_cross_currency_basis_flat_legs_request:
        'refdata.v1.cross_currency_basis_flat_legs.list_by_convention_id',
    list_cross_currency_basis_flat_leg_versions_request:
        'refdata.v1.cross_currency_basis_flat_legs_versions.list',
    get_cross_currency_basis_flat_leg_version_request:
        'refdata.v1.cross_currency_basis_flat_legs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cross_currency_basis_flat_legs_request: true,
    get_cross_currency_basis_flat_leg_request: true,
    get_many_cross_currency_basis_flat_legs_request: true,
    put_cross_currency_basis_flat_leg_request: true,
    put_many_cross_currency_basis_flat_legs_request: true,
    delete_cross_currency_basis_flat_leg_request: true,
    delete_many_cross_currency_basis_flat_legs_request: true,
    list_by_convention_id_cross_currency_basis_flat_legs_request: true,
    list_cross_currency_basis_flat_leg_versions_request: true,
    get_cross_currency_basis_flat_leg_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.cross_currency_basis_flat_legs.created',
    updated: 'refdata.v1.events.cross_currency_basis_flat_legs.updated',
    deleted: 'refdata.v1.events.cross_currency_basis_flat_legs.deleted',
} as const;
