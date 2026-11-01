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
import type { CrossCurrencyFixFloatFixedLeg } from '../domain/cross_currency_fix_float_fixed_leg.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CrossCurrencyFixFloatFixedLegKey {
    id: string;
}

export interface CrossCurrencyFixFloatFixedLegWrite {
    id: string;
    convention_id: string;
    fixed_currency: string;
    fixed_frequency: string;
    fixed_convention: string;
    fixed_day_count_fraction: string;
}

export interface CrossCurrencyFixFloatFixedLegChange {
    write: CrossCurrencyFixFloatFixedLegWrite;
    precondition: Precondition;
}

export interface CrossCurrencyFixFloatFixedLegRemoval {
    key: CrossCurrencyFixFloatFixedLegKey;
    precondition: Precondition;
}

export interface CrossCurrencyFixFloatFixedLegLookup {
    key: CrossCurrencyFixFloatFixedLegKey;
    cross_currency_fix_float_fixed_leg: CrossCurrencyFixFloatFixedLeg | null;
}

export interface CrossCurrencyFixFloatFixedLegsFilter {
    convention_id: string | null;
    id_one_of: string[] | null;
    convention_id_one_of: string[] | null;
}

export interface CrossCurrencyFixFloatFixedLegEvent {
    event_id: string;
    key: CrossCurrencyFixFloatFixedLegKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CrossCurrencyFixFloatFixedLegVersionKey {
    cross_currency_fix_float_fixed_leg: CrossCurrencyFixFloatFixedLegKey;
    version: number;
}

export interface CrossCurrencyFixFloatFixedLegVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCrossCurrencyFixFloatFixedLegsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyFixFloatFixedLegsFilter | null;
    as_of: string | null;
}

export interface ListCrossCurrencyFixFloatFixedLegsResponse {
    result: Result;
    cross_currency_fix_float_fixed_legs: CrossCurrencyFixFloatFixedLeg[];
    total: number;
}

export interface GetCrossCurrencyFixFloatFixedLegRequest {
    key: CrossCurrencyFixFloatFixedLegKey;
}

export interface GetCrossCurrencyFixFloatFixedLegResponse {
    result: Result;
    cross_currency_fix_float_fixed_leg: CrossCurrencyFixFloatFixedLeg | null;
}

export interface GetManyCrossCurrencyFixFloatFixedLegsRequest {
    keys: CrossCurrencyFixFloatFixedLegKey[];
}

export interface GetManyCrossCurrencyFixFloatFixedLegsResponse {
    result: Result;
    entries: CrossCurrencyFixFloatFixedLegLookup[];
}

export interface PutCrossCurrencyFixFloatFixedLegRequest {
    change: CrossCurrencyFixFloatFixedLegChange;
    intent: ChangeIntent;
}

export interface PutCrossCurrencyFixFloatFixedLegResponse {
    result: Result;
    cross_currency_fix_float_fixed_leg: CrossCurrencyFixFloatFixedLeg | null;
}

export interface PutManyCrossCurrencyFixFloatFixedLegsRequest {
    changes: CrossCurrencyFixFloatFixedLegChange[];
    intent: ChangeIntent;
}

export interface PutManyCrossCurrencyFixFloatFixedLegsResponse {
    result: Result;
    cross_currency_fix_float_fixed_legs: CrossCurrencyFixFloatFixedLeg[];
}

export interface DeleteCrossCurrencyFixFloatFixedLegRequest {
    removal: CrossCurrencyFixFloatFixedLegRemoval;
    intent: ChangeIntent;
}

export interface DeleteCrossCurrencyFixFloatFixedLegResponse {
    result: Result;
}

export interface DeleteManyCrossCurrencyFixFloatFixedLegsRequest {
    removals: CrossCurrencyFixFloatFixedLegRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCrossCurrencyFixFloatFixedLegsResponse {
    result: Result;
}

export interface ListByConventionIdCrossCurrencyFixFloatFixedLegsRequest {
    convention_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyFixFloatFixedLegsFilter | null;
}

export interface ListByConventionIdCrossCurrencyFixFloatFixedLegsResponse {
    result: Result;
    cross_currency_fix_float_fixed_legs: CrossCurrencyFixFloatFixedLeg[];
    total: number;
}

export interface ListCrossCurrencyFixFloatFixedLegVersionsRequest {
    key: CrossCurrencyFixFloatFixedLegKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyFixFloatFixedLegVersionsFilter | null;
}

export interface ListCrossCurrencyFixFloatFixedLegVersionsResponse {
    result: Result;
    versions: CrossCurrencyFixFloatFixedLeg[];
    total: number;
}

export interface GetCrossCurrencyFixFloatFixedLegVersionRequest {
    key: CrossCurrencyFixFloatFixedLegVersionKey;
}

export interface GetCrossCurrencyFixFloatFixedLegVersionResponse {
    result: Result;
    version: CrossCurrencyFixFloatFixedLeg | null;
}

export const subjects = {
    list_cross_currency_fix_float_fixed_legs_request:
        'refdata.v1.cross_currency_fix_float_fixed_legs.list',
    get_cross_currency_fix_float_fixed_leg_request:
        'refdata.v1.cross_currency_fix_float_fixed_legs.get',
    get_many_cross_currency_fix_float_fixed_legs_request:
        'refdata.v1.cross_currency_fix_float_fixed_legs.get_many',
    put_cross_currency_fix_float_fixed_leg_request:
        'refdata.v1.cross_currency_fix_float_fixed_legs.put',
    put_many_cross_currency_fix_float_fixed_legs_request:
        'refdata.v1.cross_currency_fix_float_fixed_legs.put_many',
    delete_cross_currency_fix_float_fixed_leg_request:
        'refdata.v1.cross_currency_fix_float_fixed_legs.delete',
    delete_many_cross_currency_fix_float_fixed_legs_request:
        'refdata.v1.cross_currency_fix_float_fixed_legs.delete_many',
    list_by_convention_id_cross_currency_fix_float_fixed_legs_request:
        'refdata.v1.cross_currency_fix_float_fixed_legs.list_by_convention_id',
    list_cross_currency_fix_float_fixed_leg_versions_request:
        'refdata.v1.cross_currency_fix_float_fixed_legs_versions.list',
    get_cross_currency_fix_float_fixed_leg_version_request:
        'refdata.v1.cross_currency_fix_float_fixed_legs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cross_currency_fix_float_fixed_legs_request: true,
    get_cross_currency_fix_float_fixed_leg_request: true,
    get_many_cross_currency_fix_float_fixed_legs_request: true,
    put_cross_currency_fix_float_fixed_leg_request: true,
    put_many_cross_currency_fix_float_fixed_legs_request: true,
    delete_cross_currency_fix_float_fixed_leg_request: true,
    delete_many_cross_currency_fix_float_fixed_legs_request: true,
    list_by_convention_id_cross_currency_fix_float_fixed_legs_request: true,
    list_cross_currency_fix_float_fixed_leg_versions_request: true,
    get_cross_currency_fix_float_fixed_leg_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.cross_currency_fix_float_fixed_legs_events.created',
    updated: 'refdata.v1.cross_currency_fix_float_fixed_legs_events.updated',
    deleted: 'refdata.v1.cross_currency_fix_float_fixed_legs_events.deleted',
} as const;
