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
import type { CrossCurrencyFixFloatFloatLeg } from '../domain/cross_currency_fix_float_float_leg.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CrossCurrencyFixFloatFloatLegKey {
    id: string;
}

export interface CrossCurrencyFixFloatFloatLegWrite {
    id: string;
    convention_id: string;
    index: string;
    float_index_is_resettable: boolean | null;
    include_spread: boolean | null;
    lookback: string | null;
    fixing_days: number | null;
    rate_cutoff: number | null;
    is_averaged: boolean | null;
    observation_shift: boolean | null;
}

export interface CrossCurrencyFixFloatFloatLegChange {
    write: CrossCurrencyFixFloatFloatLegWrite;
    precondition: Precondition;
}

export interface CrossCurrencyFixFloatFloatLegRemoval {
    key: CrossCurrencyFixFloatFloatLegKey;
    precondition: Precondition;
}

export interface CrossCurrencyFixFloatFloatLegLookup {
    key: CrossCurrencyFixFloatFloatLegKey;
    cross_currency_fix_float_float_leg: CrossCurrencyFixFloatFloatLeg | null;
}

export interface CrossCurrencyFixFloatFloatLegsFilter {
    convention_id: string | null;
    id_one_of: string[] | null;
    convention_id_one_of: string[] | null;
}

export interface CrossCurrencyFixFloatFloatLegEvent {
    event_id: string;
    key: CrossCurrencyFixFloatFloatLegKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CrossCurrencyFixFloatFloatLegVersionKey {
    cross_currency_fix_float_float_leg: CrossCurrencyFixFloatFloatLegKey;
    version: number;
}

export interface CrossCurrencyFixFloatFloatLegVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCrossCurrencyFixFloatFloatLegsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyFixFloatFloatLegsFilter | null;
    as_of: string | null;
}

export interface ListCrossCurrencyFixFloatFloatLegsResponse {
    result: Result;
    cross_currency_fix_float_float_legs: CrossCurrencyFixFloatFloatLeg[];
    total: number;
}

export interface GetCrossCurrencyFixFloatFloatLegRequest {
    key: CrossCurrencyFixFloatFloatLegKey;
}

export interface GetCrossCurrencyFixFloatFloatLegResponse {
    result: Result;
    cross_currency_fix_float_float_leg: CrossCurrencyFixFloatFloatLeg | null;
}

export interface GetManyCrossCurrencyFixFloatFloatLegsRequest {
    keys: CrossCurrencyFixFloatFloatLegKey[];
}

export interface GetManyCrossCurrencyFixFloatFloatLegsResponse {
    result: Result;
    entries: CrossCurrencyFixFloatFloatLegLookup[];
}

export interface PutCrossCurrencyFixFloatFloatLegRequest {
    change: CrossCurrencyFixFloatFloatLegChange;
    intent: ChangeIntent;
}

export interface PutCrossCurrencyFixFloatFloatLegResponse {
    result: Result;
    cross_currency_fix_float_float_leg: CrossCurrencyFixFloatFloatLeg | null;
}

export interface PutManyCrossCurrencyFixFloatFloatLegsRequest {
    changes: CrossCurrencyFixFloatFloatLegChange[];
    intent: ChangeIntent;
}

export interface PutManyCrossCurrencyFixFloatFloatLegsResponse {
    result: Result;
    cross_currency_fix_float_float_legs: CrossCurrencyFixFloatFloatLeg[];
}

export interface DeleteCrossCurrencyFixFloatFloatLegRequest {
    removal: CrossCurrencyFixFloatFloatLegRemoval;
    intent: ChangeIntent;
}

export interface DeleteCrossCurrencyFixFloatFloatLegResponse {
    result: Result;
}

export interface DeleteManyCrossCurrencyFixFloatFloatLegsRequest {
    removals: CrossCurrencyFixFloatFloatLegRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCrossCurrencyFixFloatFloatLegsResponse {
    result: Result;
}

export interface ListByConventionIdCrossCurrencyFixFloatFloatLegsRequest {
    convention_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyFixFloatFloatLegsFilter | null;
}

export interface ListByConventionIdCrossCurrencyFixFloatFloatLegsResponse {
    result: Result;
    cross_currency_fix_float_float_legs: CrossCurrencyFixFloatFloatLeg[];
    total: number;
}

export interface ListCrossCurrencyFixFloatFloatLegVersionsRequest {
    key: CrossCurrencyFixFloatFloatLegKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyFixFloatFloatLegVersionsFilter | null;
}

export interface ListCrossCurrencyFixFloatFloatLegVersionsResponse {
    result: Result;
    versions: CrossCurrencyFixFloatFloatLeg[];
    total: number;
}

export interface GetCrossCurrencyFixFloatFloatLegVersionRequest {
    key: CrossCurrencyFixFloatFloatLegVersionKey;
}

export interface GetCrossCurrencyFixFloatFloatLegVersionResponse {
    result: Result;
    version: CrossCurrencyFixFloatFloatLeg | null;
}

export const subjects = {
    list_cross_currency_fix_float_float_legs_request:
        'refdata.v1.cross_currency_fix_float_float_legs.list',
    get_cross_currency_fix_float_float_leg_request:
        'refdata.v1.cross_currency_fix_float_float_legs.get',
    get_many_cross_currency_fix_float_float_legs_request:
        'refdata.v1.cross_currency_fix_float_float_legs.get_many',
    put_cross_currency_fix_float_float_leg_request:
        'refdata.v1.cross_currency_fix_float_float_legs.put',
    put_many_cross_currency_fix_float_float_legs_request:
        'refdata.v1.cross_currency_fix_float_float_legs.put_many',
    delete_cross_currency_fix_float_float_leg_request:
        'refdata.v1.cross_currency_fix_float_float_legs.delete',
    delete_many_cross_currency_fix_float_float_legs_request:
        'refdata.v1.cross_currency_fix_float_float_legs.delete_many',
    list_by_convention_id_cross_currency_fix_float_float_legs_request:
        'refdata.v1.cross_currency_fix_float_float_legs.list_by_convention_id',
    list_cross_currency_fix_float_float_leg_versions_request:
        'refdata.v1.cross_currency_fix_float_float_legs_versions.list',
    get_cross_currency_fix_float_float_leg_version_request:
        'refdata.v1.cross_currency_fix_float_float_legs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cross_currency_fix_float_float_legs_request: true,
    get_cross_currency_fix_float_float_leg_request: true,
    get_many_cross_currency_fix_float_float_legs_request: true,
    put_cross_currency_fix_float_float_leg_request: true,
    put_many_cross_currency_fix_float_float_legs_request: true,
    delete_cross_currency_fix_float_float_leg_request: true,
    delete_many_cross_currency_fix_float_float_legs_request: true,
    list_by_convention_id_cross_currency_fix_float_float_legs_request: true,
    list_cross_currency_fix_float_float_leg_versions_request: true,
    get_cross_currency_fix_float_float_leg_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.cross_currency_fix_float_float_legs_events.created',
    updated: 'refdata.v1.cross_currency_fix_float_float_legs_events.updated',
    deleted: 'refdata.v1.cross_currency_fix_float_float_legs_events.deleted',
} as const;
