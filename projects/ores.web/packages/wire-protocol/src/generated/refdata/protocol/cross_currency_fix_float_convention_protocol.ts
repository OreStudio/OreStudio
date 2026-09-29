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
import type { CrossCurrencyFixFloatConvention } from '../domain/cross_currency_fix_float_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CrossCurrencyFixFloatConventionKey {
    id: string;
}

export interface CrossCurrencyFixFloatConventionWrite {
    id: string;
    settlement_days: number;
    settlement_calendar: string;
    settlement_convention: string;
    fixed_currency: string;
    fixed_frequency: string;
    fixed_convention: string;
    fixed_day_count_fraction: string;
    index: string;
    eom: boolean | null;
    is_resettable: boolean | null;
    float_index_is_resettable: boolean | null;
    include_spread: boolean | null;
    lookback: string | null;
    fixing_days: number | null;
    rate_cutoff: number | null;
    is_averaged: boolean | null;
    observation_shift: boolean | null;
}

export interface CrossCurrencyFixFloatConventionChange {
    write: CrossCurrencyFixFloatConventionWrite;
    precondition: Precondition;
}

export interface CrossCurrencyFixFloatConventionRemoval {
    key: CrossCurrencyFixFloatConventionKey;
    precondition: Precondition;
}

export interface CrossCurrencyFixFloatConventionLookup {
    key: CrossCurrencyFixFloatConventionKey;
    cross_currency_fix_float_convention: CrossCurrencyFixFloatConvention | null;
}

export interface CrossCurrencyFixFloatConventionEvent {
    event_id: string;
    key: CrossCurrencyFixFloatConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CrossCurrencyFixFloatConventionVersionKey {
    cross_currency_fix_float_convention: CrossCurrencyFixFloatConventionKey;
    version: number;
}

export interface CrossCurrencyFixFloatConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCrossCurrencyFixFloatConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCrossCurrencyFixFloatConventionsResponse {
    result: Result;
    cross_currency_fix_float_conventions: CrossCurrencyFixFloatConvention[];
    total: number;
}

export interface GetCrossCurrencyFixFloatConventionRequest {
    key: CrossCurrencyFixFloatConventionKey;
}

export interface GetCrossCurrencyFixFloatConventionResponse {
    result: Result;
    cross_currency_fix_float_convention: CrossCurrencyFixFloatConvention | null;
}

export interface GetManyCrossCurrencyFixFloatConventionsRequest {
    keys: CrossCurrencyFixFloatConventionKey[];
}

export interface GetManyCrossCurrencyFixFloatConventionsResponse {
    result: Result;
    entries: CrossCurrencyFixFloatConventionLookup[];
}

export interface PutCrossCurrencyFixFloatConventionRequest {
    change: CrossCurrencyFixFloatConventionChange;
    intent: ChangeIntent;
}

export interface PutCrossCurrencyFixFloatConventionResponse {
    result: Result;
    cross_currency_fix_float_convention: CrossCurrencyFixFloatConvention | null;
}

export interface PutManyCrossCurrencyFixFloatConventionsRequest {
    changes: CrossCurrencyFixFloatConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyCrossCurrencyFixFloatConventionsResponse {
    result: Result;
    cross_currency_fix_float_conventions: CrossCurrencyFixFloatConvention[];
}

export interface DeleteCrossCurrencyFixFloatConventionRequest {
    removal: CrossCurrencyFixFloatConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteCrossCurrencyFixFloatConventionResponse {
    result: Result;
}

export interface DeleteManyCrossCurrencyFixFloatConventionsRequest {
    removals: CrossCurrencyFixFloatConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCrossCurrencyFixFloatConventionsResponse {
    result: Result;
}

export interface ListCrossCurrencyFixFloatConventionVersionsRequest {
    key: CrossCurrencyFixFloatConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyFixFloatConventionVersionsFilter | null;
}

export interface ListCrossCurrencyFixFloatConventionVersionsResponse {
    result: Result;
    versions: CrossCurrencyFixFloatConvention[];
    total: number;
}

export interface GetCrossCurrencyFixFloatConventionVersionRequest {
    key: CrossCurrencyFixFloatConventionVersionKey;
}

export interface GetCrossCurrencyFixFloatConventionVersionResponse {
    result: Result;
    version: CrossCurrencyFixFloatConvention | null;
}

export const subjects = {
    list_cross_currency_fix_float_conventions_request:
        'refdata.v1.cross_currency_fix_float_conventions.list',
    get_cross_currency_fix_float_convention_request:
        'refdata.v1.cross_currency_fix_float_conventions.get',
    get_many_cross_currency_fix_float_conventions_request:
        'refdata.v1.cross_currency_fix_float_conventions.get_many',
    put_cross_currency_fix_float_convention_request:
        'refdata.v1.cross_currency_fix_float_conventions.put',
    put_many_cross_currency_fix_float_conventions_request:
        'refdata.v1.cross_currency_fix_float_conventions.put_many',
    delete_cross_currency_fix_float_convention_request:
        'refdata.v1.cross_currency_fix_float_conventions.delete',
    delete_many_cross_currency_fix_float_conventions_request:
        'refdata.v1.cross_currency_fix_float_conventions.delete_many',
    list_cross_currency_fix_float_convention_versions_request:
        'refdata.v1.cross_currency_fix_float_conventions_versions.list',
    get_cross_currency_fix_float_convention_version_request:
        'refdata.v1.cross_currency_fix_float_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cross_currency_fix_float_conventions_request: true,
    get_cross_currency_fix_float_convention_request: true,
    get_many_cross_currency_fix_float_conventions_request: true,
    put_cross_currency_fix_float_convention_request: true,
    put_many_cross_currency_fix_float_conventions_request: true,
    delete_cross_currency_fix_float_convention_request: true,
    delete_many_cross_currency_fix_float_conventions_request: true,
    list_cross_currency_fix_float_convention_versions_request: true,
    get_cross_currency_fix_float_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.cross_currency_fix_float_conventions_events.created',
    updated: 'refdata.v1.cross_currency_fix_float_conventions_events.updated',
    deleted: 'refdata.v1.cross_currency_fix_float_conventions_events.deleted',
} as const;
