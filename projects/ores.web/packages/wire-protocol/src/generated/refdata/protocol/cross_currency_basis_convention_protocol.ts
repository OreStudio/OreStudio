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
import type { CrossCurrencyBasisConvention } from '../domain/cross_currency_basis_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CrossCurrencyBasisConventionKey {
    id: string;
}

export interface CrossCurrencyBasisConventionWrite {
    id: string;
    settlement_days: number;
    settlement_calendar: string | null;
    roll_convention: string;
    flat_index: string;
    spread_index: string;
    eom: boolean | null;
    is_resettable: boolean | null;
    flat_index_is_resettable: boolean | null;
    flat_tenor: string | null;
    spread_tenor: string | null;
    spread_payment_lag: number | null;
    flat_payment_lag: number | null;
    spread_include_spread: boolean | null;
    spread_lookback: string | null;
    spread_fixing_days: number | null;
    spread_rate_cutoff: number | null;
    spread_is_averaged: boolean | null;
    spread_observation_shift: boolean | null;
    flat_include_spread: boolean | null;
    flat_lookback: string | null;
    flat_fixing_days: number | null;
    flat_rate_cutoff: number | null;
    flat_is_averaged: boolean | null;
    flat_observation_shift: boolean | null;
}

export interface CrossCurrencyBasisConventionChange {
    write: CrossCurrencyBasisConventionWrite;
    precondition: Precondition;
}

export interface CrossCurrencyBasisConventionRemoval {
    key: CrossCurrencyBasisConventionKey;
    precondition: Precondition;
}

export interface CrossCurrencyBasisConventionLookup {
    key: CrossCurrencyBasisConventionKey;
    cross_currency_basis_convention: CrossCurrencyBasisConvention | null;
}

export interface CrossCurrencyBasisConventionEvent {
    event_id: string;
    key: CrossCurrencyBasisConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CrossCurrencyBasisConventionVersionKey {
    cross_currency_basis_convention: CrossCurrencyBasisConventionKey;
    version: number;
}

export interface CrossCurrencyBasisConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCrossCurrencyBasisConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCrossCurrencyBasisConventionsResponse {
    result: Result;
    cross_currency_basis_conventions: CrossCurrencyBasisConvention[];
    total: number;
}

export interface GetCrossCurrencyBasisConventionRequest {
    key: CrossCurrencyBasisConventionKey;
}

export interface GetCrossCurrencyBasisConventionResponse {
    result: Result;
    cross_currency_basis_convention: CrossCurrencyBasisConvention | null;
}

export interface GetManyCrossCurrencyBasisConventionsRequest {
    keys: CrossCurrencyBasisConventionKey[];
}

export interface GetManyCrossCurrencyBasisConventionsResponse {
    result: Result;
    entries: CrossCurrencyBasisConventionLookup[];
}

export interface PutCrossCurrencyBasisConventionRequest {
    change: CrossCurrencyBasisConventionChange;
    intent: ChangeIntent;
}

export interface PutCrossCurrencyBasisConventionResponse {
    result: Result;
    cross_currency_basis_convention: CrossCurrencyBasisConvention | null;
}

export interface PutManyCrossCurrencyBasisConventionsRequest {
    changes: CrossCurrencyBasisConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyCrossCurrencyBasisConventionsResponse {
    result: Result;
    cross_currency_basis_conventions: CrossCurrencyBasisConvention[];
}

export interface DeleteCrossCurrencyBasisConventionRequest {
    removal: CrossCurrencyBasisConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteCrossCurrencyBasisConventionResponse {
    result: Result;
}

export interface DeleteManyCrossCurrencyBasisConventionsRequest {
    removals: CrossCurrencyBasisConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCrossCurrencyBasisConventionsResponse {
    result: Result;
}

export interface ListCrossCurrencyBasisConventionVersionsRequest {
    key: CrossCurrencyBasisConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyBasisConventionVersionsFilter | null;
}

export interface ListCrossCurrencyBasisConventionVersionsResponse {
    result: Result;
    versions: CrossCurrencyBasisConvention[];
    total: number;
}

export interface GetCrossCurrencyBasisConventionVersionRequest {
    key: CrossCurrencyBasisConventionVersionKey;
}

export interface GetCrossCurrencyBasisConventionVersionResponse {
    result: Result;
    version: CrossCurrencyBasisConvention | null;
}

export const subjects = {
    list_cross_currency_basis_conventions_request:
        'refdata.v1.cross_currency_basis_conventions.list',
    get_cross_currency_basis_convention_request: 'refdata.v1.cross_currency_basis_conventions.get',
    get_many_cross_currency_basis_conventions_request:
        'refdata.v1.cross_currency_basis_conventions.get_many',
    put_cross_currency_basis_convention_request: 'refdata.v1.cross_currency_basis_conventions.put',
    put_many_cross_currency_basis_conventions_request:
        'refdata.v1.cross_currency_basis_conventions.put_many',
    delete_cross_currency_basis_convention_request:
        'refdata.v1.cross_currency_basis_conventions.delete',
    delete_many_cross_currency_basis_conventions_request:
        'refdata.v1.cross_currency_basis_conventions.delete_many',
    list_cross_currency_basis_convention_versions_request:
        'refdata.v1.cross_currency_basis_conventions_versions.list',
    get_cross_currency_basis_convention_version_request:
        'refdata.v1.cross_currency_basis_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cross_currency_basis_conventions_request: true,
    get_cross_currency_basis_convention_request: true,
    get_many_cross_currency_basis_conventions_request: true,
    put_cross_currency_basis_convention_request: true,
    put_many_cross_currency_basis_conventions_request: true,
    delete_cross_currency_basis_convention_request: true,
    delete_many_cross_currency_basis_conventions_request: true,
    list_cross_currency_basis_convention_versions_request: true,
    get_cross_currency_basis_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.cross_currency_basis_conventions_events.created',
    updated: 'refdata.v1.cross_currency_basis_conventions_events.updated',
    deleted: 'refdata.v1.cross_currency_basis_conventions_events.deleted',
} as const;
