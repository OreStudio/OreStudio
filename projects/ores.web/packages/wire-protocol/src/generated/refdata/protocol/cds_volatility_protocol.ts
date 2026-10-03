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
import type { CdsVolatility } from '../domain/cds_volatility.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CdsVolatilityKey {
    id: string;
}

export interface CdsVolatilityWrite {
    id: string;
    curve_definition_id: string;
    expiries: string | null;
    day_counter: string | null;
    calendar: string | null;
    strike_type: string | null;
    quote_name: string | null;
    strike_factor: number | null;
}

export interface CdsVolatilityChange {
    write: CdsVolatilityWrite;
    precondition: Precondition;
}

export interface CdsVolatilityRemoval {
    key: CdsVolatilityKey;
    precondition: Precondition;
}

export interface CdsVolatilityLookup {
    key: CdsVolatilityKey;
    cds_volatility: CdsVolatility | null;
}

export interface CdsVolatilityEvent {
    event_id: string;
    key: CdsVolatilityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CdsVolatilityVersionKey {
    cds_volatility: CdsVolatilityKey;
    version: number;
}

export interface CdsVolatilityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCdsVolatilitiesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCdsVolatilitiesResponse {
    result: Result;
    cds_volatilities: CdsVolatility[];
    total: number;
}

export interface GetCdsVolatilityRequest {
    key: CdsVolatilityKey;
}

export interface GetCdsVolatilityResponse {
    result: Result;
    cds_volatility: CdsVolatility | null;
}

export interface GetManyCdsVolatilitiesRequest {
    keys: CdsVolatilityKey[];
}

export interface GetManyCdsVolatilitiesResponse {
    result: Result;
    entries: CdsVolatilityLookup[];
}

export interface PutCdsVolatilityRequest {
    change: CdsVolatilityChange;
    intent: ChangeIntent;
}

export interface PutCdsVolatilityResponse {
    result: Result;
    cds_volatility: CdsVolatility | null;
}

export interface PutManyCdsVolatilitiesRequest {
    changes: CdsVolatilityChange[];
    intent: ChangeIntent;
}

export interface PutManyCdsVolatilitiesResponse {
    result: Result;
    cds_volatilities: CdsVolatility[];
}

export interface DeleteCdsVolatilityRequest {
    removal: CdsVolatilityRemoval;
    intent: ChangeIntent;
}

export interface DeleteCdsVolatilityResponse {
    result: Result;
}

export interface DeleteManyCdsVolatilitiesRequest {
    removals: CdsVolatilityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCdsVolatilitiesResponse {
    result: Result;
}

export interface ListCdsVolatilityVersionsRequest {
    key: CdsVolatilityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CdsVolatilityVersionsFilter | null;
}

export interface ListCdsVolatilityVersionsResponse {
    result: Result;
    versions: CdsVolatility[];
    total: number;
}

export interface GetCdsVolatilityVersionRequest {
    key: CdsVolatilityVersionKey;
}

export interface GetCdsVolatilityVersionResponse {
    result: Result;
    version: CdsVolatility | null;
}

export const subjects = {
    list_cds_volatilities_request: 'refdata.v1.cds_volatilities.list',
    get_cds_volatility_request: 'refdata.v1.cds_volatilities.get',
    get_many_cds_volatilities_request: 'refdata.v1.cds_volatilities.get_many',
    put_cds_volatility_request: 'refdata.v1.cds_volatilities.put',
    put_many_cds_volatilities_request: 'refdata.v1.cds_volatilities.put_many',
    delete_cds_volatility_request: 'refdata.v1.cds_volatilities.delete',
    delete_many_cds_volatilities_request: 'refdata.v1.cds_volatilities.delete_many',
    list_cds_volatility_versions_request: 'refdata.v1.cds_volatilities_versions.list',
    get_cds_volatility_version_request: 'refdata.v1.cds_volatilities_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cds_volatilities_request: true,
    get_cds_volatility_request: true,
    get_many_cds_volatilities_request: true,
    put_cds_volatility_request: true,
    put_many_cds_volatilities_request: true,
    delete_cds_volatility_request: true,
    delete_many_cds_volatilities_request: true,
    list_cds_volatility_versions_request: true,
    get_cds_volatility_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.cds_volatilities_events.created',
    updated: 'refdata.v1.cds_volatilities_events.updated',
    deleted: 'refdata.v1.cds_volatilities_events.deleted',
} as const;
