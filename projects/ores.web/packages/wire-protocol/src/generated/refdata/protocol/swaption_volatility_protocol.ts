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
import type { SwaptionVolatility } from '../domain/swaption_volatility.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SwaptionVolatilityKey {
    id: string;
}

export interface SwaptionVolatilityWrite {
    id: string;
    curve_definition_id: string;
    dimension: string | null;
    volatility_type: string | null;
    interpolation: string | null;
    extrapolation: string | null;
    output_volatility_type: string | null;
    model_shift: string | null;
    output_shift: string | null;
    day_counter: string | null;
    calendar: string | null;
    business_day_convention: string | null;
    option_tenors: string | null;
    swap_tenors: string | null;
    short_swap_index_base: string | null;
    swap_index_base: string | null;
    smile_option_tenors: string | null;
    smile_swap_tenors: string | null;
    smile_spreads: string | null;
    quote_tag: string | null;
    has_proxy_config: boolean;
    proxy_source_curve_id: string | null;
    proxy_source_short_swap_index_base: string | null;
    proxy_source_swap_index_base: string | null;
    proxy_target_short_swap_index_base: string | null;
    proxy_target_swap_index_base: string | null;
}

export interface SwaptionVolatilityChange {
    write: SwaptionVolatilityWrite;
    precondition: Precondition;
}

export interface SwaptionVolatilityRemoval {
    key: SwaptionVolatilityKey;
    precondition: Precondition;
}

export interface SwaptionVolatilityLookup {
    key: SwaptionVolatilityKey;
    swaption_volatility: SwaptionVolatility | null;
}

export interface SwaptionVolatilityEvent {
    event_id: string;
    key: SwaptionVolatilityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SwaptionVolatilityVersionKey {
    swaption_volatility: SwaptionVolatilityKey;
    version: number;
}

export interface SwaptionVolatilityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSwaptionVolatilitiesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListSwaptionVolatilitiesResponse {
    result: Result;
    swaption_volatilities: SwaptionVolatility[];
    total: number;
}

export interface GetSwaptionVolatilityRequest {
    key: SwaptionVolatilityKey;
}

export interface GetSwaptionVolatilityResponse {
    result: Result;
    swaption_volatility: SwaptionVolatility | null;
}

export interface GetManySwaptionVolatilitiesRequest {
    keys: SwaptionVolatilityKey[];
}

export interface GetManySwaptionVolatilitiesResponse {
    result: Result;
    entries: SwaptionVolatilityLookup[];
}

export interface PutSwaptionVolatilityRequest {
    change: SwaptionVolatilityChange;
    intent: ChangeIntent;
}

export interface PutSwaptionVolatilityResponse {
    result: Result;
    swaption_volatility: SwaptionVolatility | null;
}

export interface PutManySwaptionVolatilitiesRequest {
    changes: SwaptionVolatilityChange[];
    intent: ChangeIntent;
}

export interface PutManySwaptionVolatilitiesResponse {
    result: Result;
    swaption_volatilities: SwaptionVolatility[];
}

export interface DeleteSwaptionVolatilityRequest {
    removal: SwaptionVolatilityRemoval;
    intent: ChangeIntent;
}

export interface DeleteSwaptionVolatilityResponse {
    result: Result;
}

export interface DeleteManySwaptionVolatilitiesRequest {
    removals: SwaptionVolatilityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySwaptionVolatilitiesResponse {
    result: Result;
}

export interface ListSwaptionVolatilityVersionsRequest {
    key: SwaptionVolatilityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SwaptionVolatilityVersionsFilter | null;
}

export interface ListSwaptionVolatilityVersionsResponse {
    result: Result;
    versions: SwaptionVolatility[];
    total: number;
}

export interface GetSwaptionVolatilityVersionRequest {
    key: SwaptionVolatilityVersionKey;
}

export interface GetSwaptionVolatilityVersionResponse {
    result: Result;
    version: SwaptionVolatility | null;
}

export const subjects = {
    list_swaption_volatilities_request: 'refdata.v1.swaption_volatilities.list',
    get_swaption_volatility_request: 'refdata.v1.swaption_volatilities.get',
    get_many_swaption_volatilities_request: 'refdata.v1.swaption_volatilities.get_many',
    put_swaption_volatility_request: 'refdata.v1.swaption_volatilities.put',
    put_many_swaption_volatilities_request: 'refdata.v1.swaption_volatilities.put_many',
    delete_swaption_volatility_request: 'refdata.v1.swaption_volatilities.delete',
    delete_many_swaption_volatilities_request: 'refdata.v1.swaption_volatilities.delete_many',
    list_swaption_volatility_versions_request: 'refdata.v1.swaption_volatilities_versions.list',
    get_swaption_volatility_version_request: 'refdata.v1.swaption_volatilities_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_swaption_volatilities_request: true,
    get_swaption_volatility_request: true,
    get_many_swaption_volatilities_request: true,
    put_swaption_volatility_request: true,
    put_many_swaption_volatilities_request: true,
    delete_swaption_volatility_request: true,
    delete_many_swaption_volatilities_request: true,
    list_swaption_volatility_versions_request: true,
    get_swaption_volatility_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.swaption_volatilities_events.created',
    updated: 'refdata.v1.swaption_volatilities_events.updated',
    deleted: 'refdata.v1.swaption_volatilities_events.deleted',
} as const;
