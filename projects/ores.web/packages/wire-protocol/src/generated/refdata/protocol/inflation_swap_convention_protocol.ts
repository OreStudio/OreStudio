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
import type { InflationSwapConvention } from '../domain/inflation_swap_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InflationSwapConventionKey {
    id: string;
}

export interface InflationSwapConventionWrite {
    id: string;
    fix_calendar: string;
    fix_convention: string;
    day_count_fraction: string;
    index: string;
    interpolated: boolean;
    observation_lag: string;
    adjust_inflation_observation_dates: boolean;
    inflation_calendar: string;
    inflation_convention: string;
    publication_roll: string | null;
    start_delay: string | null;
    start_delay_convention: string | null;
}

export interface InflationSwapConventionChange {
    write: InflationSwapConventionWrite;
    precondition: Precondition;
}

export interface InflationSwapConventionRemoval {
    key: InflationSwapConventionKey;
    precondition: Precondition;
}

export interface InflationSwapConventionLookup {
    key: InflationSwapConventionKey;
    inflation_swap_convention: InflationSwapConvention | null;
}

export interface InflationSwapConventionEvent {
    event_id: string;
    key: InflationSwapConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InflationSwapConventionVersionKey {
    inflation_swap_convention: InflationSwapConventionKey;
    version: number;
}

export interface InflationSwapConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInflationSwapConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListInflationSwapConventionsResponse {
    result: Result;
    inflation_swap_conventions: InflationSwapConvention[];
    total: number;
}

export interface GetInflationSwapConventionRequest {
    key: InflationSwapConventionKey;
}

export interface GetInflationSwapConventionResponse {
    result: Result;
    inflation_swap_convention: InflationSwapConvention | null;
}

export interface GetManyInflationSwapConventionsRequest {
    keys: InflationSwapConventionKey[];
}

export interface GetManyInflationSwapConventionsResponse {
    result: Result;
    entries: InflationSwapConventionLookup[];
}

export interface PutInflationSwapConventionRequest {
    change: InflationSwapConventionChange;
    intent: ChangeIntent;
}

export interface PutInflationSwapConventionResponse {
    result: Result;
    inflation_swap_convention: InflationSwapConvention | null;
}

export interface PutManyInflationSwapConventionsRequest {
    changes: InflationSwapConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyInflationSwapConventionsResponse {
    result: Result;
    inflation_swap_conventions: InflationSwapConvention[];
}

export interface DeleteInflationSwapConventionRequest {
    removal: InflationSwapConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteInflationSwapConventionResponse {
    result: Result;
}

export interface DeleteManyInflationSwapConventionsRequest {
    removals: InflationSwapConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInflationSwapConventionsResponse {
    result: Result;
}

export interface ListInflationSwapConventionVersionsRequest {
    key: InflationSwapConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InflationSwapConventionVersionsFilter | null;
}

export interface ListInflationSwapConventionVersionsResponse {
    result: Result;
    versions: InflationSwapConvention[];
    total: number;
}

export interface GetInflationSwapConventionVersionRequest {
    key: InflationSwapConventionVersionKey;
}

export interface GetInflationSwapConventionVersionResponse {
    result: Result;
    version: InflationSwapConvention | null;
}

export const subjects = {
    list_inflation_swap_conventions_request: 'refdata.v1.inflation_swap_conventions.list',
    get_inflation_swap_convention_request: 'refdata.v1.inflation_swap_conventions.get',
    get_many_inflation_swap_conventions_request: 'refdata.v1.inflation_swap_conventions.get_many',
    put_inflation_swap_convention_request: 'refdata.v1.inflation_swap_conventions.put',
    put_many_inflation_swap_conventions_request: 'refdata.v1.inflation_swap_conventions.put_many',
    delete_inflation_swap_convention_request: 'refdata.v1.inflation_swap_conventions.delete',
    delete_many_inflation_swap_conventions_request:
        'refdata.v1.inflation_swap_conventions.delete_many',
    list_inflation_swap_convention_versions_request:
        'refdata.v1.inflation_swap_conventions_versions.list',
    get_inflation_swap_convention_version_request:
        'refdata.v1.inflation_swap_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_inflation_swap_conventions_request: true,
    get_inflation_swap_convention_request: true,
    get_many_inflation_swap_conventions_request: true,
    put_inflation_swap_convention_request: true,
    put_many_inflation_swap_conventions_request: true,
    delete_inflation_swap_convention_request: true,
    delete_many_inflation_swap_conventions_request: true,
    list_inflation_swap_convention_versions_request: true,
    get_inflation_swap_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.inflation_swap_conventions_events.created',
    updated: 'refdata.v1.inflation_swap_conventions_events.updated',
    deleted: 'refdata.v1.inflation_swap_conventions_events.deleted',
} as const;
