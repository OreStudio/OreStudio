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
import type { AverageOisConvention } from '../domain/average_ois_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface AverageOisConventionKey {
    id: string;
}

export interface AverageOisConventionWrite {
    id: string;
    spot_lag: number;
    fixed_tenor: string;
    fixed_day_count_fraction: string;
    fixed_calendar: string | null;
    fixed_convention: string | null;
    fixed_payment_convention: string | null;
    fixed_frequency: string | null;
    index: string;
    on_tenor: string;
    rate_cutoff: string;
}

export interface AverageOisConventionChange {
    write: AverageOisConventionWrite;
    precondition: Precondition;
}

export interface AverageOisConventionRemoval {
    key: AverageOisConventionKey;
    precondition: Precondition;
}

export interface AverageOisConventionLookup {
    key: AverageOisConventionKey;
    average_ois_convention: AverageOisConvention | null;
}

export interface AverageOisConventionsFilter {
    id_one_of: string[] | null;
}

export interface AverageOisConventionEvent {
    event_id: string;
    key: AverageOisConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AverageOisConventionVersionKey {
    average_ois_convention: AverageOisConventionKey;
    version: number;
}

export interface AverageOisConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAverageOisConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: AverageOisConventionsFilter | null;
    as_of: string | null;
}

export interface ListAverageOisConventionsResponse {
    result: Result;
    average_ois_conventions: AverageOisConvention[];
    total: number;
}

export interface GetAverageOisConventionRequest {
    key: AverageOisConventionKey;
}

export interface GetAverageOisConventionResponse {
    result: Result;
    average_ois_convention: AverageOisConvention | null;
}

export interface GetManyAverageOisConventionsRequest {
    keys: AverageOisConventionKey[];
}

export interface GetManyAverageOisConventionsResponse {
    result: Result;
    entries: AverageOisConventionLookup[];
}

export interface PutAverageOisConventionRequest {
    change: AverageOisConventionChange;
    intent: ChangeIntent;
}

export interface PutAverageOisConventionResponse {
    result: Result;
    average_ois_convention: AverageOisConvention | null;
}

export interface PutManyAverageOisConventionsRequest {
    changes: AverageOisConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyAverageOisConventionsResponse {
    result: Result;
    average_ois_conventions: AverageOisConvention[];
}

export interface DeleteAverageOisConventionRequest {
    removal: AverageOisConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteAverageOisConventionResponse {
    result: Result;
}

export interface DeleteManyAverageOisConventionsRequest {
    removals: AverageOisConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAverageOisConventionsResponse {
    result: Result;
}

export interface ListAverageOisConventionVersionsRequest {
    key: AverageOisConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AverageOisConventionVersionsFilter | null;
}

export interface ListAverageOisConventionVersionsResponse {
    result: Result;
    versions: AverageOisConvention[];
    total: number;
}

export interface GetAverageOisConventionVersionRequest {
    key: AverageOisConventionVersionKey;
}

export interface GetAverageOisConventionVersionResponse {
    result: Result;
    version: AverageOisConvention | null;
}

export const subjects = {
    list_average_ois_conventions_request: 'refdata.v1.average_ois_conventions.list',
    get_average_ois_convention_request: 'refdata.v1.average_ois_conventions.get',
    get_many_average_ois_conventions_request: 'refdata.v1.average_ois_conventions.get_many',
    put_average_ois_convention_request: 'refdata.v1.average_ois_conventions.put',
    put_many_average_ois_conventions_request: 'refdata.v1.average_ois_conventions.put_many',
    delete_average_ois_convention_request: 'refdata.v1.average_ois_conventions.delete',
    delete_many_average_ois_conventions_request: 'refdata.v1.average_ois_conventions.delete_many',
    list_average_ois_convention_versions_request:
        'refdata.v1.average_ois_conventions_versions.list',
    get_average_ois_convention_version_request: 'refdata.v1.average_ois_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_average_ois_conventions_request: true,
    get_average_ois_convention_request: true,
    get_many_average_ois_conventions_request: true,
    put_average_ois_convention_request: true,
    put_many_average_ois_conventions_request: true,
    delete_average_ois_convention_request: true,
    delete_many_average_ois_conventions_request: true,
    list_average_ois_convention_versions_request: true,
    get_average_ois_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.average_ois_conventions_events.created',
    updated: 'refdata.v1.average_ois_conventions_events.updated',
    deleted: 'refdata.v1.average_ois_conventions_events.deleted',
} as const;
