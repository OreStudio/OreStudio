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
import type { IntradayPowerLoadConvention } from '../domain/intraday_power_load_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface IntradayPowerLoadConventionKey {
    id: string;
}

export interface IntradayPowerLoadConventionWrite {
    id: string;
    explicit_load_profile: string | null;
    business_day_load_rules: string | null;
}

export interface IntradayPowerLoadConventionChange {
    write: IntradayPowerLoadConventionWrite;
    precondition: Precondition;
}

export interface IntradayPowerLoadConventionRemoval {
    key: IntradayPowerLoadConventionKey;
    precondition: Precondition;
}

export interface IntradayPowerLoadConventionLookup {
    key: IntradayPowerLoadConventionKey;
    intraday_power_load_convention: IntradayPowerLoadConvention | null;
}

export interface IntradayPowerLoadConventionsFilter {
    id_one_of: string[] | null;
}

export interface IntradayPowerLoadConventionEvent {
    event_id: string;
    key: IntradayPowerLoadConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface IntradayPowerLoadConventionVersionKey {
    intraday_power_load_convention: IntradayPowerLoadConventionKey;
    version: number;
}

export interface IntradayPowerLoadConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListIntradayPowerLoadConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: IntradayPowerLoadConventionsFilter | null;
    as_of: string | null;
}

export interface ListIntradayPowerLoadConventionsResponse {
    result: Result;
    intraday_power_load_conventions: IntradayPowerLoadConvention[];
    total: number;
}

export interface GetIntradayPowerLoadConventionRequest {
    key: IntradayPowerLoadConventionKey;
}

export interface GetIntradayPowerLoadConventionResponse {
    result: Result;
    intraday_power_load_convention: IntradayPowerLoadConvention | null;
}

export interface GetManyIntradayPowerLoadConventionsRequest {
    keys: IntradayPowerLoadConventionKey[];
}

export interface GetManyIntradayPowerLoadConventionsResponse {
    result: Result;
    entries: IntradayPowerLoadConventionLookup[];
}

export interface PutIntradayPowerLoadConventionRequest {
    change: IntradayPowerLoadConventionChange;
    intent: ChangeIntent;
}

export interface PutIntradayPowerLoadConventionResponse {
    result: Result;
    intraday_power_load_convention: IntradayPowerLoadConvention | null;
}

export interface PutManyIntradayPowerLoadConventionsRequest {
    changes: IntradayPowerLoadConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyIntradayPowerLoadConventionsResponse {
    result: Result;
    intraday_power_load_conventions: IntradayPowerLoadConvention[];
}

export interface DeleteIntradayPowerLoadConventionRequest {
    removal: IntradayPowerLoadConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteIntradayPowerLoadConventionResponse {
    result: Result;
}

export interface DeleteManyIntradayPowerLoadConventionsRequest {
    removals: IntradayPowerLoadConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyIntradayPowerLoadConventionsResponse {
    result: Result;
}

export interface ListIntradayPowerLoadConventionVersionsRequest {
    key: IntradayPowerLoadConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: IntradayPowerLoadConventionVersionsFilter | null;
}

export interface ListIntradayPowerLoadConventionVersionsResponse {
    result: Result;
    versions: IntradayPowerLoadConvention[];
    total: number;
}

export interface GetIntradayPowerLoadConventionVersionRequest {
    key: IntradayPowerLoadConventionVersionKey;
}

export interface GetIntradayPowerLoadConventionVersionResponse {
    result: Result;
    version: IntradayPowerLoadConvention | null;
}

export const subjects = {
    list_intraday_power_load_conventions_request: 'refdata.v1.intraday_power_load_conventions.list',
    get_intraday_power_load_convention_request: 'refdata.v1.intraday_power_load_conventions.get',
    get_many_intraday_power_load_conventions_request:
        'refdata.v1.intraday_power_load_conventions.get_many',
    put_intraday_power_load_convention_request: 'refdata.v1.intraday_power_load_conventions.put',
    put_many_intraday_power_load_conventions_request:
        'refdata.v1.intraday_power_load_conventions.put_many',
    delete_intraday_power_load_convention_request:
        'refdata.v1.intraday_power_load_conventions.delete',
    delete_many_intraday_power_load_conventions_request:
        'refdata.v1.intraday_power_load_conventions.delete_many',
    list_intraday_power_load_convention_versions_request:
        'refdata.v1.intraday_power_load_conventions_versions.list',
    get_intraday_power_load_convention_version_request:
        'refdata.v1.intraday_power_load_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_intraday_power_load_conventions_request: true,
    get_intraday_power_load_convention_request: true,
    get_many_intraday_power_load_conventions_request: true,
    put_intraday_power_load_convention_request: true,
    put_many_intraday_power_load_conventions_request: true,
    delete_intraday_power_load_convention_request: true,
    delete_many_intraday_power_load_conventions_request: true,
    list_intraday_power_load_convention_versions_request: true,
    get_intraday_power_load_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.intraday_power_load_conventions_events.created',
    updated: 'refdata.v1.intraday_power_load_conventions_events.updated',
    deleted: 'refdata.v1.intraday_power_load_conventions_events.deleted',
} as const;
