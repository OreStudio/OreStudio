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
import type { ZeroConvention } from '../domain/zero_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ZeroConventionKey {
    id: string;
}

export interface ZeroConventionWrite {
    id: string;
    tenor_based: boolean;
    day_count_fraction: string;
    compounding: string | null;
    compounding_frequency: string | null;
    tenor_calendar: string | null;
    spot_lag: number | null;
    spot_calendar: string | null;
    roll_convention: string | null;
    end_of_month: boolean | null;
}

export interface ZeroConventionChange {
    write: ZeroConventionWrite;
    precondition: Precondition;
}

export interface ZeroConventionRemoval {
    key: ZeroConventionKey;
    precondition: Precondition;
}

export interface ZeroConventionLookup {
    key: ZeroConventionKey;
    zero_convention: ZeroConvention | null;
}

export interface ZeroConventionEvent {
    event_id: string;
    key: ZeroConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ZeroConventionVersionKey {
    zero_convention: ZeroConventionKey;
    version: number;
}

export interface ZeroConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListZeroConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListZeroConventionsResponse {
    result: Result;
    zero_conventions: ZeroConvention[];
    total: number;
}

export interface GetZeroConventionRequest {
    key: ZeroConventionKey;
}

export interface GetZeroConventionResponse {
    result: Result;
    zero_convention: ZeroConvention | null;
}

export interface GetManyZeroConventionsRequest {
    keys: ZeroConventionKey[];
}

export interface GetManyZeroConventionsResponse {
    result: Result;
    entries: ZeroConventionLookup[];
}

export interface PutZeroConventionRequest {
    change: ZeroConventionChange;
    intent: ChangeIntent;
}

export interface PutZeroConventionResponse {
    result: Result;
    zero_convention: ZeroConvention;
}

export interface PutManyZeroConventionsRequest {
    changes: ZeroConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyZeroConventionsResponse {
    result: Result;
    zero_conventions: ZeroConvention[];
}

export interface DeleteZeroConventionRequest {
    removal: ZeroConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteZeroConventionResponse {
    result: Result;
}

export interface DeleteManyZeroConventionsRequest {
    removals: ZeroConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyZeroConventionsResponse {
    result: Result;
}

export interface ListZeroConventionVersionsRequest {
    key: ZeroConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ZeroConventionVersionsFilter | null;
}

export interface ListZeroConventionVersionsResponse {
    result: Result;
    versions: ZeroConvention[];
    total: number;
}

export interface GetZeroConventionVersionRequest {
    key: ZeroConventionVersionKey;
}

export interface GetZeroConventionVersionResponse {
    result: Result;
    version: ZeroConvention;
}

export const subjects = {
    list_zero_conventions_request: "refdata.v1.zero_conventions.list",
    get_zero_convention_request: "refdata.v1.zero_conventions.get",
    get_many_zero_conventions_request: "refdata.v1.zero_conventions.get_many",
    put_zero_convention_request: "refdata.v1.zero_conventions.put",
    put_many_zero_conventions_request: "refdata.v1.zero_conventions.put_many",
    delete_zero_convention_request: "refdata.v1.zero_conventions.delete",
    delete_many_zero_conventions_request: "refdata.v1.zero_conventions.delete_many",
    list_zero_convention_versions_request: "refdata.v1.zero_conventions_versions.list",
    get_zero_convention_version_request: "refdata.v1.zero_conventions_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_zero_conventions_request: true,
    get_zero_convention_request: true,
    get_many_zero_conventions_request: true,
    put_zero_convention_request: true,
    put_many_zero_conventions_request: true,
    delete_zero_convention_request: true,
    delete_many_zero_conventions_request: true,
    list_zero_convention_versions_request: true,
    get_zero_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.zero_conventions_events.created",
    updated: "refdata.v1.zero_conventions_events.updated",
    deleted: "refdata.v1.zero_conventions_events.deleted",
} as const;
