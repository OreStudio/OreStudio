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
import type { SwapConvention } from '../domain/swap_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SwapConventionKey {
    id: string;
}

export interface SwapConventionWrite {
    id: string;
    fixed_calendar: string | null;
    fixed_frequency: string;
    fixed_convention: string | null;
    fixed_day_count_fraction: string;
    index: string;
    float_frequency: string | null;
    sub_periods_coupon_type: string | null;
}

export interface SwapConventionChange {
    write: SwapConventionWrite;
    precondition: Precondition;
}

export interface SwapConventionRemoval {
    key: SwapConventionKey;
    precondition: Precondition;
}

export interface SwapConventionLookup {
    key: SwapConventionKey;
    swap_convention: SwapConvention | null;
}

export interface SwapConventionEvent {
    event_id: string;
    key: SwapConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SwapConventionVersionKey {
    swap_convention: SwapConventionKey;
    version: number;
}

export interface SwapConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSwapConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListSwapConventionsResponse {
    result: Result;
    swap_conventions: SwapConvention[];
    total: number;
}

export interface GetSwapConventionRequest {
    key: SwapConventionKey;
}

export interface GetSwapConventionResponse {
    result: Result;
    swap_convention: SwapConvention | null;
}

export interface GetManySwapConventionsRequest {
    keys: SwapConventionKey[];
}

export interface GetManySwapConventionsResponse {
    result: Result;
    entries: SwapConventionLookup[];
}

export interface PutSwapConventionRequest {
    change: SwapConventionChange;
    intent: ChangeIntent;
}

export interface PutSwapConventionResponse {
    result: Result;
    swap_convention: SwapConvention;
}

export interface PutManySwapConventionsRequest {
    changes: SwapConventionChange[];
    intent: ChangeIntent;
}

export interface PutManySwapConventionsResponse {
    result: Result;
    swap_conventions: SwapConvention[];
}

export interface DeleteSwapConventionRequest {
    removal: SwapConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteSwapConventionResponse {
    result: Result;
}

export interface DeleteManySwapConventionsRequest {
    removals: SwapConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySwapConventionsResponse {
    result: Result;
}

export interface ListSwapConventionVersionsRequest {
    key: SwapConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SwapConventionVersionsFilter | null;
}

export interface ListSwapConventionVersionsResponse {
    result: Result;
    versions: SwapConvention[];
    total: number;
}

export interface GetSwapConventionVersionRequest {
    key: SwapConventionVersionKey;
}

export interface GetSwapConventionVersionResponse {
    result: Result;
    version: SwapConvention;
}

export const subjects = {
    list_swap_conventions_request: "refdata.v1.swap_conventions.list",
    get_swap_convention_request: "refdata.v1.swap_conventions.get",
    get_many_swap_conventions_request: "refdata.v1.swap_conventions.get_many",
    put_swap_convention_request: "refdata.v1.swap_conventions.put",
    put_many_swap_conventions_request: "refdata.v1.swap_conventions.put_many",
    delete_swap_convention_request: "refdata.v1.swap_conventions.delete",
    delete_many_swap_conventions_request: "refdata.v1.swap_conventions.delete_many",
    list_swap_convention_versions_request: "refdata.v1.swap_conventions_versions.list",
    get_swap_convention_version_request: "refdata.v1.swap_conventions_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_swap_conventions_request: true,
    get_swap_convention_request: true,
    get_many_swap_conventions_request: true,
    put_swap_convention_request: true,
    put_many_swap_conventions_request: true,
    delete_swap_convention_request: true,
    delete_many_swap_conventions_request: true,
    list_swap_convention_versions_request: true,
    get_swap_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.swap_conventions_events.created",
    updated: "refdata.v1.swap_conventions_events.updated",
    deleted: "refdata.v1.swap_conventions_events.deleted",
} as const;
