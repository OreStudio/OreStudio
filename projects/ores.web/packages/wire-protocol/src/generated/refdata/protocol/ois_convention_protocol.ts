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
import type { OisConvention } from '../domain/ois_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface OisConventionKey {
    id: string;
}

export interface OisConventionWrite {
    id: string;
    spot_lag: number;
    index: string;
    fixed_day_count_fraction: string;
    fixed_calendar: string | null;
    payment_lag: number | null;
    end_of_month: boolean | null;
    fixed_frequency: string | null;
    fixed_convention: string | null;
    fixed_payment_convention: string | null;
    rule: string | null;
    payment_calendar: string | null;
    rate_cutoff: number | null;
}

export interface OisConventionChange {
    write: OisConventionWrite;
    precondition: Precondition;
}

export interface OisConventionRemoval {
    key: OisConventionKey;
    precondition: Precondition;
}

export interface OisConventionLookup {
    key: OisConventionKey;
    ois_convention: OisConvention | null;
}

export interface OisConventionEvent {
    event_id: string;
    key: OisConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface OisConventionVersionKey {
    ois_convention: OisConventionKey;
    version: number;
}

export interface OisConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListOisConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListOisConventionsResponse {
    result: Result;
    ois_conventions: OisConvention[];
    total: number;
}

export interface GetOisConventionRequest {
    key: OisConventionKey;
}

export interface GetOisConventionResponse {
    result: Result;
    ois_convention: OisConvention | null;
}

export interface GetManyOisConventionsRequest {
    keys: OisConventionKey[];
}

export interface GetManyOisConventionsResponse {
    result: Result;
    entries: OisConventionLookup[];
}

export interface PutOisConventionRequest {
    change: OisConventionChange;
    intent: ChangeIntent;
}

export interface PutOisConventionResponse {
    result: Result;
    ois_convention: OisConvention;
}

export interface PutManyOisConventionsRequest {
    changes: OisConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyOisConventionsResponse {
    result: Result;
    ois_conventions: OisConvention[];
}

export interface DeleteOisConventionRequest {
    removal: OisConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteOisConventionResponse {
    result: Result;
}

export interface DeleteManyOisConventionsRequest {
    removals: OisConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyOisConventionsResponse {
    result: Result;
}

export interface ListOisConventionVersionsRequest {
    key: OisConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: OisConventionVersionsFilter | null;
}

export interface ListOisConventionVersionsResponse {
    result: Result;
    versions: OisConvention[];
    total: number;
}

export interface GetOisConventionVersionRequest {
    key: OisConventionVersionKey;
}

export interface GetOisConventionVersionResponse {
    result: Result;
    version: OisConvention;
}

export const subjects = {
    list_ois_conventions_request: "refdata.v1.ois_conventions.list",
    get_ois_convention_request: "refdata.v1.ois_conventions.get",
    get_many_ois_conventions_request: "refdata.v1.ois_conventions.get_many",
    put_ois_convention_request: "refdata.v1.ois_conventions.put",
    put_many_ois_conventions_request: "refdata.v1.ois_conventions.put_many",
    delete_ois_convention_request: "refdata.v1.ois_conventions.delete",
    delete_many_ois_conventions_request: "refdata.v1.ois_conventions.delete_many",
    list_ois_convention_versions_request: "refdata.v1.ois_conventions_versions.list",
    get_ois_convention_version_request: "refdata.v1.ois_conventions_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ois_conventions_request: true,
    get_ois_convention_request: true,
    get_many_ois_conventions_request: true,
    put_ois_convention_request: true,
    put_many_ois_conventions_request: true,
    delete_ois_convention_request: true,
    delete_many_ois_conventions_request: true,
    list_ois_convention_versions_request: true,
    get_ois_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.ois_conventions_events.created",
    updated: "refdata.v1.ois_conventions_events.updated",
    deleted: "refdata.v1.ois_conventions_events.deleted",
} as const;
