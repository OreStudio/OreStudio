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
import type { OvernightIndexConvention } from '../domain/overnight_index_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface OvernightIndexConventionKey {
    id: string;
}

export interface OvernightIndexConventionWrite {
    id: string;
    fixing_calendar: string;
    day_count_fraction: string;
    settlement_days: number;
}

export interface OvernightIndexConventionChange {
    write: OvernightIndexConventionWrite;
    precondition: Precondition;
}

export interface OvernightIndexConventionRemoval {
    key: OvernightIndexConventionKey;
    precondition: Precondition;
}

export interface OvernightIndexConventionLookup {
    key: OvernightIndexConventionKey;
    overnight_index_convention: OvernightIndexConvention | null;
}

export interface OvernightIndexConventionEvent {
    event_id: string;
    key: OvernightIndexConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface OvernightIndexConventionVersionKey {
    overnight_index_convention: OvernightIndexConventionKey;
    version: number;
}

export interface OvernightIndexConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListOvernightIndexConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListOvernightIndexConventionsResponse {
    result: Result;
    overnight_index_conventions: OvernightIndexConvention[];
    total: number;
}

export interface GetOvernightIndexConventionRequest {
    key: OvernightIndexConventionKey;
}

export interface GetOvernightIndexConventionResponse {
    result: Result;
    overnight_index_convention: OvernightIndexConvention | null;
}

export interface GetManyOvernightIndexConventionsRequest {
    keys: OvernightIndexConventionKey[];
}

export interface GetManyOvernightIndexConventionsResponse {
    result: Result;
    entries: OvernightIndexConventionLookup[];
}

export interface PutOvernightIndexConventionRequest {
    change: OvernightIndexConventionChange;
    intent: ChangeIntent;
}

export interface PutOvernightIndexConventionResponse {
    result: Result;
    overnight_index_convention: OvernightIndexConvention;
}

export interface PutManyOvernightIndexConventionsRequest {
    changes: OvernightIndexConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyOvernightIndexConventionsResponse {
    result: Result;
    overnight_index_conventions: OvernightIndexConvention[];
}

export interface DeleteOvernightIndexConventionRequest {
    removal: OvernightIndexConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteOvernightIndexConventionResponse {
    result: Result;
}

export interface DeleteManyOvernightIndexConventionsRequest {
    removals: OvernightIndexConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyOvernightIndexConventionsResponse {
    result: Result;
}

export interface ListOvernightIndexConventionVersionsRequest {
    key: OvernightIndexConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: OvernightIndexConventionVersionsFilter | null;
}

export interface ListOvernightIndexConventionVersionsResponse {
    result: Result;
    versions: OvernightIndexConvention[];
    total: number;
}

export interface GetOvernightIndexConventionVersionRequest {
    key: OvernightIndexConventionVersionKey;
}

export interface GetOvernightIndexConventionVersionResponse {
    result: Result;
    version: OvernightIndexConvention;
}

export const subjects = {
    list_overnight_index_conventions_request: "refdata.v1.overnight_index_conventions.list",
    get_overnight_index_convention_request: "refdata.v1.overnight_index_conventions.get",
    get_many_overnight_index_conventions_request: "refdata.v1.overnight_index_conventions.get_many",
    put_overnight_index_convention_request: "refdata.v1.overnight_index_conventions.put",
    put_many_overnight_index_conventions_request: "refdata.v1.overnight_index_conventions.put_many",
    delete_overnight_index_convention_request: "refdata.v1.overnight_index_conventions.delete",
    delete_many_overnight_index_conventions_request: "refdata.v1.overnight_index_conventions.delete_many",
    list_overnight_index_convention_versions_request: "refdata.v1.overnight_index_conventions_versions.list",
    get_overnight_index_convention_version_request: "refdata.v1.overnight_index_conventions_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_overnight_index_conventions_request: true,
    get_overnight_index_convention_request: true,
    get_many_overnight_index_conventions_request: true,
    put_overnight_index_convention_request: true,
    put_many_overnight_index_conventions_request: true,
    delete_overnight_index_convention_request: true,
    delete_many_overnight_index_conventions_request: true,
    list_overnight_index_convention_versions_request: true,
    get_overnight_index_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.overnight_index_conventions_events.created",
    updated: "refdata.v1.overnight_index_conventions_events.updated",
    deleted: "refdata.v1.overnight_index_conventions_events.deleted",
} as const;
