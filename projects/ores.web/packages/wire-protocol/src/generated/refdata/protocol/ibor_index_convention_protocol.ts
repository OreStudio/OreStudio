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
import type { IborIndexConvention } from '../domain/ibor_index_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface IborIndexConventionKey {
    id: string;
}

export interface IborIndexConventionWrite {
    id: string;
    fixing_calendar: string;
    day_count_fraction: string;
    settlement_days: number;
    business_day_convention: string;
    end_of_month: boolean;
}

export interface IborIndexConventionChange {
    write: IborIndexConventionWrite;
    precondition: Precondition;
}

export interface IborIndexConventionRemoval {
    key: IborIndexConventionKey;
    precondition: Precondition;
}

export interface IborIndexConventionLookup {
    key: IborIndexConventionKey;
    ibor_index_convention: IborIndexConvention | null;
}

export interface IborIndexConventionEvent {
    event_id: string;
    key: IborIndexConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface IborIndexConventionVersionKey {
    ibor_index_convention: IborIndexConventionKey;
    version: number;
}

export interface IborIndexConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListIborIndexConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListIborIndexConventionsResponse {
    result: Result;
    ibor_index_conventions: IborIndexConvention[];
    total: number;
}

export interface GetIborIndexConventionRequest {
    key: IborIndexConventionKey;
}

export interface GetIborIndexConventionResponse {
    result: Result;
    ibor_index_convention: IborIndexConvention | null;
}

export interface GetManyIborIndexConventionsRequest {
    keys: IborIndexConventionKey[];
}

export interface GetManyIborIndexConventionsResponse {
    result: Result;
    entries: IborIndexConventionLookup[];
}

export interface PutIborIndexConventionRequest {
    change: IborIndexConventionChange;
    intent: ChangeIntent;
}

export interface PutIborIndexConventionResponse {
    result: Result;
    ibor_index_convention: IborIndexConvention;
}

export interface PutManyIborIndexConventionsRequest {
    changes: IborIndexConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyIborIndexConventionsResponse {
    result: Result;
    ibor_index_conventions: IborIndexConvention[];
}

export interface DeleteIborIndexConventionRequest {
    removal: IborIndexConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteIborIndexConventionResponse {
    result: Result;
}

export interface DeleteManyIborIndexConventionsRequest {
    removals: IborIndexConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyIborIndexConventionsResponse {
    result: Result;
}

export interface ListIborIndexConventionVersionsRequest {
    key: IborIndexConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: IborIndexConventionVersionsFilter | null;
}

export interface ListIborIndexConventionVersionsResponse {
    result: Result;
    versions: IborIndexConvention[];
    total: number;
}

export interface GetIborIndexConventionVersionRequest {
    key: IborIndexConventionVersionKey;
}

export interface GetIborIndexConventionVersionResponse {
    result: Result;
    version: IborIndexConvention;
}

export const subjects = {
    list_ibor_index_conventions_request: "refdata.v1.ibor_index_conventions.list",
    get_ibor_index_convention_request: "refdata.v1.ibor_index_conventions.get",
    get_many_ibor_index_conventions_request: "refdata.v1.ibor_index_conventions.get_many",
    put_ibor_index_convention_request: "refdata.v1.ibor_index_conventions.put",
    put_many_ibor_index_conventions_request: "refdata.v1.ibor_index_conventions.put_many",
    delete_ibor_index_convention_request: "refdata.v1.ibor_index_conventions.delete",
    delete_many_ibor_index_conventions_request: "refdata.v1.ibor_index_conventions.delete_many",
    list_ibor_index_convention_versions_request: "refdata.v1.ibor_index_conventions_versions.list",
    get_ibor_index_convention_version_request: "refdata.v1.ibor_index_conventions_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ibor_index_conventions_request: true,
    get_ibor_index_convention_request: true,
    get_many_ibor_index_conventions_request: true,
    put_ibor_index_convention_request: true,
    put_many_ibor_index_conventions_request: true,
    delete_ibor_index_convention_request: true,
    delete_many_ibor_index_conventions_request: true,
    list_ibor_index_convention_versions_request: true,
    get_ibor_index_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.ibor_index_conventions_events.created",
    updated: "refdata.v1.ibor_index_conventions_events.updated",
    deleted: "refdata.v1.ibor_index_conventions_events.deleted",
} as const;
