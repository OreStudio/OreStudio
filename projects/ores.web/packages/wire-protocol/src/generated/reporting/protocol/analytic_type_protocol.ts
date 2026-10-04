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
import type { AnalyticType } from '../domain/analytic_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface AnalyticTypeKey {
    code: string;
}

export interface AnalyticTypeWrite {
    code: string;
    name: string;
    description: string;
    parameter_entity: string | null;
    display_order: number;
}

export interface AnalyticTypeChange {
    write: AnalyticTypeWrite;
    precondition: Precondition;
}

export interface AnalyticTypeRemoval {
    key: AnalyticTypeKey;
    precondition: Precondition;
}

export interface AnalyticTypeLookup {
    key: AnalyticTypeKey;
    analytic_type: AnalyticType | null;
}

export interface AnalyticTypesFilter {
    code_one_of: string[] | null;
}

export interface AnalyticTypeEvent {
    event_id: string;
    key: AnalyticTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AnalyticTypeVersionKey {
    analytic_type: AnalyticTypeKey;
    version: number;
}

export interface AnalyticTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAnalyticTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: AnalyticTypesFilter | null;
}

export interface ListAnalyticTypesResponse {
    result: Result;
    types: AnalyticType[];
    total: number;
}

export interface GetAnalyticTypeRequest {
    key: AnalyticTypeKey;
}

export interface GetAnalyticTypeResponse {
    result: Result;
    analytic_type: AnalyticType | null;
}

export interface GetManyAnalyticTypesRequest {
    keys: AnalyticTypeKey[];
}

export interface GetManyAnalyticTypesResponse {
    result: Result;
    entries: AnalyticTypeLookup[];
}

export interface PutAnalyticTypeRequest {
    change: AnalyticTypeChange;
    intent: ChangeIntent;
}

export interface PutAnalyticTypeResponse {
    result: Result;
    analytic_type: AnalyticType | null;
}

export interface PutManyAnalyticTypesRequest {
    changes: AnalyticTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyAnalyticTypesResponse {
    result: Result;
    types: AnalyticType[];
}

export interface DeleteAnalyticTypeRequest {
    removal: AnalyticTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteAnalyticTypeResponse {
    result: Result;
}

export interface DeleteManyAnalyticTypesRequest {
    removals: AnalyticTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAnalyticTypesResponse {
    result: Result;
}

export interface ListAnalyticTypeVersionsRequest {
    key: AnalyticTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AnalyticTypeVersionsFilter | null;
}

export interface ListAnalyticTypeVersionsResponse {
    result: Result;
    versions: AnalyticType[];
    total: number;
}

export interface GetAnalyticTypeVersionRequest {
    key: AnalyticTypeVersionKey;
}

export interface GetAnalyticTypeVersionResponse {
    result: Result;
    version: AnalyticType | null;
}

export const subjects = {
    list_analytic_types_request: 'reporting.v1.analytic_types.list',
    get_analytic_type_request: 'reporting.v1.analytic_types.get',
    get_many_analytic_types_request: 'reporting.v1.analytic_types.get_many',
    put_analytic_type_request: 'reporting.v1.analytic_types.put',
    put_many_analytic_types_request: 'reporting.v1.analytic_types.put_many',
    delete_analytic_type_request: 'reporting.v1.analytic_types.delete',
    delete_many_analytic_types_request: 'reporting.v1.analytic_types.delete_many',
    list_analytic_type_versions_request: 'reporting.v1.analytic_types_versions.list',
    get_analytic_type_version_request: 'reporting.v1.analytic_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_analytic_types_request: true,
    get_analytic_type_request: true,
    get_many_analytic_types_request: true,
    put_analytic_type_request: true,
    put_many_analytic_types_request: true,
    delete_analytic_type_request: true,
    delete_many_analytic_types_request: true,
    list_analytic_type_versions_request: true,
    get_analytic_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.analytic_types_events.created',
    updated: 'reporting.v1.analytic_types_events.updated',
    deleted: 'reporting.v1.analytic_types_events.deleted',
} as const;
