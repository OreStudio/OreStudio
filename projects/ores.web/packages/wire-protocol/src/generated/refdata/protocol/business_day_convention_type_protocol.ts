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
import type { BusinessDayConventionType } from '../domain/business_day_convention_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BusinessDayConventionTypeKey {
    code: string;
}

export interface BusinessDayConventionTypeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface BusinessDayConventionTypeChange {
    write: BusinessDayConventionTypeWrite;
    precondition: Precondition;
}

export interface BusinessDayConventionTypeRemoval {
    key: BusinessDayConventionTypeKey;
    precondition: Precondition;
}

export interface BusinessDayConventionTypeLookup {
    key: BusinessDayConventionTypeKey;
    business_day_convention_type: BusinessDayConventionType | null;
}

export interface BusinessDayConventionTypeEvent {
    event_id: string;
    key: BusinessDayConventionTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BusinessDayConventionTypeVersionKey {
    business_day_convention_type: BusinessDayConventionTypeKey;
    version: number;
}

export interface BusinessDayConventionTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBusinessDayConventionTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBusinessDayConventionTypesResponse {
    result: Result;
    types: BusinessDayConventionType[];
    total: number;
}

export interface GetBusinessDayConventionTypeRequest {
    key: BusinessDayConventionTypeKey;
}

export interface GetBusinessDayConventionTypeResponse {
    result: Result;
    business_day_convention_type: BusinessDayConventionType | null;
}

export interface GetManyBusinessDayConventionTypesRequest {
    keys: BusinessDayConventionTypeKey[];
}

export interface GetManyBusinessDayConventionTypesResponse {
    result: Result;
    entries: BusinessDayConventionTypeLookup[];
}

export interface PutBusinessDayConventionTypeRequest {
    change: BusinessDayConventionTypeChange;
    intent: ChangeIntent;
}

export interface PutBusinessDayConventionTypeResponse {
    result: Result;
    business_day_convention_type: BusinessDayConventionType;
}

export interface PutManyBusinessDayConventionTypesRequest {
    changes: BusinessDayConventionTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyBusinessDayConventionTypesResponse {
    result: Result;
    types: BusinessDayConventionType[];
}

export interface DeleteBusinessDayConventionTypeRequest {
    removal: BusinessDayConventionTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteBusinessDayConventionTypeResponse {
    result: Result;
}

export interface DeleteManyBusinessDayConventionTypesRequest {
    removals: BusinessDayConventionTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBusinessDayConventionTypesResponse {
    result: Result;
}

export interface ListBusinessDayConventionTypeVersionsRequest {
    key: BusinessDayConventionTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BusinessDayConventionTypeVersionsFilter | null;
}

export interface ListBusinessDayConventionTypeVersionsResponse {
    result: Result;
    versions: BusinessDayConventionType[];
    total: number;
}

export interface GetBusinessDayConventionTypeVersionRequest {
    key: BusinessDayConventionTypeVersionKey;
}

export interface GetBusinessDayConventionTypeVersionResponse {
    result: Result;
    version: BusinessDayConventionType;
}

export const subjects = {
    list_business_day_convention_types_request: "refdata.v1.business_day_convention_types.list",
    get_business_day_convention_type_request: "refdata.v1.business_day_convention_types.get",
    get_many_business_day_convention_types_request: "refdata.v1.business_day_convention_types.get_many",
    put_business_day_convention_type_request: "refdata.v1.business_day_convention_types.put",
    put_many_business_day_convention_types_request: "refdata.v1.business_day_convention_types.put_many",
    delete_business_day_convention_type_request: "refdata.v1.business_day_convention_types.delete",
    delete_many_business_day_convention_types_request: "refdata.v1.business_day_convention_types.delete_many",
    list_business_day_convention_type_versions_request: "refdata.v1.business_day_convention_types_versions.list",
    get_business_day_convention_type_version_request: "refdata.v1.business_day_convention_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_business_day_convention_types_request: true,
    get_business_day_convention_type_request: true,
    get_many_business_day_convention_types_request: true,
    put_business_day_convention_type_request: true,
    put_many_business_day_convention_types_request: true,
    delete_business_day_convention_type_request: true,
    delete_many_business_day_convention_types_request: true,
    list_business_day_convention_type_versions_request: true,
    get_business_day_convention_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.business_day_convention_types_events.created",
    updated: "refdata.v1.business_day_convention_types_events.updated",
    deleted: "refdata.v1.business_day_convention_types_events.deleted",
} as const;
