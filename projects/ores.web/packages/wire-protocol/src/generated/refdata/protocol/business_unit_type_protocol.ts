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
import type { BusinessUnitType } from '../domain/business_unit_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BusinessUnitTypeKey {
    code: string;
}

export interface BusinessUnitTypeWrite {
    id: string;
    coding_scheme_code: string;
    code: string;
    name: string;
    level: number;
    description: string;
}

export interface BusinessUnitTypeChange {
    write: BusinessUnitTypeWrite;
    precondition: Precondition;
}

export interface BusinessUnitTypeRemoval {
    key: BusinessUnitTypeKey;
    precondition: Precondition;
}

export interface BusinessUnitTypeLookup {
    key: BusinessUnitTypeKey;
    business_unit_type: BusinessUnitType | null;
}

export interface BusinessUnitTypeEvent {
    event_id: string;
    key: BusinessUnitTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BusinessUnitTypeVersionKey {
    business_unit_type: BusinessUnitTypeKey;
    version: number;
}

export interface BusinessUnitTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBusinessUnitTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBusinessUnitTypesResponse {
    result: Result;
    types: BusinessUnitType[];
    total: number;
}

export interface GetBusinessUnitTypeRequest {
    key: BusinessUnitTypeKey;
}

export interface GetBusinessUnitTypeResponse {
    result: Result;
    business_unit_type: BusinessUnitType | null;
}

export interface GetManyBusinessUnitTypesRequest {
    keys: BusinessUnitTypeKey[];
}

export interface GetManyBusinessUnitTypesResponse {
    result: Result;
    entries: BusinessUnitTypeLookup[];
}

export interface PutBusinessUnitTypeRequest {
    change: BusinessUnitTypeChange;
    intent: ChangeIntent;
}

export interface PutBusinessUnitTypeResponse {
    result: Result;
    business_unit_type: BusinessUnitType;
}

export interface PutManyBusinessUnitTypesRequest {
    changes: BusinessUnitTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyBusinessUnitTypesResponse {
    result: Result;
    types: BusinessUnitType[];
}

export interface DeleteBusinessUnitTypeRequest {
    removal: BusinessUnitTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteBusinessUnitTypeResponse {
    result: Result;
}

export interface DeleteManyBusinessUnitTypesRequest {
    removals: BusinessUnitTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBusinessUnitTypesResponse {
    result: Result;
}

export interface ListBusinessUnitTypeVersionsRequest {
    key: BusinessUnitTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BusinessUnitTypeVersionsFilter | null;
}

export interface ListBusinessUnitTypeVersionsResponse {
    result: Result;
    versions: BusinessUnitType[];
    total: number;
}

export interface GetBusinessUnitTypeVersionRequest {
    key: BusinessUnitTypeVersionKey;
}

export interface GetBusinessUnitTypeVersionResponse {
    result: Result;
    version: BusinessUnitType;
}

export const subjects = {
    list_business_unit_types_request: "refdata.v1.business_unit_types.list",
    get_business_unit_type_request: "refdata.v1.business_unit_types.get",
    get_many_business_unit_types_request: "refdata.v1.business_unit_types.get_many",
    put_business_unit_type_request: "refdata.v1.business_unit_types.put",
    put_many_business_unit_types_request: "refdata.v1.business_unit_types.put_many",
    delete_business_unit_type_request: "refdata.v1.business_unit_types.delete",
    delete_many_business_unit_types_request: "refdata.v1.business_unit_types.delete_many",
    list_business_unit_type_versions_request: "refdata.v1.business_unit_types_versions.list",
    get_business_unit_type_version_request: "refdata.v1.business_unit_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_business_unit_types_request: true,
    get_business_unit_type_request: true,
    get_many_business_unit_types_request: true,
    put_business_unit_type_request: true,
    put_many_business_unit_types_request: true,
    delete_business_unit_type_request: true,
    delete_many_business_unit_types_request: true,
    list_business_unit_type_versions_request: true,
    get_business_unit_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.business_unit_types_events.created",
    updated: "refdata.v1.business_unit_types_events.updated",
    deleted: "refdata.v1.business_unit_types_events.deleted",
} as const;
