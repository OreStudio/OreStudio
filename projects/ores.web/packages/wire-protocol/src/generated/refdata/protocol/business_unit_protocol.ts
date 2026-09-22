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
import type { BusinessUnit } from '../domain/business_unit.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BusinessUnitKey {
    id: string;
}

export interface BusinessUnitWrite {
    id: string;
    unit_name: string;
    parent_business_unit_id: string | null;
    unit_code: string;
    business_centre_code: string;
    unit_type_id: string | null;
    status: string;
}

export interface BusinessUnitChange {
    write: BusinessUnitWrite;
    precondition: Precondition;
}

export interface BusinessUnitRemoval {
    key: BusinessUnitKey;
    precondition: Precondition;
}

export interface BusinessUnitLookup {
    key: BusinessUnitKey;
    business_unit: BusinessUnit | null;
}

export interface BusinessUnitEvent {
    event_id: string;
    key: BusinessUnitKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BusinessUnitVersionKey {
    business_unit: BusinessUnitKey;
    version: number;
}

export interface BusinessUnitVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBusinessUnitsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBusinessUnitsResponse {
    result: Result;
    business_units: BusinessUnit[];
    total: number;
}

export interface GetBusinessUnitRequest {
    key: BusinessUnitKey;
}

export interface GetBusinessUnitResponse {
    result: Result;
    business_unit: BusinessUnit | null;
}

export interface GetManyBusinessUnitsRequest {
    keys: BusinessUnitKey[];
}

export interface GetManyBusinessUnitsResponse {
    result: Result;
    entries: BusinessUnitLookup[];
}

export interface PutBusinessUnitRequest {
    change: BusinessUnitChange;
    intent: ChangeIntent;
}

export interface PutBusinessUnitResponse {
    result: Result;
    business_unit: BusinessUnit;
}

export interface PutManyBusinessUnitsRequest {
    changes: BusinessUnitChange[];
    intent: ChangeIntent;
}

export interface PutManyBusinessUnitsResponse {
    result: Result;
    business_units: BusinessUnit[];
}

export interface DeleteBusinessUnitRequest {
    removal: BusinessUnitRemoval;
    intent: ChangeIntent;
}

export interface DeleteBusinessUnitResponse {
    result: Result;
}

export interface DeleteManyBusinessUnitsRequest {
    removals: BusinessUnitRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBusinessUnitsResponse {
    result: Result;
}

export interface ListBusinessUnitVersionsRequest {
    key: BusinessUnitKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BusinessUnitVersionsFilter | null;
}

export interface ListBusinessUnitVersionsResponse {
    result: Result;
    versions: BusinessUnit[];
    total: number;
}

export interface GetBusinessUnitVersionRequest {
    key: BusinessUnitVersionKey;
}

export interface GetBusinessUnitVersionResponse {
    result: Result;
    version: BusinessUnit;
}

export const subjects = {
    list_business_units_request: "refdata.v1.business_units.list",
    get_business_unit_request: "refdata.v1.business_units.get",
    get_many_business_units_request: "refdata.v1.business_units.get_many",
    put_business_unit_request: "refdata.v1.business_units.put",
    put_many_business_units_request: "refdata.v1.business_units.put_many",
    delete_business_unit_request: "refdata.v1.business_units.delete",
    delete_many_business_units_request: "refdata.v1.business_units.delete_many",
    list_business_unit_versions_request: "refdata.v1.business_units_versions.list",
    get_business_unit_version_request: "refdata.v1.business_units_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_business_units_request: true,
    get_business_unit_request: true,
    get_many_business_units_request: true,
    put_business_unit_request: true,
    put_many_business_units_request: true,
    delete_business_unit_request: true,
    delete_many_business_units_request: true,
    list_business_unit_versions_request: true,
    get_business_unit_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.business_units_events.created",
    updated: "refdata.v1.business_units_events.updated",
    deleted: "refdata.v1.business_units_events.deleted",
} as const;
