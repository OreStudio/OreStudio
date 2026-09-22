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
import type { TenorUnit } from '../domain/tenor_unit.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TenorUnitKey {
    code: string;
}

export interface TenorUnitWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface TenorUnitChange {
    write: TenorUnitWrite;
    precondition: Precondition;
}

export interface TenorUnitRemoval {
    key: TenorUnitKey;
    precondition: Precondition;
}

export interface TenorUnitLookup {
    key: TenorUnitKey;
    tenor_unit: TenorUnit | null;
}

export interface TenorUnitEvent {
    event_id: string;
    key: TenorUnitKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TenorUnitVersionKey {
    tenor_unit: TenorUnitKey;
    version: number;
}

export interface TenorUnitVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTenorUnitsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTenorUnitsResponse {
    result: Result;
    units: TenorUnit[];
    total: number;
}

export interface GetTenorUnitRequest {
    key: TenorUnitKey;
}

export interface GetTenorUnitResponse {
    result: Result;
    tenor_unit: TenorUnit | null;
}

export interface GetManyTenorUnitsRequest {
    keys: TenorUnitKey[];
}

export interface GetManyTenorUnitsResponse {
    result: Result;
    entries: TenorUnitLookup[];
}

export interface PutTenorUnitRequest {
    change: TenorUnitChange;
    intent: ChangeIntent;
}

export interface PutTenorUnitResponse {
    result: Result;
    tenor_unit: TenorUnit;
}

export interface PutManyTenorUnitsRequest {
    changes: TenorUnitChange[];
    intent: ChangeIntent;
}

export interface PutManyTenorUnitsResponse {
    result: Result;
    units: TenorUnit[];
}

export interface DeleteTenorUnitRequest {
    removal: TenorUnitRemoval;
    intent: ChangeIntent;
}

export interface DeleteTenorUnitResponse {
    result: Result;
}

export interface DeleteManyTenorUnitsRequest {
    removals: TenorUnitRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTenorUnitsResponse {
    result: Result;
}

export interface ListTenorUnitVersionsRequest {
    key: TenorUnitKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorUnitVersionsFilter | null;
}

export interface ListTenorUnitVersionsResponse {
    result: Result;
    versions: TenorUnit[];
    total: number;
}

export interface GetTenorUnitVersionRequest {
    key: TenorUnitVersionKey;
}

export interface GetTenorUnitVersionResponse {
    result: Result;
    version: TenorUnit;
}

export const subjects = {
    list_tenor_units_request: "refdata.v1.tenor_units.list",
    get_tenor_unit_request: "refdata.v1.tenor_units.get",
    get_many_tenor_units_request: "refdata.v1.tenor_units.get_many",
    put_tenor_unit_request: "refdata.v1.tenor_units.put",
    put_many_tenor_units_request: "refdata.v1.tenor_units.put_many",
    delete_tenor_unit_request: "refdata.v1.tenor_units.delete",
    delete_many_tenor_units_request: "refdata.v1.tenor_units.delete_many",
    list_tenor_unit_versions_request: "refdata.v1.tenor_units_versions.list",
    get_tenor_unit_version_request: "refdata.v1.tenor_units_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_tenor_units_request: true,
    get_tenor_unit_request: true,
    get_many_tenor_units_request: true,
    put_tenor_unit_request: true,
    put_many_tenor_units_request: true,
    delete_tenor_unit_request: true,
    delete_many_tenor_units_request: true,
    list_tenor_unit_versions_request: true,
    get_tenor_unit_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.tenor_units_events.created",
    updated: "refdata.v1.tenor_units_events.updated",
    deleted: "refdata.v1.tenor_units_events.deleted",
} as const;
