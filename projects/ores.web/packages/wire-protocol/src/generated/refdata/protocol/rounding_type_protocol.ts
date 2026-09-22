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
import type { RoundingType } from '../domain/rounding_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RoundingTypeKey {
    code: string;
}

export interface RoundingTypeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface RoundingTypeChange {
    write: RoundingTypeWrite;
    precondition: Precondition;
}

export interface RoundingTypeRemoval {
    key: RoundingTypeKey;
    precondition: Precondition;
}

export interface RoundingTypeLookup {
    key: RoundingTypeKey;
    rounding_type: RoundingType | null;
}

export interface RoundingTypeEvent {
    event_id: string;
    key: RoundingTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface RoundingTypeVersionKey {
    rounding_type: RoundingTypeKey;
    version: number;
}

export interface RoundingTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRoundingTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListRoundingTypesResponse {
    result: Result;
    types: RoundingType[];
    total: number;
}

export interface GetRoundingTypeRequest {
    key: RoundingTypeKey;
}

export interface GetRoundingTypeResponse {
    result: Result;
    rounding_type: RoundingType | null;
}

export interface GetManyRoundingTypesRequest {
    keys: RoundingTypeKey[];
}

export interface GetManyRoundingTypesResponse {
    result: Result;
    entries: RoundingTypeLookup[];
}

export interface PutRoundingTypeRequest {
    change: RoundingTypeChange;
    intent: ChangeIntent;
}

export interface PutRoundingTypeResponse {
    result: Result;
    rounding_type: RoundingType;
}

export interface PutManyRoundingTypesRequest {
    changes: RoundingTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyRoundingTypesResponse {
    result: Result;
    types: RoundingType[];
}

export interface DeleteRoundingTypeRequest {
    removal: RoundingTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteRoundingTypeResponse {
    result: Result;
}

export interface DeleteManyRoundingTypesRequest {
    removals: RoundingTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRoundingTypesResponse {
    result: Result;
}

export interface ListRoundingTypeVersionsRequest {
    key: RoundingTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RoundingTypeVersionsFilter | null;
}

export interface ListRoundingTypeVersionsResponse {
    result: Result;
    versions: RoundingType[];
    total: number;
}

export interface GetRoundingTypeVersionRequest {
    key: RoundingTypeVersionKey;
}

export interface GetRoundingTypeVersionResponse {
    result: Result;
    version: RoundingType;
}

export const subjects = {
    list_rounding_types_request: "refdata.v1.rounding_types.list",
    get_rounding_type_request: "refdata.v1.rounding_types.get",
    get_many_rounding_types_request: "refdata.v1.rounding_types.get_many",
    put_rounding_type_request: "refdata.v1.rounding_types.put",
    put_many_rounding_types_request: "refdata.v1.rounding_types.put_many",
    delete_rounding_type_request: "refdata.v1.rounding_types.delete",
    delete_many_rounding_types_request: "refdata.v1.rounding_types.delete_many",
    list_rounding_type_versions_request: "refdata.v1.rounding_types_versions.list",
    get_rounding_type_version_request: "refdata.v1.rounding_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_rounding_types_request: true,
    get_rounding_type_request: true,
    get_many_rounding_types_request: true,
    put_rounding_type_request: true,
    put_many_rounding_types_request: true,
    delete_rounding_type_request: true,
    delete_many_rounding_types_request: true,
    list_rounding_type_versions_request: true,
    get_rounding_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.rounding_types_events.created",
    updated: "refdata.v1.rounding_types_events.updated",
    deleted: "refdata.v1.rounding_types_events.deleted",
} as const;
