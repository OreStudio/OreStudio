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
import type { LegType } from '../domain/leg_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface LegTypeKey {
    code: string;
}

export interface LegTypeWrite {
    code: string;
    description: string;
}

export interface LegTypeChange {
    write: LegTypeWrite;
    precondition: Precondition;
}

export interface LegTypeRemoval {
    key: LegTypeKey;
    precondition: Precondition;
}

export interface LegTypeLookup {
    key: LegTypeKey;
    leg_type: LegType | null;
}

export interface LegTypeEvent {
    event_id: string;
    key: LegTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface LegTypeVersionKey {
    leg_type: LegTypeKey;
    version: number;
}

export interface LegTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListLegTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListLegTypesResponse {
    result: Result;
    types: LegType[];
    total: number;
}

export interface GetLegTypeRequest {
    key: LegTypeKey;
}

export interface GetLegTypeResponse {
    result: Result;
    leg_type: LegType | null;
}

export interface GetManyLegTypesRequest {
    keys: LegTypeKey[];
}

export interface GetManyLegTypesResponse {
    result: Result;
    entries: LegTypeLookup[];
}

export interface PutLegTypeRequest {
    change: LegTypeChange;
    intent: ChangeIntent;
}

export interface PutLegTypeResponse {
    result: Result;
    leg_type: LegType;
}

export interface PutManyLegTypesRequest {
    changes: LegTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyLegTypesResponse {
    result: Result;
    types: LegType[];
}

export interface DeleteLegTypeRequest {
    removal: LegTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteLegTypeResponse {
    result: Result;
}

export interface DeleteManyLegTypesRequest {
    removals: LegTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyLegTypesResponse {
    result: Result;
}

export interface ListLegTypeVersionsRequest {
    key: LegTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: LegTypeVersionsFilter | null;
}

export interface ListLegTypeVersionsResponse {
    result: Result;
    versions: LegType[];
    total: number;
}

export interface GetLegTypeVersionRequest {
    key: LegTypeVersionKey;
}

export interface GetLegTypeVersionResponse {
    result: Result;
    version: LegType;
}

export const subjects = {
    list_leg_types_request: "refdata.v1.leg_types.list",
    get_leg_type_request: "refdata.v1.leg_types.get",
    get_many_leg_types_request: "refdata.v1.leg_types.get_many",
    put_leg_type_request: "refdata.v1.leg_types.put",
    put_many_leg_types_request: "refdata.v1.leg_types.put_many",
    delete_leg_type_request: "refdata.v1.leg_types.delete",
    delete_many_leg_types_request: "refdata.v1.leg_types.delete_many",
    list_leg_type_versions_request: "refdata.v1.leg_types_versions.list",
    get_leg_type_version_request: "refdata.v1.leg_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_leg_types_request: true,
    get_leg_type_request: true,
    get_many_leg_types_request: true,
    put_leg_type_request: true,
    put_many_leg_types_request: true,
    delete_leg_type_request: true,
    delete_many_leg_types_request: true,
    list_leg_type_versions_request: true,
    get_leg_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.leg_types_events.created",
    updated: "refdata.v1.leg_types_events.updated",
    deleted: "refdata.v1.leg_types_events.deleted",
} as const;
