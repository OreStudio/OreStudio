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
import type { PurposeType } from '../domain/purpose_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PurposeTypeKey {
    code: string;
}

export interface PurposeTypeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface PurposeTypeChange {
    write: PurposeTypeWrite;
    precondition: Precondition;
}

export interface PurposeTypeRemoval {
    key: PurposeTypeKey;
    precondition: Precondition;
}

export interface PurposeTypeLookup {
    key: PurposeTypeKey;
    purpose_type: PurposeType | null;
}

export interface PurposeTypeEvent {
    event_id: string;
    key: PurposeTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PurposeTypeVersionKey {
    purpose_type: PurposeTypeKey;
    version: number;
}

export interface PurposeTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPurposeTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListPurposeTypesResponse {
    result: Result;
    types: PurposeType[];
    total: number;
}

export interface GetPurposeTypeRequest {
    key: PurposeTypeKey;
}

export interface GetPurposeTypeResponse {
    result: Result;
    purpose_type: PurposeType | null;
}

export interface GetManyPurposeTypesRequest {
    keys: PurposeTypeKey[];
}

export interface GetManyPurposeTypesResponse {
    result: Result;
    entries: PurposeTypeLookup[];
}

export interface PutPurposeTypeRequest {
    change: PurposeTypeChange;
    intent: ChangeIntent;
}

export interface PutPurposeTypeResponse {
    result: Result;
    purpose_type: PurposeType;
}

export interface PutManyPurposeTypesRequest {
    changes: PurposeTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyPurposeTypesResponse {
    result: Result;
    types: PurposeType[];
}

export interface DeletePurposeTypeRequest {
    removal: PurposeTypeRemoval;
    intent: ChangeIntent;
}

export interface DeletePurposeTypeResponse {
    result: Result;
}

export interface DeleteManyPurposeTypesRequest {
    removals: PurposeTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPurposeTypesResponse {
    result: Result;
}

export interface ListPurposeTypeVersionsRequest {
    key: PurposeTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PurposeTypeVersionsFilter | null;
}

export interface ListPurposeTypeVersionsResponse {
    result: Result;
    versions: PurposeType[];
    total: number;
}

export interface GetPurposeTypeVersionRequest {
    key: PurposeTypeVersionKey;
}

export interface GetPurposeTypeVersionResponse {
    result: Result;
    version: PurposeType;
}

export const subjects = {
    list_purpose_types_request: "refdata.v1.purpose_types.list",
    get_purpose_type_request: "refdata.v1.purpose_types.get",
    get_many_purpose_types_request: "refdata.v1.purpose_types.get_many",
    put_purpose_type_request: "refdata.v1.purpose_types.put",
    put_many_purpose_types_request: "refdata.v1.purpose_types.put_many",
    delete_purpose_type_request: "refdata.v1.purpose_types.delete",
    delete_many_purpose_types_request: "refdata.v1.purpose_types.delete_many",
    list_purpose_type_versions_request: "refdata.v1.purpose_types_versions.list",
    get_purpose_type_version_request: "refdata.v1.purpose_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_purpose_types_request: true,
    get_purpose_type_request: true,
    get_many_purpose_types_request: true,
    put_purpose_type_request: true,
    put_many_purpose_types_request: true,
    delete_purpose_type_request: true,
    delete_many_purpose_types_request: true,
    list_purpose_type_versions_request: true,
    get_purpose_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.purpose_types_events.created",
    updated: "refdata.v1.purpose_types_events.updated",
    deleted: "refdata.v1.purpose_types_events.deleted",
} as const;
