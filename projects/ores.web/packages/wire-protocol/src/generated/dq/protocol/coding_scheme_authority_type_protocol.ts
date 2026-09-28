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
import type { CodingSchemeAuthorityType } from '../domain/coding_scheme_authority_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CodingSchemeAuthorityTypeKey {
    code: string;
}

export interface CodingSchemeAuthorityTypeWrite {
    code: string;
    name: string;
    description: string;
}

export interface CodingSchemeAuthorityTypeChange {
    write: CodingSchemeAuthorityTypeWrite;
    precondition: Precondition;
}

export interface CodingSchemeAuthorityTypeRemoval {
    key: CodingSchemeAuthorityTypeKey;
    precondition: Precondition;
}

export interface CodingSchemeAuthorityTypeLookup {
    key: CodingSchemeAuthorityTypeKey;
    coding_scheme_authority_type: CodingSchemeAuthorityType | null;
}

export interface CodingSchemeAuthorityTypeEvent {
    event_id: string;
    key: CodingSchemeAuthorityTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CodingSchemeAuthorityTypeVersionKey {
    coding_scheme_authority_type: CodingSchemeAuthorityTypeKey;
    version: number;
}

export interface CodingSchemeAuthorityTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCodingSchemeAuthorityTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCodingSchemeAuthorityTypesResponse {
    result: Result;
    authority_types: CodingSchemeAuthorityType[];
    total: number;
}

export interface GetCodingSchemeAuthorityTypeRequest {
    key: CodingSchemeAuthorityTypeKey;
}

export interface GetCodingSchemeAuthorityTypeResponse {
    result: Result;
    coding_scheme_authority_type: CodingSchemeAuthorityType | null;
}

export interface GetManyCodingSchemeAuthorityTypesRequest {
    keys: CodingSchemeAuthorityTypeKey[];
}

export interface GetManyCodingSchemeAuthorityTypesResponse {
    result: Result;
    entries: CodingSchemeAuthorityTypeLookup[];
}

export interface PutCodingSchemeAuthorityTypeRequest {
    change: CodingSchemeAuthorityTypeChange;
    intent: ChangeIntent;
}

export interface PutCodingSchemeAuthorityTypeResponse {
    result: Result;
    coding_scheme_authority_type: CodingSchemeAuthorityType | null;
}

export interface PutManyCodingSchemeAuthorityTypesRequest {
    changes: CodingSchemeAuthorityTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyCodingSchemeAuthorityTypesResponse {
    result: Result;
    authority_types: CodingSchemeAuthorityType[];
}

export interface DeleteCodingSchemeAuthorityTypeRequest {
    removal: CodingSchemeAuthorityTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteCodingSchemeAuthorityTypeResponse {
    result: Result;
}

export interface DeleteManyCodingSchemeAuthorityTypesRequest {
    removals: CodingSchemeAuthorityTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCodingSchemeAuthorityTypesResponse {
    result: Result;
}

export interface ListCodingSchemeAuthorityTypeVersionsRequest {
    key: CodingSchemeAuthorityTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CodingSchemeAuthorityTypeVersionsFilter | null;
}

export interface ListCodingSchemeAuthorityTypeVersionsResponse {
    result: Result;
    versions: CodingSchemeAuthorityType[];
    total: number;
}

export interface GetCodingSchemeAuthorityTypeVersionRequest {
    key: CodingSchemeAuthorityTypeVersionKey;
}

export interface GetCodingSchemeAuthorityTypeVersionResponse {
    result: Result;
    version: CodingSchemeAuthorityType | null;
}

export const subjects = {
    list_coding_scheme_authority_types_request: 'dq.v1.coding_scheme_authority_types.list',
    get_coding_scheme_authority_type_request: 'dq.v1.coding_scheme_authority_types.get',
    get_many_coding_scheme_authority_types_request: 'dq.v1.coding_scheme_authority_types.get_many',
    put_coding_scheme_authority_type_request: 'dq.v1.coding_scheme_authority_types.put',
    put_many_coding_scheme_authority_types_request: 'dq.v1.coding_scheme_authority_types.put_many',
    delete_coding_scheme_authority_type_request: 'dq.v1.coding_scheme_authority_types.delete',
    delete_many_coding_scheme_authority_types_request:
        'dq.v1.coding_scheme_authority_types.delete_many',
    list_coding_scheme_authority_type_versions_request:
        'dq.v1.coding_scheme_authority_types_versions.list',
    get_coding_scheme_authority_type_version_request:
        'dq.v1.coding_scheme_authority_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_coding_scheme_authority_types_request: true,
    get_coding_scheme_authority_type_request: true,
    get_many_coding_scheme_authority_types_request: true,
    put_coding_scheme_authority_type_request: true,
    put_many_coding_scheme_authority_types_request: true,
    delete_coding_scheme_authority_type_request: true,
    delete_many_coding_scheme_authority_types_request: true,
    list_coding_scheme_authority_type_versions_request: true,
    get_coding_scheme_authority_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'dq.v1.coding_scheme_authority_types_events.created',
    updated: 'dq.v1.coding_scheme_authority_types_events.updated',
    deleted: 'dq.v1.coding_scheme_authority_types_events.deleted',
} as const;
