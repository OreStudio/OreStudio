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
import type { ContactType } from '../domain/contact_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ContactTypeKey {
    code: string;
}

export interface ContactTypeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface ContactTypeChange {
    write: ContactTypeWrite;
    precondition: Precondition;
}

export interface ContactTypeRemoval {
    key: ContactTypeKey;
    precondition: Precondition;
}

export interface ContactTypeLookup {
    key: ContactTypeKey;
    contact_type: ContactType | null;
}

export interface ContactTypeEvent {
    event_id: string;
    key: ContactTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ContactTypeVersionKey {
    contact_type: ContactTypeKey;
    version: number;
}

export interface ContactTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListContactTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListContactTypesResponse {
    result: Result;
    types: ContactType[];
    total: number;
}

export interface GetContactTypeRequest {
    key: ContactTypeKey;
}

export interface GetContactTypeResponse {
    result: Result;
    contact_type: ContactType | null;
}

export interface GetManyContactTypesRequest {
    keys: ContactTypeKey[];
}

export interface GetManyContactTypesResponse {
    result: Result;
    entries: ContactTypeLookup[];
}

export interface PutContactTypeRequest {
    change: ContactTypeChange;
    intent: ChangeIntent;
}

export interface PutContactTypeResponse {
    result: Result;
    contact_type: ContactType;
}

export interface PutManyContactTypesRequest {
    changes: ContactTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyContactTypesResponse {
    result: Result;
    types: ContactType[];
}

export interface DeleteContactTypeRequest {
    removal: ContactTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteContactTypeResponse {
    result: Result;
}

export interface DeleteManyContactTypesRequest {
    removals: ContactTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyContactTypesResponse {
    result: Result;
}

export interface ListContactTypeVersionsRequest {
    key: ContactTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ContactTypeVersionsFilter | null;
}

export interface ListContactTypeVersionsResponse {
    result: Result;
    versions: ContactType[];
    total: number;
}

export interface GetContactTypeVersionRequest {
    key: ContactTypeVersionKey;
}

export interface GetContactTypeVersionResponse {
    result: Result;
    version: ContactType;
}

export const subjects = {
    list_contact_types_request: "refdata.v1.contact_types.list",
    get_contact_type_request: "refdata.v1.contact_types.get",
    get_many_contact_types_request: "refdata.v1.contact_types.get_many",
    put_contact_type_request: "refdata.v1.contact_types.put",
    put_many_contact_types_request: "refdata.v1.contact_types.put_many",
    delete_contact_type_request: "refdata.v1.contact_types.delete",
    delete_many_contact_types_request: "refdata.v1.contact_types.delete_many",
    list_contact_type_versions_request: "refdata.v1.contact_types_versions.list",
    get_contact_type_version_request: "refdata.v1.contact_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_contact_types_request: true,
    get_contact_type_request: true,
    get_many_contact_types_request: true,
    put_contact_type_request: true,
    put_many_contact_types_request: true,
    delete_contact_type_request: true,
    delete_many_contact_types_request: true,
    list_contact_type_versions_request: true,
    get_contact_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.contact_types_events.created",
    updated: "refdata.v1.contact_types_events.updated",
    deleted: "refdata.v1.contact_types_events.deleted",
} as const;
