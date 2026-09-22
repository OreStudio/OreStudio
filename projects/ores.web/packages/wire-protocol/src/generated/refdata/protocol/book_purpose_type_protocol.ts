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
import type { BookPurposeType } from '../domain/book_purpose_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BookPurposeTypeKey {
    code: string;
}

export interface BookPurposeTypeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface BookPurposeTypeChange {
    write: BookPurposeTypeWrite;
    precondition: Precondition;
}

export interface BookPurposeTypeRemoval {
    key: BookPurposeTypeKey;
    precondition: Precondition;
}

export interface BookPurposeTypeLookup {
    key: BookPurposeTypeKey;
    book_purpose_type: BookPurposeType | null;
}

export interface BookPurposeTypeEvent {
    event_id: string;
    key: BookPurposeTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BookPurposeTypeVersionKey {
    book_purpose_type: BookPurposeTypeKey;
    version: number;
}

export interface BookPurposeTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBookPurposeTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBookPurposeTypesResponse {
    result: Result;
    types: BookPurposeType[];
    total: number;
}

export interface GetBookPurposeTypeRequest {
    key: BookPurposeTypeKey;
}

export interface GetBookPurposeTypeResponse {
    result: Result;
    book_purpose_type: BookPurposeType | null;
}

export interface GetManyBookPurposeTypesRequest {
    keys: BookPurposeTypeKey[];
}

export interface GetManyBookPurposeTypesResponse {
    result: Result;
    entries: BookPurposeTypeLookup[];
}

export interface PutBookPurposeTypeRequest {
    change: BookPurposeTypeChange;
    intent: ChangeIntent;
}

export interface PutBookPurposeTypeResponse {
    result: Result;
    book_purpose_type: BookPurposeType;
}

export interface PutManyBookPurposeTypesRequest {
    changes: BookPurposeTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyBookPurposeTypesResponse {
    result: Result;
    types: BookPurposeType[];
}

export interface DeleteBookPurposeTypeRequest {
    removal: BookPurposeTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteBookPurposeTypeResponse {
    result: Result;
}

export interface DeleteManyBookPurposeTypesRequest {
    removals: BookPurposeTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBookPurposeTypesResponse {
    result: Result;
}

export interface ListBookPurposeTypeVersionsRequest {
    key: BookPurposeTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BookPurposeTypeVersionsFilter | null;
}

export interface ListBookPurposeTypeVersionsResponse {
    result: Result;
    versions: BookPurposeType[];
    total: number;
}

export interface GetBookPurposeTypeVersionRequest {
    key: BookPurposeTypeVersionKey;
}

export interface GetBookPurposeTypeVersionResponse {
    result: Result;
    version: BookPurposeType;
}

export const subjects = {
    list_book_purpose_types_request: "refdata.v1.book_purpose_types.list",
    get_book_purpose_type_request: "refdata.v1.book_purpose_types.get",
    get_many_book_purpose_types_request: "refdata.v1.book_purpose_types.get_many",
    put_book_purpose_type_request: "refdata.v1.book_purpose_types.put",
    put_many_book_purpose_types_request: "refdata.v1.book_purpose_types.put_many",
    delete_book_purpose_type_request: "refdata.v1.book_purpose_types.delete",
    delete_many_book_purpose_types_request: "refdata.v1.book_purpose_types.delete_many",
    list_book_purpose_type_versions_request: "refdata.v1.book_purpose_types_versions.list",
    get_book_purpose_type_version_request: "refdata.v1.book_purpose_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_book_purpose_types_request: true,
    get_book_purpose_type_request: true,
    get_many_book_purpose_types_request: true,
    put_book_purpose_type_request: true,
    put_many_book_purpose_types_request: true,
    delete_book_purpose_type_request: true,
    delete_many_book_purpose_types_request: true,
    list_book_purpose_type_versions_request: true,
    get_book_purpose_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.book_purpose_types_events.created",
    updated: "refdata.v1.book_purpose_types_events.updated",
    deleted: "refdata.v1.book_purpose_types_events.deleted",
} as const;
