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
import type { RegulatoryBookType } from '../domain/regulatory_book_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RegulatoryBookTypeKey {
    code: string;
}

export interface RegulatoryBookTypeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface RegulatoryBookTypeChange {
    write: RegulatoryBookTypeWrite;
    precondition: Precondition;
}

export interface RegulatoryBookTypeRemoval {
    key: RegulatoryBookTypeKey;
    precondition: Precondition;
}

export interface RegulatoryBookTypeLookup {
    key: RegulatoryBookTypeKey;
    regulatory_book_type: RegulatoryBookType | null;
}

export interface RegulatoryBookTypeEvent {
    event_id: string;
    key: RegulatoryBookTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface RegulatoryBookTypeVersionKey {
    regulatory_book_type: RegulatoryBookTypeKey;
    version: number;
}

export interface RegulatoryBookTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRegulatoryBookTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListRegulatoryBookTypesResponse {
    result: Result;
    types: RegulatoryBookType[];
    total: number;
}

export interface GetRegulatoryBookTypeRequest {
    key: RegulatoryBookTypeKey;
}

export interface GetRegulatoryBookTypeResponse {
    result: Result;
    regulatory_book_type: RegulatoryBookType | null;
}

export interface GetManyRegulatoryBookTypesRequest {
    keys: RegulatoryBookTypeKey[];
}

export interface GetManyRegulatoryBookTypesResponse {
    result: Result;
    entries: RegulatoryBookTypeLookup[];
}

export interface PutRegulatoryBookTypeRequest {
    change: RegulatoryBookTypeChange;
    intent: ChangeIntent;
}

export interface PutRegulatoryBookTypeResponse {
    result: Result;
    regulatory_book_type: RegulatoryBookType;
}

export interface PutManyRegulatoryBookTypesRequest {
    changes: RegulatoryBookTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyRegulatoryBookTypesResponse {
    result: Result;
    types: RegulatoryBookType[];
}

export interface DeleteRegulatoryBookTypeRequest {
    removal: RegulatoryBookTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteRegulatoryBookTypeResponse {
    result: Result;
}

export interface DeleteManyRegulatoryBookTypesRequest {
    removals: RegulatoryBookTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRegulatoryBookTypesResponse {
    result: Result;
}

export interface ListRegulatoryBookTypeVersionsRequest {
    key: RegulatoryBookTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RegulatoryBookTypeVersionsFilter | null;
}

export interface ListRegulatoryBookTypeVersionsResponse {
    result: Result;
    versions: RegulatoryBookType[];
    total: number;
}

export interface GetRegulatoryBookTypeVersionRequest {
    key: RegulatoryBookTypeVersionKey;
}

export interface GetRegulatoryBookTypeVersionResponse {
    result: Result;
    version: RegulatoryBookType;
}

export const subjects = {
    list_regulatory_book_types_request: "refdata.v1.regulatory_book_types.list",
    get_regulatory_book_type_request: "refdata.v1.regulatory_book_types.get",
    get_many_regulatory_book_types_request: "refdata.v1.regulatory_book_types.get_many",
    put_regulatory_book_type_request: "refdata.v1.regulatory_book_types.put",
    put_many_regulatory_book_types_request: "refdata.v1.regulatory_book_types.put_many",
    delete_regulatory_book_type_request: "refdata.v1.regulatory_book_types.delete",
    delete_many_regulatory_book_types_request: "refdata.v1.regulatory_book_types.delete_many",
    list_regulatory_book_type_versions_request: "refdata.v1.regulatory_book_types_versions.list",
    get_regulatory_book_type_version_request: "refdata.v1.regulatory_book_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_regulatory_book_types_request: true,
    get_regulatory_book_type_request: true,
    get_many_regulatory_book_types_request: true,
    put_regulatory_book_type_request: true,
    put_many_regulatory_book_types_request: true,
    delete_regulatory_book_type_request: true,
    delete_many_regulatory_book_types_request: true,
    list_regulatory_book_type_versions_request: true,
    get_regulatory_book_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.regulatory_book_types_events.created",
    updated: "refdata.v1.regulatory_book_types_events.updated",
    deleted: "refdata.v1.regulatory_book_types_events.deleted",
} as const;
