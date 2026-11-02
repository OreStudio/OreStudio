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
import type { ProductType } from '../domain/product_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ProductTypeKey {
    code: string;
}

export interface ProductTypeWrite {
    code: string;
    name: string;
    description: string;
    source: string;
    source_ref: string;
    product_family_code: string;
    construction: string;
    has_options: boolean;
    has_extension: boolean;
    instrument_table: string;
    display_order: number;
    knowledge_note: string;
}

export interface ProductTypeChange {
    write: ProductTypeWrite;
    precondition: Precondition;
}

export interface ProductTypeRemoval {
    key: ProductTypeKey;
    precondition: Precondition;
}

export interface ProductTypeLookup {
    key: ProductTypeKey;
    product_type: string | null;
}

export interface ProductTypesFilter {
    code_one_of: string[] | null;
}

export interface ProductTypeEvent {
    event_id: string;
    key: ProductTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ProductTypeVersionKey {
    product_type: ProductTypeKey;
    version: number;
}

export interface ProductTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListProductTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ProductTypesFilter | null;
    as_of: string | null;
}

export interface ListProductTypesResponse {
    result: Result;
    types: string[];
    total: number;
}

export interface GetProductTypeRequest {
    key: ProductTypeKey;
}

export interface GetProductTypeResponse {
    result: Result;
    product_type: string | null;
}

export interface GetManyProductTypesRequest {
    keys: ProductTypeKey[];
}

export interface GetManyProductTypesResponse {
    result: Result;
    entries: ProductTypeLookup[];
}

export interface PutProductTypeRequest {
    change: ProductTypeChange;
    intent: ChangeIntent;
}

export interface PutProductTypeResponse {
    result: Result;
    product_type: string | null;
}

export interface PutManyProductTypesRequest {
    changes: ProductTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyProductTypesResponse {
    result: Result;
    types: string[];
}

export interface DeleteProductTypeRequest {
    removal: ProductTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteProductTypeResponse {
    result: Result;
}

export interface DeleteManyProductTypesRequest {
    removals: ProductTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyProductTypesResponse {
    result: Result;
}

export interface ListProductTypeVersionsRequest {
    key: ProductTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ProductTypeVersionsFilter | null;
}

export interface ListProductTypeVersionsResponse {
    result: Result;
    versions: string[];
    total: number;
}

export interface GetProductTypeVersionRequest {
    key: ProductTypeVersionKey;
}

export interface GetProductTypeVersionResponse {
    result: Result;
    version: string | null;
}

export const subjects = {
    list_product_types_request: 'trading.v1.product_types.list',
    get_product_type_request: 'trading.v1.product_types.get',
    get_many_product_types_request: 'trading.v1.product_types.get_many',
    put_product_type_request: 'trading.v1.product_types.put',
    put_many_product_types_request: 'trading.v1.product_types.put_many',
    delete_product_type_request: 'trading.v1.product_types.delete',
    delete_many_product_types_request: 'trading.v1.product_types.delete_many',
    list_product_type_versions_request: 'trading.v1.product_types_versions.list',
    get_product_type_version_request: 'trading.v1.product_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_product_types_request: true,
    get_product_type_request: true,
    get_many_product_types_request: true,
    put_product_type_request: true,
    put_many_product_types_request: true,
    delete_product_type_request: true,
    delete_many_product_types_request: true,
    list_product_type_versions_request: true,
    get_product_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.product_types_events.created',
    updated: 'trading.v1.product_types_events.updated',
    deleted: 'trading.v1.product_types_events.deleted',
} as const;
