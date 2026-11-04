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
import type { BookProductType } from '../domain/book_product_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface BookProductTypeKey {
    book_id: string;
    product_type_code: string;
}

export interface BookProductTypeWrite {
    book_id: string;
    product_type_code: string;
}

export interface BookProductTypeChange {
    write: BookProductTypeWrite;
    precondition: Precondition;
}

export interface BookProductTypeRemoval {
    key: BookProductTypeKey;
    precondition: Precondition;
}

export interface BookProductTypeLookup {
    key: BookProductTypeKey;
    book_product_type: BookProductType | null;
}

export interface BookProductTypesFilter {
    book_id: string | null;
    product_type_code: string | null;
    book_id_one_of: string[] | null;
    product_type_code_one_of: string[] | null;
}

export interface BookProductTypeEvent {
    event_id: string;
    key: BookProductTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListBookProductTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BookProductTypesFilter | null;
}

export interface ListBookProductTypesResponse {
    result: Result;
    book_product_types: BookProductType[];
    total: number;
}

export interface GetBookProductTypeRequest {
    key: BookProductTypeKey;
}

export interface GetBookProductTypeResponse {
    result: Result;
    book_product_type: BookProductType | null;
}

export interface GetManyBookProductTypesRequest {
    keys: BookProductTypeKey[];
}

export interface GetManyBookProductTypesResponse {
    result: Result;
    entries: BookProductTypeLookup[];
}

export interface PutBookProductTypeRequest {
    change: BookProductTypeChange;
    intent: ChangeIntent;
}

export interface PutBookProductTypeResponse {
    result: Result;
    book_product_type: BookProductType | null;
}

export interface PutManyBookProductTypesRequest {
    changes: BookProductTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyBookProductTypesResponse {
    result: Result;
    book_product_types: BookProductType[];
}

export interface DeleteBookProductTypeRequest {
    removal: BookProductTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteBookProductTypeResponse {
    result: Result;
}

export interface DeleteManyBookProductTypesRequest {
    removals: BookProductTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBookProductTypesResponse {
    result: Result;
}

export interface ListByBookIdBookProductTypesRequest {
    book_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: BookProductTypesFilter | null;
}

export interface ListByBookIdBookProductTypesResponse {
    result: Result;
    book_product_types: BookProductType[];
    total: number;
}

export interface ListByProductTypeCodeBookProductTypesRequest {
    product_type_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: BookProductTypesFilter | null;
}

export interface ListByProductTypeCodeBookProductTypesResponse {
    result: Result;
    book_product_types: BookProductType[];
    total: number;
}

export const subjects = {
    list_book_product_types_request: 'refdata.v1.book_product_types.list',
    get_book_product_type_request: 'refdata.v1.book_product_types.get',
    get_many_book_product_types_request: 'refdata.v1.book_product_types.get_many',
    put_book_product_type_request: 'refdata.v1.book_product_types.put',
    put_many_book_product_types_request: 'refdata.v1.book_product_types.put_many',
    delete_book_product_type_request: 'refdata.v1.book_product_types.delete',
    delete_many_book_product_types_request: 'refdata.v1.book_product_types.delete_many',
    list_by_book_id_book_product_types_request: 'refdata.v1.book_product_types.list_by_book_id',
    list_by_product_type_code_book_product_types_request:
        'refdata.v1.book_product_types.list_by_product_type_code',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_book_product_types_request: true,
    get_book_product_type_request: true,
    get_many_book_product_types_request: true,
    put_book_product_type_request: true,
    put_many_book_product_types_request: true,
    delete_book_product_type_request: true,
    delete_many_book_product_types_request: true,
    list_by_book_id_book_product_types_request: true,
    list_by_product_type_code_book_product_types_request: true,
} as const;
